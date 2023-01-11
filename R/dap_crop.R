#' @title Crop DAP file
#' @description Crop an OpenDAP resource file to a given AOI and time bound
#' @param URL local file path or dodC URL
#' @param catalog subset of open.dap catalog
#' @param AOI sf object
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate  end date (YYYY- MM-DD)
#' @param varname  name of variable to extract. If NULL, then get all
#' @param verbose  Should dap_summary be printed?
#' @details if AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @export
#' @importFrom terra vect intersect ext project
#' @importFrom glue glue

dap_crop <- function(URL = NULL,
                     catalog = NULL,
                     AOI = NULL,
                     startDate = NULL,
                     endDate = NULL,
                     varname = NULL,
                     verbose = TRUE) {

  if (!is.null(URL)) {
    catalog <- read_dap_file(URL, varname = varname, id = "local")
    catalog$tiled <- ""
  }

  ## TIME
  if (is.null(startDate) & is.null(endDate)) {
    catalog$T <- paste0("[0:1:", catalog$nT - 1, "]")
    catalog$Tdim <- catalog$nT
    tmp <- do.call(rbind, strsplit(catalog$duration, "/"))
    catalog <- cbind(catalog, data.frame(startDate = tmp[, 1], endDate = tmp[, 2]))
  } else {
    if (is.null(endDate)) {
      endDate <- startDate
    }

    if (grepl("hour", catalog$interval[1])) {
      startDate <- paste(startDate, "00:00:00")
      endDate <- paste(endDate, "23:00:00")
    }

    startDate <- as.POSIXct(startDate, tz = "UTC")
    endDate <- as.POSIXct(endDate, tz = "UTC")

    out <- list()

    for (i in 1:nrow(catalog)) {
      time_steps <- parse_date(
        duration = catalog$duration[i],
        interval = catalog$interval[i]
      )


      if (startDate >= max(time_steps)) {
        out[[i]] <- NULL
      } else {
        T1 <- which.min(abs(time_steps - startDate)) - 1
        Tn <- which.min(abs(time_steps - endDate)) - 1

        out[[i]] <- cbind(catalog[i, ], data.frame(
          T = paste0("[", T1, ":1:", Tn, "]"),
          Tdim = (Tn - T1) + 1,
          startDate = time_steps[T1 + 1],
          endDate = time_steps[Tn + 1]
        ))
      }
    }

    dur <- catalog$duration
    catalog <- do.call(rbind, out)

    if (length(catalog) == 0) {
      stop("Requested Time not found in ", unique(dur), call. = FALSE)
    }
  }

  ## SPACE (XY)
  if (is.null(AOI)) {
    catalog$X <- paste0("[0:1:", catalog$ncols - 1, "]")
    catalog$Y <- paste0("[0:1:", catalog$nrows - 1, "]")
  } else {

    AOIspat <- terra::vect(AOI)

    if (catalog$id[1] != "local") {
      catalog = spatial_intersect(x = catalog, sf::st_union(AOI), merge = TRUE)
    }

    out <- lapply(1:nrow(catalog), function(i) {
      tryCatch(
        {
          terra::intersect(ext(project(AOIspat, catalog$proj[i])), make_ext(catalog[i, ]))
        },
        error = function(e) {
          NULL
        }
      )
    })

    drops <- which(sapply(out, is.null))

    if (length(drops) != 0) {
      catalog <- catalog[-drops, ]
      out <- catalog[-drops]
    }

    if (nrow(catalog) < 1) {
      stop("No resources intersect with provided AOI", call. = FALSE)
    }

    for (i in 1:nrow(catalog)) {
      X_coords <- seq(catalog$X1[i], catalog$Xn[i], length.out = catalog$ncols[i])
      Y_coords <- seq(catalog$Y1[i], catalog$Yn[i], length.out = catalog$nrow[i])

      ys <- c(which.min(abs(Y_coords - out[[i]]$ymin)), which.min(abs(Y_coords - out[[i]]$ymax))) - 1
      xs <- c(which.min(abs(X_coords - out[[i]]$xmin)), which.min(abs(X_coords - out[[i]]$xmax))) - 1

      catalog$Y[i] <- paste0("[", paste(sort(ys), collapse = ":1:"), "]")
      catalog$X[i] <- paste0("[", paste(sort(xs), collapse = ":1:"), "]")
      catalog$X1[i] <- min(X_coords[xs + 1])
      catalog$Xn[i] <- max(X_coords[xs + 1])
      catalog$Y1[i] <- min(Y_coords[ys + 1])
      catalog$Yn[i] <- max(Y_coords[ys + 1])
      catalog$ncols[i] <- abs(diff(xs)) + 1
      catalog$nrows[i] <- abs(diff(ys)) + 1
    }
  }

  first = substr(catalog$dim_order,  1,1)
  second = substr(catalog$dim_order, 2,2)
  third = substr(catalog$dim_order,  3,3)

  first = ifelse(length(first) == 0, "T", first)
  second = ifelse(length(second) == 0, "Y", second)
  third = ifelse(length(third) == 0, "X", third)

  if (any(grepl("XY", catalog$tiled))) {
    catalog$URL <-glue("{catalog$URL}/{catalog$tile}.ncml?{catalog$varname}{catalog[[first]]}{catalog[[second]]}{catalog[[third]]}")
  } else {
    catalog$URL <-glue("{catalog$URL}?{catalog$varname}{catalog[[first]]}{catalog[[second]]}{catalog[[third]]}")
  }

  if (!is.null(varname)) {
    if (!varname %in% catalog$varname) {
      stop(
        "variable in resource include:\n",
        paste(catalog$varname, collapse = ", ")
      )
    }

    catalog <- catalog[catalog$varname %in% varname, ]
  }

  catalog$X <- NULL
  catalog$Y <- NULL
  catalog$T <- NULL

  if (verbose) {
    dap_summary(catalog)
  }

  catalog
}
