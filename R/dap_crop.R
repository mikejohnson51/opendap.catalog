#' @title Crop DAP file
#' @description Crop an OpenDAP resource file to a given AOI and time bound
#' @param URL local file path or dodC URL
#' @param catolog subset of open.dap catolog
#' @param AOI sf object
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate  end date (YYYY- MM-DD)
#' @param varname  name of variable to extract. If NULL, then get all
#' @param verbose  Should dap_summary be printed?
#' @details if AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @export
#' @importFrom terra vect intersect ext project

dap_crop <- function(URL = NULL,
                     catolog = NULL,
                     AOI = NULL,
                     startDate = NULL,
                     endDate = NULL,
                     varname = NULL,
                     verbose = TRUE) {

  if (!is.null(URL)) {
    catolog <- read_dap_file(URL, id = "local")
    catolog$tiled <- ""
  }

  ## TIME
  if (is.null(startDate) & is.null(endDate)) {
    catolog$T <- paste0("[0:1:", catolog$nT - 1, "]")
    catolog$Tdim <- catolog$nT
    tmp <- do.call(rbind, strsplit(catolog$duration, "/"))
    catolog <- cbind(catolog, data.frame(startDate = tmp[, 1], endDate = tmp[, 2]))
  } else {
    if (is.null(endDate)) {
      endDate <- startDate
    }

    if (grepl("hour", catolog$interval[1])) {
      startDate <- paste(startDate, "00:00:00")
      endDate <- paste(endDate, "23:00:00")
    }

    startDate <- as.POSIXct(startDate, tz = "UTC")
    endDate <- as.POSIXct(endDate, tz = "UTC")

    out <- list()

    for (i in 1:nrow(catolog)) {
      time_steps <- parse_date(
        duration = catolog$duration[i],
        interval = catolog$interval[i]
      )


      if (startDate >= max(time_steps)) {
        out[[i]] <- NULL
      } else {
        T1 <- which.min(abs(time_steps - startDate)) - 1
        Tn <- which.min(abs(time_steps - endDate)) - 1

        out[[i]] <- cbind(catolog[i, ], data.frame(
          T = paste0("[", T1, ":1:", Tn, "]"),
          Tdim = (Tn - T1) + 1,
          startDate = time_steps[T1 + 1],
          endDate = time_steps[Tn + 1]
        ))
      }
    }

    dur <- catolog$duration
    catolog <- do.call(rbind, out)

    if (length(catolog) == 0) {
      stop("Requested Time not found in ", unique(dur), call. = FALSE)
    }
  }

  ## SPACE (XY)
  if (is.null(AOI)) {
    catolog$X <- paste0("[0:1:", catolog$ncols - 1, "]")
    catolog$Y <- paste0("[0:1:", catolog$nrows - 1, "]")
  } else {

    AOIspat <- terra::vect(AOI)

    if (catolog$id[1] != "local") {
      catolog = spatial_intersect(x = catolog, AOI, merge = TRUE)
    }

    out <- lapply(1:nrow(catolog), function(i) {
      tryCatch(
        {
          intersect(make_ext(catolog[i, ]), ext(project(AOIspat, catolog$proj[i])))
        },
        error = function(e) {
          NULL
        }
      )
    })

    drops <- which(sapply(out, is.null))

    if (length(drops) != 0) {
      catolog <- catolog[-drops, ]
      out <- catolog[-drops]
    }

    if (nrow(catolog) < 1) {
      stop("No resources intersect with provided AOI", call. = FALSE)
    }

    for (i in 1:nrow(catolog)) {
      X_coords <- seq(catolog$X1[i], catolog$Xn[i], length.out = catolog$ncols[i])
      Y_coords <- seq(catolog$Y1[i], catolog$Yn[i], length.out = catolog$nrow[i])

      ys <- c(which.min(abs(Y_coords - out[[i]]$ymin)), which.min(abs(Y_coords - out[[i]]$ymax))) - 1
      xs <- c(which.min(abs(X_coords - out[[i]]$xmin)), which.min(abs(X_coords - out[[i]]$xmax))) - 1

      catolog$Y[i] <- paste0("[", paste(sort(ys), collapse = ":1:"), "]")
      catolog$X[i] <- paste0("[", paste(sort(xs), collapse = ":1:"), "]")
      catolog$X1[i] <- min(X_coords[xs + 1])
      catolog$Xn[i] <- max(X_coords[xs + 1])
      catolog$Y1[i] <- min(Y_coords[ys + 1])
      catolog$Yn[i] <- max(Y_coords[ys + 1])
      catolog$ncols[i] <- abs(diff(xs)) + 1
      catolog$nrows[i] <- abs(diff(ys)) + 1
    }
  }


  if (any(grepl("XY", catolog$tiled))) {
    catolog$URL <- paste0(catolog$URL, "/", catolog$tile, ".ncml?", catolog$varname, catolog$T, catolog$Y, catolog$X)
  } else {
    catolog$URL <- paste0(catolog$URL, "?", catolog$varname, catolog$T, catolog$Y, catolog$X)
  }

  if (!is.null(varname)) {
    if (!varname %in% catolog$varname) {
      stop(
        "variable in resource include:\n",
        paste(catolog$varname, collapse = ", ")
      )
    }

    catolog <- catolog[catolog$varname %in% varname, ]
  }

  catolog$X <- NULL
  catolog$Y <- NULL
  catolog$T <- NULL

  if (verbose) {
    dap_summary(catolog)
  }

  catolog
}
