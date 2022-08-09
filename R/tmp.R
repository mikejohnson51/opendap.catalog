library(sf)
library(terra)
library(raster)
library(AOI)
library(dplyr)
library(ggplot2)
library(mapview)
library(opendap.catalog)

# NULL URL
URL     <- NULL

# Retrieve catalog through spatial search
cat_search <- search(
  AOI   = AOI::aoi_get(state = "Hawaii"),
  query = "monthly precipitation"
)

# catalogs to use DAP
catalog <- cat_search[85:86,]


# AOI input
AOI       <- AOI::aoi_get(state = "Hawaii")

# start date
startDate <- "2012-01-01"

# end date
endDate   <- "2012-03-01"

varname = NULL
verbose = TRUE
dc <- dap_crop(URL = NULL,
                     catalog = catalog,
                     AOI = AOI,
                     startDate = startDate,
                     endDate = endDate,
                     varname = NULL,
                     verbose = TRUE)

dap <- dc
# dap <- dap[1, ]
library(future.apply)
# *******************
# ---- dap_get() ----
# *******************
if (!is.null(varname)) {
  if (!varname %in% dap$varname) {
    stop("variable in resource include:\n", paste(dap$varname, collapse = ", "))
  }

  dap <- dap[dap$varname %in% varname, ]
}

out <- future_lapply(1:nrow(dap), FUN = function(x) {
  go_get_dap_data(dap[x, ])
})

names(out) <- sub("_$", "", paste0(dap$varname, "_", dap$scenario))

if (any(grepl("XY", dap$tiled))) {
  u <- unique(unlist(lapply(out, units)))
  if (length(u) == 1) {
    out <- suppressWarnings({ merge(sprc(out)) })
    terra::units(out) <- rep(u, nlyr(out))
    out
  } else {
    out
  }
} else if (any(dap$tiled == "T")) {

  ll = list()
  g = expand.grid(v = unique(dap$varname), s = unique(dap$scenario))

  for(v in unique(g$v)){
    g_tmp = g[g$v == v, ]
    ind = grepl(v, names(out))
    tmp = out[ind]
    n = unlist(lapply(1:length(tmp), function(x) { paste0(names(tmp[[x]]), "_", g_tmp$s[x]) }))
    o = order(n)
    tmp = rast(tmp)
    tmp = tmp[[o]]
    names(tmp) = n
    ll[[v]] = tmp
  }

  ll
} else {
  out
}
dg <- dap_get(dc)
# dap_crop <- function(URL = NULL,
#                      catalog = NULL,
#                      AOI = NULL,
#                      startDate = NULL,
#                      endDate = NULL,
#                      varname = NULL,
#                      verbose = TRUE) {

  if (!is.null(URL)) {
    logger::log_info("URL is not NULL")
    catalog <- read_dap_file(URL, id = "local")
    catalog$tiled <- ""
  }

  ## TIME
  if (is.null(startDate) & is.null(endDate)) {
    logger::log_info("Both dates are NULL")
    catalog$T <- paste0("[0:1:", catalog$nT - 1, "]")
    catalog$Tdim <- catalog$nT
    tmp <- do.call(rbind, strsplit(catalog$duration, "/"))
    catalog <- cbind(catalog, data.frame(startDate = tmp[, 1], endDate = tmp[, 2]))
  } else {
    if (is.null(endDate)) {

      logger::log_info("endDate is NULL")

      endDate <- startDate
    }

    if (grepl("hour", catalog$interval[1])) {

      logger::log_info("hour found in catalog interval")

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
      logger::log_info("\n\noutloop -- {i} in {nrow(catalog)}")

      if (startDate >= max(time_steps)) {
        logger::log_info("start >= max timesteps")
        out[[i]] <- NULL
      } else {
        logger::log_info("start not >= max timesteps")
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
    logger::log_info("do call on catalog")
    dur <- catalog$duration
    catalog <- do.call(rbind, out)

    if (length(catalog) == 0) {
      stop("Requested Time not found in ", unique(dur), call. = FALSE)
    }
  }

  ## SPACE (XY)
  if (is.null(AOI)) {
    logger::log_info("AOI is NULL")
    catalog$X <- paste0("[0:1:", catalog$ncols - 1, "]")
    catalog$Y <- paste0("[0:1:", catalog$nrows - 1, "]")
  } else {
    logger::log_info("AOI NOT NULL")
    AOIspat <- terra::vect(AOI)

    if (catalog$id[1] != "local") {
      logger::log_info("do spatial_intersect")
      catalog = spatial_intersect(x = catalog, AOI, merge = TRUE)
    }

    out <- lapply(1:nrow(catalog), function(i) {
      tryCatch(
        {
          logger::log_info("out lapply - {i}")
          intersect(make_ext(catalog[i, ]), ext(project(AOIspat, catalog$proj[i])))
        },
        error = function(e) {
          NULL
        }
      )
    })

    drops <- which(sapply(out, is.null))

    if (length(drops) != 0) {
      logger::log_info("drops != 0")
      catalog <- catalog[-drops, ]
      out <- catalog[-drops]
    }

    if (nrow(catalog) < 1) {
      stop("No resources intersect with provided AOI", call. = FALSE)
    }

    for (i in 1:nrow(catalog)) {
      logger::log_info("{i} in {nrow(catalog)}")
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


  if (any(grepl("XY", catalog$tiled))) {
    catalog$URL <- paste0(catalog$URL, "/", catalog$tile, ".ncml?", catalog$varname, catalog$T, catalog$Y, catalog$X)
  } else {
    catalog$URL <- paste0(catalog$URL, "?", catalog$varname, catalog$T, catalog$Y, catalog$X)
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
# }
dap <- catalog
dap2 <- dap[2, ]
dg <- dap_get(dap)
dap2 <- dc[1,]
nc <- RNetCDF::open.nc(paste0(dap2$URL, "#fillmismatch"))
on.exit(close.nc(nc))
as.vector(RNetCDF::var.get.nc(nc, dap$varname, unpack = TRUE))
var <- nc
vtt <- var_to_terra(nc, dap2)

resx <- (dap2$Xn - dap2$X1) / (dap2$ncols - 1)
resy <- (dap2$Yn - dap2$Y1) / (dap2$nrows - 1)

xmin <- dap2$X1 - 0.5 * resx
xmax <- dap2$Xn + 0.5 * resx
ymin <- dap2$Y1 - 0.5 * resy
ymax <- dap2$Yn + 0.5 * resy

r <- terra::rast(
  xmin  = min(xmin, xmax),
  xmax = max(xmax, xmax),
  ymin  = min(ymin, ymax),
  ymax  = max(ymin, ymax),
  nrows = dap2$nrows,
  ncols = dap2$ncols,
  nlyrs = dap2$Tdim,
  crs = dap2$proj
)

if (length(dim(var)) == 2) {
  dim(var) <- c(dim(var), 1)
}

r[] <- var

if (dap2$toptobottom) {
  r <- terra::flip(r)
}

terra::units(r) <- dap2$units

names(r) <- seq.POSIXt(as.POSIXct(dap2$startDate),
                       as.POSIXct(dap2$endDate),
                       length.out  = dap2$Tdim
)

r
tryCatch(
  {
    if (grepl("http", dap$URL)) {
      var_to_terra(nc, dap2)
    } else {
      var_to_terra(dap_to_local(dap2), dap2)
    }
  },
  error = function(e) {
    dap2$URL
  }
)

if (!is.null(varname)) {
  if (!varname %in% dap$varname) {
    stop("variable in resource include:\n", paste(dap$varname, collapse = ", "))
  }

  dap <- dap[dap$varname %in% varname, ]
}

out <- future_lapply(1:nrow(dap), FUN = function(x) {
  go_get_dap_data(dap[x, ])
})

names(out) <- sub("_$", "", paste0(dap$varname, "_", dap$scenario))

if (any(grepl("XY", dap$tiled))) {
  u <- unique(unlist(lapply(out, units)))
  if (length(u) == 1) {
    out <- suppressWarnings({ merge(sprc(out)) })
    terra::units(out) <- rep(u, nlyr(out))
    out
  } else {
    out
  }
} else if (any(dap$tiled == "T")) {

  ll = list()
  g = expand.grid(v = unique(dap$varname), s = unique(dap$scenario))

  for(v in unique(g$v)){
    g_tmp = g[g$v == v, ]
    ind = grepl(v, names(out))
    tmp = out[ind]
    n = unlist(lapply(1:length(tmp), function(x) { paste0(names(tmp[[x]]), "_", g_tmp$s[x]) }))
    o = order(n)
    tmp = rast(tmp)
    tmp = tmp[[o]]
    names(tmp) = n
    ll[[v]] = tmp
  }

  ll
} else {
  out
}
