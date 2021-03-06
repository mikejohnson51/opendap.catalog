#' Read from a OpenDAP landing page
#' @description Reads an OpenDap resources and returns metadata
#' @param URL URL to OpenDap resource
#' @param id character. Uniquely named dataset identifier
#' @param varmeta should variable metadata be appended?
#' @return data.frame with (varname, X_name, Y_name, T_name, URL, id), grid, and time
#' @export
#' @importFrom RNetCDF open.nc close.nc

read_dap_file <- function(URL, id, varmeta = TRUE) {
  nc <- RNetCDF::open.nc(URL)
  on.exit(close.nc(nc))

  raw <- dap_xyzv(nc, varmeta = varmeta)
  raw$URL <- URL
  raw$id <- id

  raw <- merge(raw, data.frame(.resource_time(nc, raw$T_name[1]), id = id), by = "id")

  raw <- merge(raw, .resource_grid(nc, X_name = raw$X_name[1], Y_name = raw$Y_name[1]))

  raw
}

#' Get XYTV data from DAP URL
#' @param obj an OpenDap URL or NetCDF object
#' @param varmeta should variable metadata be appended?
#' @return data.frame with (varname, X_name, Y_name, T_name)
#' @export
#' @importFrom RNetCDF open.nc close.nc
#' @importFrom ncmeta nc_coord_var

dap_xyzv <- function(obj, varmeta = FALSE) {

  if (class(obj) != "NetCDF") {
    obj <- RNetCDF::open.nc(obj)
    on.exit(close.nc(obj))
  }

  raw <- ncmeta::nc_coord_var(obj)[, c("variable", "X", "Y", "T")]
  raw <- raw[!apply(raw, 1, function(x) {
    any(is.na(x))
  }), ]

  names(raw) <- c("varname", "X_name", "Y_name", "T_name")

  ll <- list()

  if (varmeta) {
    for (i in 1:nrow(raw)) {
      if (unique(ncmeta::nc_var(obj, raw$varname[i])$ndims) > 3) {
        ll[[i]] <- NULL
        warning("We do not support 4D datasets:", raw$varname[i])
      } else {
        ll[[i]] <- data.frame(
          varname = raw$varname[i],
          units = try_att(obj, raw$varname[i], "units"),
          long_name = try_att(obj, raw$varname[i], "long_name")
        )
      }
    }

    merge(raw, do.call(rbind, ll), by = "varname")
  } else {
    raw
  }
}

#' TryCatch around RNetCDF::att.get.nc()
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param variable ID or name of the variable from which the attribute will be read, or "NC_GLOBAL" for a global attribute.
#' @param attribute  Attribute name or ID.
#' @return Vector with a data type that depends on the NetCDF variable. For NetCDF variables of type NC_CHAR, the R type is either character or raw, as specified by argument rawchar. For NC_STRING, the R type is character. Numeric variables are read as double precision by default, but the smallest R type that exactly represents each external type is used if fitnum is TRUE.
#' @importFrom RNetCDF att.get.nc

try_att <- function(nc, variable, attribute) {
  tryCatch(
    {
      RNetCDF::att.get.nc(nc, variable, attribute)
    },
    error = function(e) {
      NA
    }
  )
}



#' Extract grid metadata from NC Pointer
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param X_name Name of X diminion. If NULL it is found
#' @param Y_name Name of Y diminion. If NULL it is found
#' @param stopIfNotEqualSpaced stop if not equal space grid
#' @return list with (proj, ext, and dimension)
#' @importFrom ncmeta nc_coord_var nc_grid_mapping_atts nc_gm_to_prj
#' @importFrom RNetCDF var.get.nc

.resource_grid <- function(nc, X_name = NULL, Y_name = NULL, stopIfNotEqualSpaced = TRUE) {
  omit.na <- function(x) {
    x[!is.na(x)]
  }

  if (is.null(X_name) | is.null(Y_name)) {
    atts <- dap_xyzv(nc)
    X_name <- omit.na(unique(atts$X_name))
    Y_name <- omit.na(unique(atts$Y_name))
  }

  nc_grid_mapping <- suppressWarnings(ncmeta::nc_grid_mapping_atts(nc))

  degree <- grepl("degree", try_att(nc, X_name, "units"), ignore.case = TRUE)

  if (nrow(nc_grid_mapping) == 0) {
    if (degree) {
      message(paste(
        "No projection information found. \n",
        "Coordinate variable units are degrees so, \n",
        "assuming EPSG:4326"
      ))
      proj <- "EPSG:4326"
    } else {
      warning("No projection information found in nc file.")
      proj <- NA
    }
  } else {
    proj <- try(ncmeta::nc_gm_to_prj(nc_grid_mapping))
    if (class(proj) == "try-error") {
      proj <- NA
    } else {
      proj
    }
  }

  ncols <- RNetCDF::dim.inq.nc(nc, X_name)$len
  nrows <- RNetCDF::dim.inq.nc(nc, Y_name)$len

  xx <- try(RNetCDF::var.get.nc(nc, X_name))

  if (inherits(xx, "try-error")) {
    xx <- seq_len(ncols)
  }

  rs <- xx[-length(xx)] - xx[-1]

  if (!isTRUE(all.equal(min(rs), max(rs), tolerance = 0.025, scale = abs(min(rs))))) {
    if (is.na(stopIfNotEqualSpaced)) {
      warning("cells are not equally spaced; you should extract values as points")
    } else if (stopIfNotEqualSpaced) {
      stop("cells are not equally spaced; you should extract values as points")
    }
  }

  if (any(xx > 180) & degree) {
    xx <- xx - 360
  }

  xrange <- c(min(xx), max(xx))
  resx <- (xrange[2] - xrange[1]) / (ncols - 1)
  X1 <- xx[1]
  Xn <- xx[length(xx)]
  rm(xx)

  yy <- try(RNetCDF::var.get.nc(nc, Y_name))

  if (inherits(yy, "try-error")) {
    yy <- seq_len(nrows)
  }

  Y1 <- yy[1]
  Yn <- yy[length(yy)]

  rs <- yy[-length(yy)] - yy[-1]

  if (!isTRUE(all.equal(min(rs), max(rs), tolerance = 0.025, scale = abs(min(rs))))) {
    if (is.na(stopIfNotEqualSpaced)) {
      warning("cells are not equally spaced; you should extract values as points")
    } else if (stopIfNotEqualSpaced) {
      stop("cells are not equally spaced; you should extract values as points")
    }
  }

  yrange <- c(min(yy), max(yy))
  resy <- (yrange[2] - yrange[1]) / (nrows - 1)

  if (yy[1] > yy[length(yy)]) {
    toptobottom <- FALSE
  } else {
    toptobottom <- TRUE
  }

  rm(yy)

  data.frame(
    proj = proj,
    # xmin, xmax, ymin, ymax
    X1 = X1,
    Xn = Xn,
    Y1 = Y1,
    Yn = Yn,
    resX = resx,
    resY = resy,
    ncols = ncols,
    nrows = nrows,
    toptobottom = toptobottom
  )
}

#' Extract time metadata from NC Pointer
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param T_name Name of T diminion. If NULL it is found
#' @return list with (duration, interval, and nT)
#' @importFrom ncmeta nc_coord_var
#' @importFrom RNetCDF var.get.nc var.inq.nc utcal.nc att.get.nc var.get.nc

.resource_time <- function(nc, T_name = NULL) {
  omit.na <- function(x) {
    x[!is.na(x)]
  }

  if (is.null(T_name)) {
    atts <- ncmeta::nc_coord_var(nc)
    T_name <- omit.na(unique(atts$T))
  }

  T_var_info <- var.inq.nc(nc, T_name)

  time_steps <- utcal.nc(
    unitstring = att.get.nc(nc, T_var_info$name, "units"),
    value = var.get.nc(nc, T_var_info$name, unpack = TRUE),
    type = "c"
  )

  dT <- diff(time_steps)

  g <- data.frame(expand.grid(unique(dT), units(dT)))
  g <- g[order(g$Var1), ]
  g$n <- as.numeric(table(dT))

  names(g) <- c("value", "interval", "n")

  if (nrow(g) > 1 & all(g$value %in% c(28, 29, 30, 31))) {
    g <- data.frame(value = 1, interval = "months")
  } else {
    g <- g[which.max(g$n), ]
  }

  # If time is within 5 days of today then we call the range Open
  maxDate <- ifelse(max(time_steps) >= Sys.time() - (5 * 86400) & max(time_steps) <= Sys.time() + 1,
    "..",
    as.character(max(time_steps))
  )

  nT <- ifelse(maxDate == "..", NA, length(time_steps))

  int <- paste(g$value, g$interval)

  if (length(int) == 0) {
    int <- "0"
  }

  list(
    duration = paste0(min(time_steps), "/", maxDate),
    interval = int,
    nT = nT
  )
}

#' Parse Dates from duratoon and interval
#' @param duration time durations
#' @param interval time interval
#' @export

parse_date <- function(duration, interval) {
  d <- strsplit(duration, "/")[[1]]

  if (d[2] == "..") {
    d[2] <- as.character(Sys.Date())
  }

  if (interval == "1 months") {
    d[1] <- format(as.POSIXct(d[1], tz = "UTC"), "%Y-%m-01")
  }

  if (grepl("hour", interval)) {
    d[1] <- format(as.POSIXct(d[1], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
    d[2] <- format(as.POSIXct(d[2], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  }

  seq.POSIXt(
    as.POSIXct(d[1], tz = "UTC"),
    as.POSIXct(d[2], tz = "UTC"),
    interval
  )
}

#' @title Catalog Spatial Intersect
#' @description Reduce and input catalog to those elements that intersect a given AOI
#' @param x a catalog element
#' @param AOI an AOI sf object
#' @param merge should the grid meta data be merged with the reduce catalog (default = TRUE)
#' @return data.frame
#' @export
#' @importFrom terra vect relate ext project union

spatial_intersect <- function(x, AOI, merge = TRUE) {

  # TODO need to remove/fix NA projections
  grids <- opendap.catalog::grids[!is.na(opendap.catalog::grids$proj), ]

  # Only work on grid ids found in x
  grids <- grids[grids$grid_id %in% x$grid_id, ]

  bboxs <- vect(sapply(1:nrow(grids), function(i) { make_vect(grids[i, ]) }))
  bbox_int = relate(bboxs, project(terra::union(vect(AOI)), grids$proj[1]), "intersects")
  g  = grids[bbox_int[,1], ]

  if (merge) {
    merge(x, g)
  } else {
    x[x$grid_id %in% g$grid_id, ]
  }
}

#' Convert catalog entry to extent
#' @param cat catalog entry
#' @return SpatExtent object
#' @export
#' @importFrom terra ext

make_ext <- function(cat) {
  ext(c(
    min(cat$Xn, cat$X1),
    max(cat$Xn, cat$X1),
    min(cat$Yn, cat$Y1),
    max(cat$Yn, cat$Y1)
  ))
}

#' Convert catalog entry to vect
#' @param cat catalog entry
#' @return vect object
#' @export
#' @importFrom terra vect

make_vect <- function(cat) {
  xmin <- min(cat$Xn, cat$X1)
  xmax <- max(cat$Xn, cat$X1)
  ymin <- min(cat$Yn, cat$Y1)
  ymax <- max(cat$Yn, cat$Y1)

  vect(paste0(
    "POLYGON ((",
    xmin, " ", ymin, ", ",
    xmin, " ", ymax, ", ",
    xmax, " ", ymax, ", ",
    xmax, " ", ymin, ", ",
    xmin, " ", ymin, "))"
  ),
  crs = cat$proj
  )
}


#' Read formated DAP URL as SpatRast
#' @param dap output from dap_crop
#' @return SpatRast
#' @export

go_get_dap_data <- function(dap) {
  tryCatch(
    {
      if (grepl("http", dap$URL)) {
        var_to_terra(get_data(dap), dap)
      } else {
        var_to_terra(dap_to_local(dap), dap)
      }
    },
    error = function(e) {
      dap$URL
    }
  )
}

#' Variable Array to SpatRast
#' @param var numeric array
#' @param dap dap description
#' @return SpatRast
#' @export
#' @importFrom terra rast flip units

var_to_terra <- function(var, dap) {
  resx <- (dap$Xn - dap$X1) / (dap$ncols - 1)
  resy <- (dap$Yn - dap$Y1) / (dap$nrows - 1)

  xmin <- dap$X1 - 0.5 * resx
  xmax <- dap$Xn + 0.5 * resx
  ymin <- dap$Y1 - 0.5 * resy
  ymax <- dap$Yn + 0.5 * resy

  r <- terra::rast(
    xmin = min(xmin, xmax),
    xmax = max(xmax, xmax),
    ymin = min(ymin, ymax),
    ymax = max(ymin, ymax),
    nrows = dap$nrows,
    ncols = dap$ncols,
    nlyrs = dap$Tdim,
    crs = dap$proj
  )

  if (length(dim(var)) == 2) {
    dim(var) <- c(dim(var), 1)
  }

  r[] <- var

  if (dap$toptobottom) {
    r <- terra::flip(r)
  }

  terra::units(r) <- dap$units

  names(r) <- seq.POSIXt(as.POSIXct(dap$startDate),
    as.POSIXct(dap$endDate),
    length.out  = dap$Tdim
  )

  r
}

#' Variable Array to SpatRast 2
#' @param var numeric array
#' @param dap dap description
#' @return SpatRast
#' @export
#' @importFrom terra rast crop units

var_to_terra2 <- function(dap) {

  if (nrow(dap) != 1) {
    stop("This function processes only 1 DAP row at a time... currently there are ", nrow(dap))
  }

  file <- sub("\\?.*", "", dap$URL)

  resx <- (dap$Xn - dap$X1) / (dap$ncols - 1)
  resy <- (dap$Yn - dap$Y1) / (dap$nrows - 1)

  xmin <- dap$X1 - 0.5 * resx
  xmax <- dap$Xn + 0.5 * resx
  ymin <- dap$Y1 - 0.5 * resy
  ymax <- dap$Yn + 0.5 * resy

  r <- terra::rast(
    xmin = min(xmin, xmax),
    xmax = max(xmax, xmax),
    ymin = min(ymin, ymax),
    ymax = max(ymin, ymax),
    crs = dap$proj
  )

  r = crop(rast(file), r)
  terra::units(r) <- dap$units

  names(r) <- seq.POSIXt(as.POSIXct(dap$startDate),
                         as.POSIXct(dap$endDate),
                         length.out  = dap$Tdim
  )

  r
}


#' Get DAP Array
#' @param dap dap description
#' @return SpatRast
#' @export
#' @importFrom RNetCDF open.nc

get_data <- function(dap) {
  nc <- RNetCDF::open.nc(paste0(dap$URL, "#fillmismatch"))
  on.exit(close.nc(nc))
  as.vector(RNetCDF::var.get.nc(nc, dap$varname, unpack = TRUE))
}

#' Convert OpenDAP to start/count call
#' @param dap dap description
#' @param get shpuld data be collected?
#' @return numeric array
#' @export
#' @importFrom RNetCDF open.nc close.nc var.inq.nc var.get.nc

dap_to_local <- function(dap, get = TRUE) {
  if (nrow(dap) != 1) {
    stop("This function processes only 1 DAP row at a time... currently there are ", nrow(dap))
  }

  nc <- open.nc(sub("\\?.*", "", dap$URL))
  on.exit(close.nc(nc))

  k <- regmatches(dap$URL, gregexpr("\\[.*?\\]", dap$URL))[[1]]
  k <- gsub("[", "", k, fixed = TRUE)
  k <- gsub("]", "", k, fixed = TRUE)

  nc_var_info <- var.inq.nc(nc, dap$varname)
  X_var_info <- var.inq.nc(nc, dap$X_name)$dimids
  Y_var_info <- var.inq.nc(nc, dap$Y_name)$dimids
  T_var_info <- var.inq.nc(nc, dap$T_name)$dimids

  dimid_order <- match(
    nc_var_info$dimids,
    c(T_var_info, Y_var_info, X_var_info)
  )

  start <- (as.numeric(sapply(strsplit(k, ":"), "[[", 1)) + 1)[dimid_order]

  count <- (c(dap$Tdim, dap$nrows, dap$ncols))[dimid_order]

  if (get) {
    var.get.nc(nc, dap$varname,
      start = start,
      count = count,
      unpack = TRUE
    )
  } else {
    data.frame(
      file = sub("\\?.*", "", dap$URL),
      variable = dap$varname,
      start = I(list(start)),
      count = I(list(count)), unpack = TRUE
    )
  }
}
