#' Parse Dates from duratoon and interval
#' @param duration time durations
#' @param interval time interval
#' @export

parse_date = function(duration, interval){

  d = strsplit(duration, "/")[[1]]

  if(d[2] == ".."){ d[2] = as.character(Sys.Date()) }

  #TODO move to dap_meta?
  if(interval == "1 months"){
    d[1] = format(as.POSIXct(d[1], tz = "UTC"), "%Y-%m-01")
  }

  if(grepl("hour", interval)){
    d[1] = format(as.POSIXct(d[1], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
    d[2] = format(as.POSIXct(d[2], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  }

  seq.POSIXt(as.POSIXct(d[1], tz = "UTC"),
             as.POSIXct(d[2], tz = "UTC"),
             interval)
}


#' Extract grid metadata from NC Pointer
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param X_name Name of X diminion. If NULL it is found
#' @param Y_name Name of Y diminion. If NULL it is found
#' @param stopIfNotEqualSpaced stop if not equal space grid
#' @return list with (proj, ext, and dimension)
#' @importFrom ncmeta nc_coord_var nc_grid_mapping_atts nc_gm_to_prj
#' @importFrom RNetCDF var.get.nc

.resource_grid = function(nc, X_name = NULL, Y_name = NULL, stopIfNotEqualSpaced = TRUE){

  if(is.null(X_name) | is.null(Y_name)){
    atts = dap_xyzv(nc)
    X_name   = omit.na(unique(atts$X_name))
    Y_name   = omit.na(unique(atts$Y_name))
  }

  nc_grid_mapping <- suppressWarnings(ncmeta::nc_grid_mapping_atts(nc))

  degree = grepl("degree", try_att(nc, X_name, "units"), ignore.case = TRUE)

  if(length(nc_grid_mapping) == 0) {

    if(degree) {
      message(paste("No projection information found. \n",
                    "Coordinate variable units are degrees so, \n",
                    "assuming EPSG:4326"))
      proj = "EPSG:4326"
    } else {
      warning("No projection information found in nc file.")
      proj = NA
    }
  } else {
    proj <- try(ncmeta::nc_gm_to_prj(nc_grid_mapping))
    if(class(proj) == "try-error") {
      proj = NA
    } else {
      proj
    }
  }

  ncols <-  RNetCDF::dim.inq.nc(nc, X_name)$len
  nrows <-  RNetCDF::dim.inq.nc(nc, Y_name)$len

  xx <- try(RNetCDF::var.get.nc(nc, X_name))

  if (inherits(xx, "try-error")) {
    xx <- seq_len(ncols)
  }

  rs <- xx[-length(xx)] - xx[-1]

  if (! isTRUE ( all.equal( min(rs), max(rs), tolerance = 0.025, scale= abs(min(rs))) ) ) {
    if (is.na(stopIfNotEqualSpaced)) {
      warning('cells are not equally spaced; you should extract values as points')
    } else if (stopIfNotEqualSpaced) {
      stop('cells are not equally spaced; you should extract values as points')
    }
  }

  if(any(xx > 180) & degree) { xx = xx - 360}

  xrange <- c(min(xx), max(xx))
  resx <- (xrange[2] - xrange[1]) / (ncols-1)
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

  if (! isTRUE ( all.equal( min(rs), max(rs), tolerance=0.025, scale= abs(min(rs))) ) ) {
    if (is.na(stopIfNotEqualSpaced)) {
      warning('cells are not equally spaced; you should extract values as points')
    } else if (stopIfNotEqualSpaced) {
      stop('cells are not equally spaced; you should extract values as points')
    }
  }

  yrange <- c(min(yy), max(yy))
  resy <- (yrange[2] - yrange[1]) / (nrows-1)

  if (yy[1] > yy[length(yy)]) {
    toptobottom  <- FALSE
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
    toptobottom = toptobottom)
}

#' Extract time metadata from NC Pointer
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param T_name Name of T diminion. If NULL it is found
#' @return list with (duration, interval, and nT)
#' @importFrom ncmeta nc_coord_var
#' @importFrom RNetCDF var.get.nc var.inq.nc utcal.nc att.get.nc var.get.nc

.resource_time = function(nc, T_name = NULL){

  if(is.null(T_name) ){
    atts     = ncmeta::nc_coord_var(nc)
    T_name   = omit.na(unique(atts$T))
  }

  T_var_info <- var.inq.nc(nc, T_name)

  time_steps <- utcal.nc(unitstring =  att.get.nc(nc, T_var_info$name, "units"),
                         value = var.get.nc(nc, T_var_info$name, unpack = TRUE),
                         type = "c")

  dT = diff(time_steps)

  g = data.frame(expand.grid(unique(dT), units(dT)))
  g = g[order(g$Var1),]
  g$n = as.numeric(table(dT))

  names(g) = c("value", "interval", "n")

  if(nrow(g) > 1 & all(g$value %in% c(28,29,30,31))){
    g = data.frame(value = 1, interval = "months")
  } else {
    g = g[which.max(g$n), ]
  }

  #If time is within 5 days of today then we call the range Open
  maxDate = ifelse(max(time_steps)  >= Sys.time() - (5*86400) & max(time_steps)  <= Sys.time() + 1 ,
                   "..",
                   as.character(max(time_steps)))

  nT = ifelse(maxDate == "..", NA, length(time_steps))

  int = paste(g$value, g$interval)

  if(length(int) == 0){int = "0"}

  list(
    duration = paste0(min(time_steps), "/", maxDate),
    interval = int,
    nT   = nT
  )

}

#' TryCatch around RNetCDF::att.get.nc()
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param variable ID or name of the variable from which the attribute will be read, or "NC_GLOBAL" for a global attribute.
#' @param attribute  Attribute name or ID.
#' @return Vector with a data type that depends on the NetCDF variable. For NetCDF variables of type NC_CHAR, the R type is either character or raw, as specified by argument rawchar. For NC_STRING, the R type is character. Numeric variables are read as double precision by default, but the smallest R type that exactly represents each external type is used if fitnum is TRUE.
#' @importFrom RNetCDF att.get.nc

try_att = function(nc, variable, attribute){
  tryCatch({
    RNetCDF::att.get.nc(nc, variable, attribute)
  }, error = function(e) {
    NA
  }
  )
}

#' Remove NAs from Vector
#' @param x vector
#' @return vector x without NA values

omit.na = function(x){ x[!is.na(x)] }
