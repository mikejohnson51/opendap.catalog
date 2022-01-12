#' Extract grid metadata from NC Pointer
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param X_name Name of X diminion. If NULL it is found
#' @param Y_name Name of Y diminion. If NULL it is found
#' @return list with (proj, ext, and diminsion)
#' @importFrom ncmeta nc_coord_var nc_grid_mapping_atts nc_gm_to_prj
#' @importFrom RNetCDF var.get.nc

.resource_grid = function(nc, X_name = NULL, Y_name = NULL){

  if(is.null(X_name) | is.null(Y_name)){
    atts = ncmeta::nc_coord_var(nc)
    X_name   = omit.na(unique(atts$X))
    Y_name   = omit.na(unique(atts$Y))
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

  X    = RNetCDF::var.get.nc(nc, X_name)
  Y    = RNetCDF::var.get.nc(nc, Y_name)

  if(degree & any(X > 180.001)){X = X - 360}

  list(
    proj = proj,
    ext = c(min(X), max(X), min(Y), max(Y)),
    diminsion = c(length(X), length(Y))
  )

}

#' Extract time metadata from NC Pointer
#' @param nc "NetCDF" object which points to the NetCDF dataset. Found with RNetCDF::open.nc.
#' @param T_name Name of T diminion. If NULL it is found
#' @return list with (duration, interval, and nT)
#' @importFrom ncmeta nc_coord_var
#' @importFrom RNetCDF var.get.nc var.inq.nc utcal.nc att.get.nc var.get.nc

.resource_time = function(nc, T_name = NULL){

  if(is.null(T_name) ){
    atts = ncmeta::nc_coord_var(nc)
    T_name   = omit.na(unique(atts$T))
  }

  T_var_info <- var.inq.nc(nc, T_name)

  time_steps <- utcal.nc(unitstring = att.get.nc(nc, T_var_info$name, "units"),
                         value = var.get.nc(nc, T_var_info$name, unpack = TRUE),
                         type = "c")

  dT = diff(time_steps)

  g = expand.grid(unique(dT), units(dT)) %>%
    data.frame()

  names(g) = c("value", "interval")

  if(nrow(g) > 1 & all(c(28,30,31) %in% g$value)){
    g = data.frame(value = 1, interval = "months")
  }

  #If time is within 5 days of today then we call the range Open
  maxDate = ifelse(max(time_steps)  >= Sys.time() - (5*86400) & max(time_steps)  <= Sys.time() +1 ,
                   "..",
                   as.character(max(time_steps)))

  nT = ifelse(maxDate == "..", NA, length(time_steps))

  list(#Tmin = min(time_steps),
    #Tmax = max(time_steps),
    duration = paste0(min(time_steps), "/", maxDate),
    interval = paste(g$value, g$interval),
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
