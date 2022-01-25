# AOI = AOI::aoi_get(state = "FL", county = "all")
#
#
# data = dap_crop(URL = "https://cida.usgs.gov/thredds/dodsC/bcsd_obs",
#                 AOI = NULL,
#                 startDate = "1995-01-01",
#                 endDate   = "1995-02-05")
#
# print.dap(data)
#
# ooo = get_dap(data)
#
# terra::plot(ooo[[2]])

################################################################################

#' Convert OpenDAP to start/count call
#' @param dap
#' @return
#' @export
#' @importFrom RNetCDF open.nc close.nc var.inq.nc var.get.nc

dap_to_local = function(dap){

  nc = open.nc(sub("\\?.*", "", dap$URL))
  on.exit(close.nc(nc))

  k = regmatches(dap$URL, gregexpr("\\[.*?\\]", dap$URL))[[1]]
  k  <- gsub("[", "", k, fixed = TRUE)
  k <- gsub("]", "", k, fixed = TRUE)

  nc_var_info <- var.inq.nc(nc, dap$varname)
  X_var_info  <- var.inq.nc(nc, dap$X_name)$dimids
  Y_var_info <- var.inq.nc(nc, dap$Y_name)$dimids
  T_var_info <- var.inq.nc(nc, dap$T_name)$dimids

  dimid_order <- match(nc_var_info$dimids,
                       c(T_var_info, Y_var_info, X_var_info))

  start = (as.numeric(sapply(strsplit(k, ":"),"[[",1)) + 1)[dimid_order]
  count = (c(dap$Tdim, dap$nrows, dap$ncols))[dimid_order]

  var.get.nc(nc, dap$varname,
             start = start,
             count = count,
             unpack = TRUE)

}

#' Print Summary Information About a OpenDAP Resource
#' @description Print summary information about a DAP summary
#' @param dap data.frame from catolog or dap_crop
#' @return
#' @export

print.dap = function(dap){

  xDim = unique(dap$ncols)
  yDim = unique(dap$nrows)
  tDim = unique(dap$Tdim)
  tI   = unique(data$interval)
  var =  paste0(dap$varname, " [", dap$units,"]")

cat("vars:  ", paste(">", var, collapse = "\n\t"))
cat("\nX:     ", formatC(xDim, big.mark = ",", digits = 0, format = "f"), paste0("(", dap$X_name[1], ")"))
cat("\nY:     ", formatC(yDim, big.mark = ",", digits = 0, format = "f"), paste0("(", dap$Y_name[1], ")"))
cat("\nT:     ", formatC(tDim, big.mark = ",", digits = 0, format = "f"), paste0("(", dap$T_name[1], " - ", unique(dap$interval), ")"))
cat("\nvalues:", formatC(xDim * yDim * tDim * length(tDim),
        big.mark = ",", digits = 0, format = "f"), "(vars*X*Y*T)")
}


#' @title Crop DAP file
#' @description Crop an OpenDAP resource file to a given AOI and time bound
#' @param URL local file path or dodC URL
#' @param catolog subset of open.dap catolog
#' @param AOI sf object
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate  end date (YYYY- MM-DD)
#' @details if AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @export
#' @importFrom terra vect intersect ext project

dap_crop = function(URL       = NULL,
                    catolog   = NULL,
                    AOI       = NULL,
                    startDate = NULL,
                    endDate   = NULL){

  if(!is.null(URL)){
    catolog = read_dap_file(URL, id = "local")
    catolog = variable_meta(catolog, verbose = FALSE)
  }

  if(is.null(startDate) & is.null(endDate)){
    catolog$T = paste0("[0:1:", catolog$nT - 1 ,"]")
    catolog$Tdim = catolog$nT
    tmp = do.call(rbind, strsplit(catolog$duration, "/"))
    catolog = cbind(catolog, data.frame(startDate = tmp[,1], endDate = tmp[,2]))

  } else {
      ### TIME CROP!!
      if(is.null(endDate)){ endDate = startDate}

      startDate = as.POSIXct(startDate, tz = "UTC")
      endDate   = as.POSIXct(endDate,   tz = "UTC")

      out = list()

      for(i in 1:nrow(catolog)){

        time_steps = parse_date(catolog$duration[i], catolog$interval[i])

        if(startDate >= max(time_steps)){
          out[[i]] = NULL
        } else {

          T1 = which.min(abs(time_steps - startDate)) - 1
          Tn = which.min(abs(time_steps - endDate)) - 1

          out[[i]] = cbind(catolog[i,], data.frame(T = paste0("[",T1, ":1:", Tn, "]"),
                                               Tdim  =  (Tn -  T1) + 1,
                                               startDate = time_steps[T1 + 1],
                                               endDate =   time_steps[Tn + 1]))

        }
      }

     dur = catolog$duration
     catolog = do.call(rbind, out)

     if(length(catolog) == 0){
      stop("Requested Time not found in ",  unique(dur), call. = FALSE)
     }
  }
  #####

  if(is.null(AOI)){
    catolog$X = paste0("[0:1:", catolog$ncols - 1 ,"]")
    catolog$Y = paste0("[0:1:", catolog$nrows - 1 ,"]")
  } else {

    AOIspat = terra::vect(AOI)

    out = lapply(1:nrow(catolog), function(i){
      tryCatch({
        terra::intersect(terra::ext(c(min(catolog$Xn[i], catolog$X1[i]),
                               max(catolog$Xn[i], catolog$X1[i]),
                               min(catolog$Yn[i], catolog$Y1[i]),
                               max(catolog$Yn[i], catolog$Y1[i]))),
                         terra::ext(terra::project(AOIspat, catolog$proj[i])))
      }, error = function(e) { NULL }
      )})

    catolog = catolog[!sapply(out, is.null), ]

    for(i in 1:nrow(catolog)){

      X_coords = seq(catolog$X1[i], catolog$Xn[i], length.out = catolog$ncols[i])
      Y_coords = seq(catolog$Y1[i], catolog$Yn[i], length.out = catolog$nrow[i])

      ys = c(which.min(abs(Y_coords -  out[[i]]$ymin)), which.min(abs(Y_coords -  out[[i]]$ymax))) - 1
      xs = c(which.min(abs(X_coords -  out[[i]]$xmin)), which.min(abs(X_coords - out[[i]]$xmax))) - 1

      catolog$Y[i]   = paste0("[", paste(sort(ys), collapse = ":1:"), "]")
      catolog$X[i]   = paste0("[", paste(sort(xs), collapse = ":1:"), "]")
      catolog$X1[i]  = min(X_coords[xs + 1])
      catolog$Xn[i]  = max(X_coords[xs + 1])
      catolog$Y1[i]  = min(X_coords[ys + 1])
      catolog$Yn[i]  = max(X_coords[ys + 1])
      catolog$ncols[i] = abs(diff(xs)) + 1
      catolog$nrows[i] = abs(diff(ys)) + 1
    }
  }

    catolog$URL = paste0(catolog$URL, "?", catolog$varname, catolog$T, catolog$Y, catolog$X)
    catolog$X = NULL
    catolog$Y = NULL
    catolog$T = NULL

    catolog
}



#' Download DAP resource
#' @param dap data.frame from catolog or dap_crop
#' @return
#' @export
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom terra rast flip src merge

dap_get = function(dap){

  # my.cluster <- parallel::makeCluster(
  #   parallel::detectCores() - 1,
  #   type = "PSOCK"
  # )

  # suppressWarnings({
  #     doParallel::registerDoParallel(cl = my.cluster)
  # })


    # var = foreach::foreach(i = 1:nrow(dap)) %dopar% {
    #   get_data(dap$URL[i], dap$varname[i])
    # }

  var = lapply(1:nrow(dap), function(i){
    if(grepl(dap$URL[i], "https")){
      get_data(URL = dap$URL[i], dap$varname[i])
    } else {
      dap_to_local(dap[i,])
    }

  })

  out = list()

  for(i in 1:length(var)){

    resx <- (dap$Xn[i] - dap$X1[i]) / (dap$ncols[i]-1)
    resy <- (dap$Yn[i] - dap$Y1[i]) / (dap$nrows[i]-1)

    xmin <- dap$X1[i] - 0.5 * resx
    xmax <- dap$Xn[i] + 0.5 * resx
    ymin <- dap$Y1[i] - 0.5 * resy
    ymax <- dap$Yn[i] + 0.5 * resy

    r = terra::rast(xmin = xmin,
                    xmax = xmax,
                    ymin = ymin,
                    ymax = ymax,
                    nrows = dap$nrows[i],
                    ncols = dap$ncols[i],
                    nlyrs = dap$Tdim[i],
                    crs   = dap$proj[i])

    r[] = var[[i]]

    if(dap$toptobottom[i]){ r =  terra::flip(r) }

    names(r) = seq.POSIXt(as.POSIXct(dap$startDate[i]),
                          as.POSIXct(dap$endDate[i]),
                          length.out  = dap$Tdim[i])

    out[[i]] = r
  }

  names(out) =  sub("_$", "", paste0(dap$varname, "_", dap$scenario))

  #parallel::stopCluster(cl = my.cluster)
  tryCatch(do.call(c, out), error = function(e){ terra::merge(terra::src(out))})

}


# future::plan('multicore', workers = future::availableCores() - 1)
# var = furrr::future_pmap(dap[, c("URL", "varname")], get_data)



get_data = function(URL, varname){
  tryCatch({
    nc = RNetCDF::open.nc(URL)
    as.vector(RNetCDF::var.get.nc(nc, varname, unpack = TRUE))

  }, error = function(e){
    URL
  })
}
