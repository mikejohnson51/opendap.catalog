################################################################################

#' Convert OpenDAP to start/count call
#' @param dap dap description
#' @return numeric array
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
#' @export

dap.summary = function(dap){

  xy   = expand.grid(unique(dap$ncols), unique(dap$nrows))
  cells = prod(xy[,1] * xy[,2])
  xDim = paste0(xy[,1], collapse = " - ")
  yDim = paste0(xy[,2], collapse = " - ")
  tDim = unique(dap$Tdim)
  tI   = unique(dap$interval)
  var =  paste0(dap$varname, " [", dap$units,"] (", dap$long_name, ")")

cat("vars:  ", paste(">", var, collapse = "\n\t"))
cat("\nX:     ", formatC(xDim, big.mark = ",", digits = 0, format = "f"), paste0("(", dap$X_name[1], ")"))
cat("\nY:     ", formatC(yDim, big.mark = ",", digits = 0, format = "f"), paste0("(", dap$Y_name[1], ")"))
cat("\nT:     ", formatC(tDim, big.mark = ",", digits = 0, format = "f"), paste0("(", dap$T_name[1], " - ", unique(dap$interval), ")"))
cat("\nvalues:", formatC(cells * tDim * length(var),
        big.mark = ",", digits = 0, format = "f"), "(vars*X*Y*T)")
}

#' @title Crop DAP file
#' @description Crop an OpenDAP resource file to a given AOI and time bound
#' @param URL local file path or dodC URL
#' @param catolog subset of open.dap catolog
#' @param AOI sf object
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate  end date (YYYY- MM-DD)
#' @param grid_path path to grid json
#' @details if AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @export
#' @importFrom terra vect intersect ext project
#' @importFrom jsonlite read_json

dap_crop = function(URL       = NULL,
                    catolog   = NULL,
                    AOI       = NULL,
                    startDate = NULL,
                    endDate   = NULL,
                    grid_path = '/Users/mjohnson/github/opendap.catalog/cat_grids.json'){

  if(!is.null(URL)){
    catolog = read_dap_file(URL, id = "local")
    catolog$tiled = ""
  }

  if(is.null(startDate) & is.null(endDate)){
    catolog$T = paste0("[0:1:", catolog$nT - 1 ,"]")
    catolog$Tdim = catolog$nT
    tmp = do.call(rbind, strsplit(catolog$duration, "/"))
    catolog = cbind(catolog, data.frame(startDate = tmp[,1], endDate = tmp[,2]))

  } else {
    ### TIME CROP!!
    if(is.null(endDate)){ endDate = startDate}

    if(grepl("hour", catolog$interval[1])){
      startDate = paste(startDate, "00:00:00")
      endDate = paste(endDate, "23:00:00")
    }

    startDate = as.POSIXct(startDate, tz = "UTC")
    endDate   = as.POSIXct(endDate,   tz = "UTC")

    out = list()

    for(i in 1:nrow(catolog)){

      time_steps = parse_date(duration = catolog$duration[i],
                              interval = catolog$interval[i])


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

    dur     = catolog$duration
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

    make_ext = function(cat){
      terra::ext(c(
        min(cat$Xn, cat$X1),
        max(cat$Xn, cat$X1),
        min(cat$Yn, cat$Y1),
        max(cat$Yn, cat$Y1)))
    }

     if(catolog$id[1] != 'local'){
        grids = jsonlite::read_json(grid_path, simplifyVector = TRUE)
        grids = grids[grids$grid.id %in% catolog$grid.id, ]

        if(length(unique(grids$proj)) > 1){
          bol = sapply(1:nrow(grids), function(x){  terra::relate(terra::ext(terra::project(AOIspat, grids$proj[x])), make_ext(grids[x,]), "intersects")[1,1]})
        } else {
          tmp_aoi = terra::project(AOIspat, grids$proj[1])
          bol = sapply(1:nrow(grids), function(x){  terra::relate(terra::ext(tmp_aoi), make_ext(grids[x,]), "intersects")[1,1]})
        }

        catolog = merge(catolog, grids[bol, ])
     }

    out = lapply(1:nrow(catolog), function(i){
      tryCatch({
        terra::intersect(make_ext(catolog[i,]), terra::ext(terra::project(AOIspat, catolog$proj[i])))
      }, error = function(e) { NULL }
      )})

    for(i in 1:nrow(catolog)){

      X_coords = seq(catolog$X1[i], catolog$Xn[i], length.out = catolog$ncols[i])
      Y_coords = seq(catolog$Y1[i], catolog$Yn[i], length.out = catolog$nrow[i])

      ys = c(which.min(abs(Y_coords -  out[[i]]$ymin)), which.min(abs(Y_coords -  out[[i]]$ymax))) - 1
      xs = c(which.min(abs(X_coords -  out[[i]]$xmin)), which.min(abs(X_coords - out[[i]]$xmax))) - 1

      catolog$Y[i]   = paste0("[", paste(sort(ys), collapse = ":1:"), "]")
      catolog$X[i]   = paste0("[", paste(sort(xs), collapse = ":1:"), "]")
      catolog$X1[i]  = min(X_coords[xs + 1])
      catolog$Xn[i]  = max(X_coords[xs + 1])
      catolog$Y1[i]  = min(Y_coords[ys + 1])
      catolog$Yn[i]  = max(Y_coords[ys + 1])
      catolog$ncols[i] = abs(diff(xs)) + 1
      catolog$nrows[i] = abs(diff(ys)) + 1
    }
  }

  if(catolog$tiled[1] == "XY"){
    catolog$URL = paste0(catolog$URL, "/", catolog$tile, ".ncml?", catolog$varname, catolog$T, catolog$Y, catolog$X)
  } else {
    catolog$URL = paste0(catolog$URL, "?", catolog$varname, catolog$T, catolog$Y, catolog$X)
  }

  catolog$X = NULL
  catolog$Y = NULL
  catolog$T = NULL

  catolog
}


#' Download DAP resource
#' @param dap data.frame from catolog or dap_crop
#' @return SpatRaster
#' @export
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom terra sprc merge
#' @importFrom foreach `%dopar%` foreach
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores

dap_get = function(dap){

  suppressWarnings({
  i <- NULL
  `%dopar%` <- foreach::`%dopar%`
  #doMC::registerDoMC()
  doParallel::registerDoParallel(cores = parallel::detectCores() - 1)

  out = foreach::foreach(i = 1:nrow(dap)) %dopar% {
      tryCatch({
        if(grepl("http", dap$URL[i])){
          get_data(dap[i,])
        } else {
          dap_to_local(dap[i,])
        }
      }, error = function(e){
        dap$URL[i]
      })
    }

  # out = list()
  #
  # for(i in 1:nrow(dap)){
  #         out[[i]] = tryCatch({
  #           if(grepl("http", dap$URL[i])){
  #             get_data(dap[i,])
  #           } else {
  #             dap_to_local(dap[i,])
  #           }
  #         }, error = function(e){
  #           dap$URL[i]
  #         })
  # }

  out = lapply(1:length(out), function(x){var_to_terra(out[[x]], dap[x,])})
  names(out) =  sub("_$", "", paste0(dap$varname, "_", dap$scenario))

  if(any(dap$tiled == "XY")){
    terra::merge(terra::sprc(out))
  } else {
    out
  }
  })
}

#' Convert OpenDAP to start/count call
#' @param dap dap description
#' @return
#' @export
#' @importFrom RNetCDF open.nc close.nc var.inq.nc var.get.nc

dap_to_local = function(dap){

  nc = open.nc(sub("\\?.*", "", dap$URL))
  on.exit(close.nc(nc))

  k = regmatches(dap$URL, gregexpr("\\[.*?\\]", dap$URL))[[1]]
  k  <- gsub("[", "", k, fixed = TRUE)
  k  <- gsub("]", "", k, fixed = TRUE)

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

#' Variable Array to Terra
#' @param var array
#' @param dap dap description
#' @return SpatRast
#' @export
#' @importFrom terra rast flip

var_to_terra = function(var, dap){

  resx <- (dap$Xn - dap$X1) / (dap$ncols-1)
  resy <- (dap$Yn - dap$Y1) / (dap$nrows-1)

  xmin <- dap$X1 - 0.5 * resx
  xmax <- dap$Xn + 0.5 * resx
  ymin <- dap$Y1 - 0.5 * resy
  ymax <- dap$Yn + 0.5 * resy

  r = terra::rast(xmin = xmin,
                  xmax = xmax,
                  ymin = ymin,
                  ymax = ymax,
                  nrows = dap$nrows,
                  ncols = dap$ncols,
                  nlyrs = dap$Tdim,
                  crs   = dap$proj)

  r[] = var

  if(dap$toptobottom){ r =  terra::flip(r) }

  names(r) = seq.POSIXt(as.POSIXct(dap$startDate),
                        as.POSIXct(dap$endDate),
                        length.out  = dap$Tdim)

  r

}

#' Get DAP Array
#' @param dap dap description
#' @return SpatRast
#' @export
#' @importFrom RNetCDF open.nc

get_data = function(dap){
    nc = RNetCDF::open.nc(dap$URL)
    on.exit(close.nc(nc))
    as.vector(RNetCDF::var.get.nc(nc, dap$varname, unpack = TRUE))
}
