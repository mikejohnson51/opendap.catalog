go_get_dap_data = function(dap){
  tryCatch({
    if(grepl("http", dap$URL)){
      var_to_terra(get_data(dap), dap)
    } else {
      var_to_terra(dap_to_local(dap), dap)
    }
  }, error = function(e){
    dap$URL
  })
}

#' Convert catalog entry to vect
#' @param cat catalog entry
#' @return vect object
#' @export
#' @importFrom terra vect

make_vect = function(cat){

  xmin = min(cat$Xn, cat$X1)
  xmax = max(cat$Xn, cat$X1)
  ymin = min(cat$Yn, cat$Y1)
  ymax = max(cat$Yn, cat$Y1)

  vect(paste0("POLYGON ((",
              xmin, " ", ymin, ", ",
              xmin, " ", ymax, ", ",
              xmax, " ", ymax, ", ",
              xmax, " ", ymin, ", ",
              xmin, " ", ymin ,"))"),
       crs = cat$proj)
}

#' Convert catalog entry to extent
#' @param cat catalog entry
#' @return SpatExtent object
#' @export
#' @importFrom terra ext

make_ext = function(cat){
  terra::ext(c(
    min(cat$Xn, cat$X1),
    max(cat$Xn, cat$X1),
    min(cat$Yn, cat$Y1),
    max(cat$Yn, cat$Y1)))
}

#' Convert OpenDAP to start/count call
#' @param dap dap description
#' @param get shpuld data be collected?
#' @return numeric array
#' @export
#' @importFrom RNetCDF open.nc close.nc var.inq.nc var.get.nc

dap_to_local = function(dap, get = TRUE){

  if(nrow(dap) != 1){
    stop("This function process only 1 DAP row at a time... currently there are ", nrow(dap))
  }

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

  if(get){
    var.get.nc(nc, dap$varname,
               start = start,
               count = count,
               unpack = TRUE)
  } else {
    data.frame(file = sub("\\?.*", "", dap$URL),
               variable = dap$varname,
               start = I(list(start)),
               count = I(list(count)), unpack = TRUE)
  }

}

#' Print Summary Information About a OpenDAP Resource
#' @description Print summary information about a DAP summary
#' @param dap data.frame from catalog or dap_crop
#' @param url Unique Resource Identifier (http or local)
#' @export

dap_summary = function(dap = NULL, url = NULL){

if(!is.null(url) & is.null(dap)){
  dap = dap_crop(url)
} else{
  xy    = expand.grid(unique(dap$ncols), unique(dap$nrows))
  cells = prod(xy[, 1] * xy[, 2])
  xDim  = formatC(
    paste0(xy[, 1], collapse = " - "),
    big.mark = ",",
    digits = 0,
    format = "f"
  )
  yDim  = formatC(
    paste0(xy[, 2], collapse = " - "),
    big.mark = ",",
    digits = 0,
    format = "f"
  )
  tDim  = formatC(
    unique(dap$Tdim),
    big.mark = ",",
    digits = 0,
    format = "f"
  )
  ext   = paste0(
    round(unique(pmin(dap$X1, dap$Xn)), 3),
    ", ",
    round(unique(pmax(dap$X1, dap$Xn)), 3),
    ", ",
    round(unique(pmin(dap$Y1, dap$Yn)), 3),
    ", ",
    round(unique(pmax(dap$Y1, dap$Yn)), 3),
    " (xmin, xmax, ymin, ymax)"
  )
  var   = paste0(dap$varname, " [", dap$units, "] (", dap$long_name, ")")
  a = dap$proj[1]

  {
    cat("source:       ",    strsplit(dap$URL[1], "\\?")[[1]][1], "\n")
    cat("varname(s):\n  ", paste(">", var, collapse = "\n   "))
    cat(paste0("\n", paste(rep("=", 50), collapse = "")))
    cat(
      "\ndiminsions: ",
      paste0(
        xDim,
        ", ",
        yDim,
        ", ",
        tDim,
        " (names: ",
        dap$X_name[1],
        ",",
        dap$Y_name[1],
        ",",
        dap$T_name[1],
        ")"
      )
    )
    cat(
      "\nresolution: ",
      paste0(
        round(dap$resX[1], 3),
        ", ",
        round(dap$resY[1], 3),
        ", ",
        dap$interval[1]
      )
    )
    cat("\nextent:     ",  ext)
    cat("\ncrs:        ", ifelse(nchar(a) > 50, paste0(strtrim(a, 50), '...'), a))
    cat(
      "\ntime:       ",
      as.character(dap$startDate[1]),
      'to',
      as.character(dap$endDate[1])#,
      #paste0("(by: ", dap$interval[1], ")")
    )
    cat(paste0("\n", paste(rep("=", 50), collapse = "")))
    cat(
      "\nvalues:",
      formatC(
        cells * as.numeric(tDim) * length(var),
        big.mark = ",",
        digits = 0,
        format = "f"
      ),
      "(vars*X*Y*T)"
    )
  }
  }
}

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

dap_crop = function(URL       = NULL,
                    catolog   = NULL,
                    AOI       = NULL,
                    startDate = NULL,
                    endDate   = NULL,
                    varname   = NULL,
                    verbose   = TRUE
                    ){

  if(!is.null(URL)){
    catolog       <-  read_dap_file(URL, id = "local")
    catolog$tiled <-  ""
  }

  ## TIME
  if(is.null(startDate) & is.null(endDate)){
    catolog$T = paste0("[0:1:", catolog$nT - 1 ,"]")
    catolog$Tdim = catolog$nT
    tmp = do.call(rbind, strsplit(catolog$duration, "/"))
    catolog = cbind(catolog, data.frame(startDate = tmp[,1], endDate = tmp[,2]))

  } else {
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

  ## SPACE (XY)
  if(is.null(AOI)){
    catolog$X = paste0("[0:1:", catolog$ncols - 1 ,"]")
    catolog$Y = paste0("[0:1:", catolog$nrows - 1 ,"]")
  } else {

    AOIspat = terra::vect(AOI)

     if(catolog$id[1] != 'local'){
        grids = opendap.catalog::grids
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

    drops = which(sapply(out, is.null))

    if(length(drops) != 0){
      catolog = catolog[-drops,]
      out     = catolog[-drops]
    }

    if(nrow(catolog) < 1){ stop("No resources intersect with provided AOI", call. = FALSE) }

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


  if(any(grepl("XY", catolog$tiled))){
    catolog$URL = paste0(catolog$URL, "/", catolog$tile, ".ncml?", catolog$varname, catolog$T, catolog$Y, catolog$X)
  } else {
    catolog$URL = paste0(catolog$URL, "?", catolog$varname, catolog$T, catolog$Y, catolog$X)
  }

  if(!is.null(varname)){

    if(!varname %in% catolog$varname){
      stop('variable in resource include:\n',
           paste(catolog$varname, collapse = ", "))
    }

    catolog = catolog[catolog$varname %in% varname,]
  }

  catolog$X = NULL
  catolog$Y = NULL
  catolog$T = NULL

  if(verbose){ dap_summary(catolog)}

  catolog
}


#' Download DAP resource
#' @param dap data.frame from catolog or dap_crop
#' @param varname  name of variable to extract. If NULL, then get all
#' @return SpatRaster
#' @export
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom terra sprc merge units nlyr
#' @importFrom future.apply future_lapply

dap_get = function(dap, varname = NULL){

  if(!is.null(varname)){

    if(!varname %in% dap$varname){
      stop('variable in resource include:\n',
           paste(dap$varname, collapse = ", "))
    }

    dap = dap[dap$varname %in% varname,]
  }

  out = future_lapply(1:nrow(dap), FUN = function(x){ go_get_dap_data(dap[x,]) })

  #out = lapply(1:length(out), function(x){var_to_terra(out[[x]], dap[x,])})
  names(out) =  sub("_$", "", paste0(dap$varname, "_", dap$scenario))

  if(any(grepl("XY", dap$tiled))){
    u = unique(unlist(lapply(out, units)))
    if(length(u) == 1) {
      out = terra::merge(terra::sprc(out))
      terra::units(out) = rep(u, nlyr(out))
      out
    } else {
      out
    }
  } else if(any(dap$tiled == "T")) {
    #TODO
    # group dap by id, varname, units
    # merge by ID if units are the same
    # if units are the same
    out
  } else {
    out
  }
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

  if(length(dim(var)) == 2) {
    dim(var) = c(dim(var), 1)
  }

  r[] = var

  if(dap$toptobottom){ r =  terra::flip(r) }

  terra::units(r) = dap$units

  names(r) = seq.POSIXt(as.POSIXct(dap$startDate),
                        as.POSIXct(dap$endDate),
                        length.out  = dap$Tdim)

  r

}

#' @title Get DAP
#' @description Define and get data from a DAP resource
#' @param URL local file path or dodC URL
#' @param catolog subset of open.dap catolog
#' @param AOI sf object
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate  end date (YYYY- MM-DD)
#' @param varname  name of variable to extract. If NULL, then get all
#' @param verbose  Should dap_summary be printed?
#' @details Wraps dap_get and dap_crop into one.
#' If AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @export
#' @importFrom terra vect intersect ext project

dap = function(URL       = NULL,
               catolog   = NULL,
               AOI       = NULL,
               startDate = NULL,
               endDate   = NULL,
               varname   = NULL,
               verbose   = TRUE){

  dap = dap_crop(URL = URL,
                 catolog = catolog,
                 AOI = AOI,
                 startDate = startDate,
                 endDate = endDate,
                 varname = varname,
                 verbose = verbose)

  dap_get(dap)

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
