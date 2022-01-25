#' Align Grid to DAP Resource
#' @param AOI 'sf' or SpatVect object
#' @param ref catalog entries for data source
#' @export
#' @importFrom terra project vect ext intersect rast rowColFromCell cells res

.local_reference_extent = function(AOI, ref){

  AOIbb = ext(terra::project(terra::vect(AOI), ref$proj[1]))

  out = lapply(1:nrow(ref), function(x){
     tryCatch({
       terra::intersect(ext(c(min(ref$Xn[i], ref$X1[i]),
                              max(ref$Xn[i], ref$X1[i]),
                              min(ref$Yn[i], ref$Y1[i]),
                              max(ref$Yn[i], ref$Y1[i]))),
                         AOIbb)
   }, error = function(e) { NULL }
  )})

  tmp = ref[!sapply(out, is.null), ]

  fin = list()

  for(i in 1:nrow(tmp)){

      X_coords = seq(tmp$X1[i], tmp$Xn[i], length.out = tmp$ncols[i])
      Y_coords = seq(tmp$Y1[i], tmp$Yn[i], length.out = tmp$nrow[i])

      ys = c(which.min(abs(Y_coords -  out[[i]]$ymin)), which.min(abs(Y_coords -  out[[i]]$ymax))) - 1
      xs = c(which.min(abs(X_coords -  out[[i]]$xmin)), which.min(abs(X_coords - out[[i]]$xmax))) - 1

      fin[[i]] = data.frame(
        URL = tmp$URL[i],
        Y   = paste0("[", paste(sort(ys), collapse = ":1:"), "]"),
        X   = paste0("[", paste(sort(xs), collapse = ":1:"), "]"),
        ext = I(list(c(X_coords[xs + 1], Y_coords[ys + 1]))),
        ncols = abs(diff(xs)) + 1,
        nrows = abs(diff(ys)) + 1,
        proj = ref$proj[i],
        varname = tmp$varname[i],
        toptobottom = tmp$toptobottom[i]
      )
    }

  do.call(rbind, fin)
}

#' Parse Dates from duratoon and interval
#' @param duration time durations
#' @param interval time interval
#' @export

parse_date = function(duration, interval){

  d = strsplit(duration, "/")[[1]]

  if(d[2] == ".."){ d[2] = Sys.Date() }

  #TODO move to dap_meta?
  if(interval == "1 months"){
    d[1] = format(as.POSIXct(d[1], tz = "UTC"), "%Y-%m-01")
  }

  seq.POSIXt(as.POSIXct(d[1], tz = "UTC"),
             as.POSIXct(d[2], tz = "UTC"),
             interval)
}

#' Align Time to DAP Resource
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate start date (YYYY-MM-DD). If NULL, then only startDate is found.
#' @param ref catalog entries for data source
#' @export

.local_time_extent = function(startDate, endDate = NULL, ref){

  if(is.null(endDate)){ endDate = startDate}

  startDate = as.POSIXct(paste0(startDate), tz = "UTC")
  endDate   = as.POSIXct(paste0(endDate), tz = "UTC")

  out = list()

  for(i in 1:nrow(ref)){

    time_steps = parse_date(ref$duration[i], ref$interval[i])

    if(startDate >= max(time_steps)){
      out[[i]] = NULL
    } else {
      tmp_start  = max(min(time_steps), startDate)
      tmp_end    = min(max(time_steps), endDate)

      out[[i]] = cbind(ref[i,], data.frame(T = paste0("[",
                                       which.min(abs(time_steps - startDate)) - 1,
                                       ":1:",
                                       which.min(abs(time_steps - endDate)) - 1,
                                       "]"),
                            Tdim  =  which.min(abs(time_steps - endDate)) -  which.min(abs(time_steps - startDate)) + 1,
                            startDate = tmp_start,
                            endDate = tmp_end))
    }
  }

  do.call(rbind, out)
}


#' Get data for Catalog entry for an AOI and time
#'
#' @param AOI sf or SpatVect Object
#' @param ref catalog entries for data source
#' @param startDate start date (YYYY-MM-DD)
#' @param enDate start date (YYYY-MM-DD). If NULL, then only startDate is found.
#' @export
#' @importFrom doMC registerDoMC
#' @importFrom foreach foreach `%dopar%`
#' @importFrom terra rast ext res src merge
#' @importFrom RNetCDF open.nc var.get.nc

