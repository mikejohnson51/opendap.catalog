#' Align Grid to DAP Resource
#' @param AOI 'sf' or SpatVect object
#' @param ref catalog entries for data source
#' @return
#' @export
#' @importFrom terra project vect ext intersect rast rowColFromCell cells res

.local_reference_extent = function(AOI, ref){

  AOIbb = terra::project(terra::vect(AOI), ref$proj[1])

  dataset_exts = lapply(ref$ext, terra::ext)

  dataset_exts_ints = lapply(1:length(dataset_exts), function(x){
    tryCatch({
      terra::intersect(dataset_exts[[x]], AOIbb)
    }, error = function(e){ NULL}
    )
  })

  out = list()

  for(i in 1:length(dataset_exts_ints)){

    if(!is.null(dataset_exts_ints[[i]])){

      bb2 = terra::rast(extent = ref$ext[[i]],
                        nrows  = ref$dimension[[i]][1],
                        ncols  = ref$dimension[[i]][2],
                        crs    = ref$proj[i])

      oo =  terra::rowColFromCell(bb2,  terra::cells(bb2, dataset_exts_ints[[i]]))

      out[[i]] = data.frame(
        URL = paste0(ref$URL[i], '?', ref$varname[i]),
        Y = paste0("[", paste(range(oo[,1]) - 1, collapse = ":1:"), "]"),
        X = paste0("[", paste(range(oo[,2]) - 1, collapse = ":1:"), "]"),
        ext = I(list(as.vector(dataset_exts_ints[[i]]))),
        res = I(list(terra::res(bb2))),
        ncols = length(unique(oo[,2])),
        nrows = length(unique(oo[,1])),
        proj = ref$proj[i]
      )
    }

  }

  do.call(rbind, out)
}

#' Parse Dates from duratoon and interval
#' @param duration
#' @param interval
#' @return
#' @export

parse_date = function(duration, interval){

  d = strsplit(duration, "/")[[1]]

  if(d[2] == ".."){ d[2] = Sys.Date() }

  seq.POSIXt(as.POSIXct(d[1], tz = "UTC"),
             as.POSIXct(d[2], tz = "UTC"),
             interval)
}

#' Align Time to DAP Resource
#' @param startDate start date (YYYY-MM-DD)
#' @param enDate start date (YYYY-MM-DD). If NULL, then only startDate is found.
#' @param ref catalog entries for data source
#' @return
#' @export

.local_time_extent = function(startDate, endDate = NULL, ref){

  if(is.null(endDate)){ endDate = startDate}

  startDate = as.POSIXct(paste0(startDate), tz = "UTC")
  endDate   = as.POSIXct(paste0(endDate), tz = "UTC")

  time_steps = parse_date(ref$duration, ref$interval)

  if(endDate > tail(time_steps, 1)){
    warning("Dataset ends on: ", tail(time_steps,1), "\n changing endDate")
    endDate = tail(time_steps,1)
  }

  if(startDate > tail(time_steps, 1)){
    warning("Dataset ends on: ", tail(time_steps,1), "\n changing startDate")
    startDate = tail(time_steps,1)
  }

  if(startDate < head(time_steps, 1)){
    warning("Dataset starts on: ", head(time_steps,1), "\n changing startDate")
    startDate = head(time_steps,1)
  }

  list(T = paste0("[",
                  which.min(abs(time_steps - startDate)) - 1,
                  ":",
                  which.min(abs(time_steps - endDate)) - 1,
                  "]"),
       Tdim  =  which.min(abs(time_steps - endDate)) -  which.min(abs(time_steps - startDate)) + 1,
       startDate = startDate,
       endDate = endDate)
}
