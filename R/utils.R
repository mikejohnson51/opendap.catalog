#' Get XYTV data from DAP URL
#' @param obj an OpenDap URL or NetCDF object
#' @param varmeta should variable metadata be appended?
#' @return data.frame with (varname, X_name, Y_name, T_name)
#' @export
#' @importFrom RNetCDF open.nc close.nc
#' @importFrom ncmeta nc_coord_var

dap_xyzv = function(obj, varmeta = FALSE){

  if(class(obj) != "NetCDF"){
    obj   = RNetCDF::open.nc(obj)
    on.exit(close.nc(obj))
  }

  raw = ncmeta::nc_coord_var(obj)[, c('variable', 'X','Y','T')]
  raw = raw[!apply(raw, 1, function(x){any(is.na(x))}), ]
  names(raw) <- c('varname', "X_name", "Y_name", "T_name")

  ll = list()

  if(varmeta){
    for(i in 1:nrow(raw)){
    ll[[i]] = data.frame(
      varname = raw$varname[i],
      units = try_att(obj, raw$varname[i], "units"),
      long_name = try_att(obj, raw$varname[i], "long_name"))
    }


  merge(raw, do.call(rbind, ll), by = "varname")

  } else {
    raw
  }

}

#' Read from a THREDDS catalog HTML page
#' @description Scraps a TDS landing page for avalaible datasets
#' @param URL URL to THREDDS catalog
#' @param id character. Uniquely named TDS identifier
#' @return data.frame with (link, URL, id)
#' @export
#' @importFrom rvest read_html html_nodes html_attr

read_tds = function(URL, id){

  dat = read_html(URL)
  dat = html_nodes(dat, "a")
  dat = data.frame(link = html_attr(dat, "href"), id = id)

  dat$link =  gsub(".*=","", dat$link)

  dat$URL = paste0(dirname(URL), "/dodsC/", dat$link, ".nc")

  dat[!grepl("http|https|html", dat$link),]

}

#' Read from a OpenDAP landing page
#' @description Reads an OpenDap resources and returns metadata
#' @param URL URL to OpenDap resource
#' @param id character. Uniquely named dataset identifier
#' @param varmeta should variable metadata be appended?
#' @return data.frame with (varname, X_name, Y_name, T_name, URL, id), grid, and time
#' @export
#' @importFrom RNetCDF open.nc close.nc

read_dap_file  = function(URL, id, varmeta = TRUE){

  nc   = RNetCDF::open.nc(URL)
  on.exit(close.nc(nc))

  raw = dap_xyzv(nc, varmeta = varmeta)
  raw$URL = URL
  raw$id  = id

  raw = merge(raw,  data.frame(.resource_time(nc, raw$T_name[1]), id = id) , by = 'id')

  raw = merge(raw, .resource_grid(nc, X_name = raw$X_name[1], Y_name = raw$Y_name[1]))

  raw
}

#' Add Variable Metadata
#' @param raw a data.frame
#' @param verbose should messaging be displayed?
#' @return data.frame
#' @export
#' @importFrom RNetCDF open.nc file.inq.nc var.inq.nc close.nc

variable_meta = function(raw, verbose = TRUE){

  if(!"variable" %in% names(raw)){
    warning("raw must include variable column")
    if(!"variable" %in% names(raw)){
      warning("trying varname. Chance of failure...")
      raw$variable = raw$varname
    }
  }

  if(all(c('units', "long_name") %in% names(raw))){
    if(verbose){ message("Variable metadata already exists") }
    return(raw)
  } else {

  res <- by(raw, list(raw$variable), function(x) {
      c(URL = x$URL[1], variable = x$variable[1], id = x$id[1])
  })

  tmp <- data.frame(do.call(rbind, res))

  ll = list()

  for(i in 1:nrow(tmp)){

    ll[[i]] = tryCatch({
      t = dap_xyzv(paste0(tmp$URL[i], "#fillmismatch"), varmeta = TRUE)
      t$variable = tmp$variable[i]
      t
    }, error = function(e){
      NULL
    })


    if(verbose){
        message("[", tmp$id[i], ":", tmp$variable[i], "] (", i, "/", nrow(tmp), ")")
    }
  }

  return(merge(raw, do.call(rbind, ll), by = "variable"))

  }
}

#' Add Time Metadata
#' @param raw a data.frame
#' @return data.frame
#' @export
#' @importFrom RNetCDF open.nc close.nc

time_meta = function(raw){

  if(all(c('duration', 'interval', 'nT') %in% names(raw))){
   message("Time metadata already exists")
   return(raw)
  } else {

  flag = !"scenario" %in% names(raw)

  if(flag) {
    raw$scenario = "total"
    tmp = raw[1,]
  } else {

    res <- by(raw, list(raw$scenario, raw$id), function(x) {
      c(URL = x$URL[1], scenario = x$scenario[1], id = x$id[1], T_name = x$T_name[1])
    })

    tmp <- data.frame(do.call(rbind, res))
  }

  ll = list()

  for(i in 1:nrow(tmp)){

    nc = RNetCDF::open.nc(paste0(tmp$URL[i], "#fillmismatch"))

    ll[[i]] = data.frame(.resource_time(nc, T_name = tmp$T_name[i]),
                         scenario = tmp$scenario[i],
                         id = tmp$id[i])

    message("[", tmp$id[i], ":", tmp$scenario[i], "] (", i, "/", nrow(tmp), ")")
    RNetCDF::close.nc(nc)
  }


  if(flag){

    raw$duration = ll[[1]]$duration
    raw$interval = ll[[1]]$interval
    raw$nT = ll[[1]]$nT

  } else {
    raw  = merge(raw, do.call(rbind, ll), by = c("scenario", "id"))
  }

  return(raw)
  }
}

#' Add Grid Metadata
#' @param raw a data.frame
#' @return data.frame (raw, + dimension, proj, ext, X_name, Y_name, T_name)
#' @export
#' @importFrom RNetCDF open.nc close.nc

grid_meta = function(raw){

  if(all(c('T_name', 'X_name', 'Y_name', 'nrows', 'ncols', 'X1', "Xn", "Y1", "Yn", 'proj') %in% names(raw))){
    message("Grid metadata already exists")
    return(raw)
  } else {
    url = paste0(raw$URL[1], "#fillmismatch")
    nc = RNetCDF::open.nc(url)
    g = .resource_grid(nc)
    RNetCDF::close.nc(nc)

    return(cbind(raw, g))
  }
}

#' Add All DAP Metadata
#' @param raw a data.frame
#' @return data.frame
#' @export

dap_meta = function(raw){

 raw = variable_meta(raw)
 raw = time_meta(raw)
 raw = grid_meta(raw)

 raw
}
