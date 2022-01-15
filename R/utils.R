#' Retrieve XYTV data from DAP URL
#' @param url a OpenDap URL
#' @return data.frame with (varname, X_name, Y_name, T_name)
#' @export
#' @importFrom dplyr filter select
#' @importFrom RNetCDF open.nc close.nc
#' @importFrom ncmeta nc_coord_var

dap_xyxv = function(url){

  .data <- NULL

  nc   = RNetCDF::open.nc(url)

  atts = ncmeta::nc_coord_var(nc)

  T_name   = omit.na(unique(atts$T))
  X_name   = omit.na(unique(atts$X))
  Y_name   = omit.na(unique(atts$Y))

  ######

  raw = filter(atts, .data$X == X_name, .data$Y == Y_name, .data$T == T_name) %>%
    select(varname = .data$variable, X_name = .data$X, Y_name = .data$Y, T_name = .data$T)
}

#' Read from a THREDDS catalog HTML page
#' @description Scraps a TDS landing page for avalaible datasets
#' @param URL URL to THREDDS catalog
#' @param id character. Uniquely named TDS identifier
#' @return data.frame with (link, URL, id)
#' @export
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom dplyr mutate filter

read_tds = function(URL, id){

  .data <- NULL

  dat = read_html(URL) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    data.frame()

  names(dat) = "link"

  dat %>%
    mutate(link =  gsub(".*=","", .data$link),
           URL = paste0(dirname(URL), "/dodsC/", .data$link, ".nc"),
           id = !!id) %>%
    filter(!grepl("http|https|html", .data$link))
}


#' Read from a OpenDAP landing page
#' @description Reads an OpenDap resources and returns metadata
#' @param URL URL to OpenDap resource
#' @param id character. Uniquely named dataset identifier
#' @return data.frame with (varname, X_name, Y_name, T_name, URL, id), grid, and time
#' @export
#' @importFrom RNetCDF open.nc
#' @importFrom ncmeta nc_coord_var
#' @importFrom dplyr mutate filter select left_join

read_dap_file  = function(URL, id){

  .data <- NULL

  nc   = RNetCDF::open.nc(URL)

  atts = ncmeta::nc_coord_var(nc) %>%
    dplyr::select(variable, X,Y,T)

  raw = filter(atts,  !apply(atts, 1, function(x){any(is.na(x))}))

  T_name   = omit.na(unique(raw$T))
  X_name   = omit.na(unique(raw$X))
  Y_name   = omit.na(unique(raw$Y))

  raw = raw %>%
    select(varname = .data$variable, X_name = .data$X, Y_name = .data$Y, T_name = .data$T) %>%
    mutate(URL = URL, id = !!id)

  time = data.frame(.resource_time(nc, T_name)) %>%
    mutate(id =!!id)

  raw = left_join(raw, time, by = "id")

  g = .resource_grid(nc, X_name, Y_name)

  raw$proj = g$proj
  raw$ext  = I(list(g$ext))
  raw$dimension  = I(list(g$dimension))

  close.nc(nc)

  raw
}

#' Add Variable Metadata
#' @param raw a data.frame
#' @return data.frame
#' @export
#' @importFrom RNetCDF open.nc file.inq.nc var.inq.nc close.nc
#' @importFrom dplyr bind_rows right_join group_by slice ungroup

variable_meta = function(raw){

  if(!"variable" %in% names(raw)){
    stop("raw must include variable column")
  }

  if(all(c('varname', 'units') %in% names(raw))){
    message("Variable metadata already exists")
    return(raw)
  } else {

  .data <- NULL

  tmp = group_by(raw, .data$variable) %>%
    slice(1) %>%
    ungroup()

  ll = list()

  for(i in 1:nrow(tmp)){

    nc = tryCatch({
      suppressMessages({
        RNetCDF::open.nc(paste0(tmp$URL[i], "#fillmismatch"))
      })
    }, error = function(e){
      NULL
    }
    )

    if(!is.null(nc)){

      if("varname" %in% names(raw)){
        varnames <- RNetCDF::var.inq.nc(nc, tmp$varname[i])$name
      } else {
        nvar <- RNetCDF::file.inq.nc(nc)$nvar

        varnames <- character(nvar)

        for(j in seq_len(nvar)) {
          varnames[j] <- RNetCDF::var.inq.nc(nc, j-1)$name
        }
      }

      if(!is.null(varnames)){
        name = varnames[!grepl("lat$|lon$|latitude$|longitude$|time$|crs$|day$",
                               varnames,
                               ignore.case = TRUE)]
      } else {
        name = varnames[i]
      }

      ll[[i]] = data.frame(
        variable = tmp$variable[i],
        varname = name,
        units = try_att(nc, name, "units")
      )

      message("[", tmp$id[i], ":", tmp$variable[i], "] (", i, "/", nrow(tmp), ")")
      RNetCDF::close.nc(nc)

    } else {

      ll[[i]] = data.frame(
        variable = raw$variable[i],
        varname = NA,
        units = NA
      )

      message(basename(raw$URL[i]), " fails")
    }
  }

  if("varname" %in% names(raw)) {
    out = bind_rows(ll) %>%
      select(-.data$varname)
  } else {
    out = bind_rows(ll)
  }

  return(right_join(out, raw, by = "variable"))

  }
}

#' Add Time Metadata
#' @param raw a data.frame
#' @return data.frame
#' @export
#' @importFrom RNetCDF open.nc close.nc
#' @importFrom dplyr mutate

time_meta = function(raw){

  if(all(c('duration', 'interval', 'nT') %in% names(raw))){
   message("Time metadata already exists")
   return(raw)
  } else {

  .data <- NULL

  flag = !"scenario" %in% names(raw)

  if(flag) {
    tmp = raw[1,]
  } else {
    tmp = raw %>%
      group_by(.data$scenario) %>%
      slice(1) %>%
      ungroup()
  }

  ll = list()

  for(i in 1:nrow(tmp)){

    nc = RNetCDF::open.nc(paste0(tmp$URL[i], "#fillmismatch"))

    ll[[i]] = as.data.frame(.resource_time(nc)) %>%
      mutate(scenario = tmp$scenario[i])

    message("[", tmp$id[i], ":", tmp$scenario[i], "] (", i, "/", nrow(tmp), ")")
    RNetCDF::close.nc(nc)
  }


  if(flag){

    raw$duration = ll[[1]]$duration
    raw$interval = ll[[1]]$interval
    raw$nT = ll[[1]]$nT

  } else {
    raw  = bind_rows(ll) %>%
      right_join(raw, by = "scenario")
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

  if(all(c('T_name', 'X_name', 'Y_name', 'dimension', 'ext', 'proj') %in% names(raw))){
    message("Grid metadata already exists")
    return(raw)
  } else {
    url = paste0(raw$URL[1], "#fillmismatch")
    o = dap_xyxv(url)
    nc = RNetCDF::open.nc(url)
    g = .resource_grid(nc)
    raw$proj = g$proj
    raw$ext  = I(list(g$ext))
    raw$dimension  = I(list(g$dimension))
    raw$X_name = o$X_name
    raw$Y_name = o$T_name
    raw$T_name = o$X_name
    RNetCDF::close.nc(nc)

    return(raw)
  }
}

#' Add All DAP Metadata
#' @param raw a data.frame
#' @return data.frame
#' @export
#' @importFrom dplyr `%>%`

dap_meta = function(raw){
 variable_meta(raw) %>%
    time_meta() %>%
    grid_meta()
}
