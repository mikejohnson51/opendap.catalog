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

omit.na = function(x){ x[!is.na(x)] }

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
    data.frame() %>%
    setNames(c("value", "interval"))

  if(nrow(g) > 1 & all(c(28,30,31) %in% g$value)){
    g = data.frame(value = 1, interval = "months")
  }

  #If time is within 5 days of today then we call the range Open
  maxDate = ifelse(max(time_steps)  >= Sys.time() - (5*86400), "..", max(time_steps))
  nT = ifelse(maxDate == "..", NA, length(time_steps))


  list(#Tmin = min(time_steps),
       #Tmax = max(time_steps),
       duration = paste0(min(time_steps), "/", maxDate),
       interval = paste(g$value, g$interval),
       nT   = nT
      )
}

date.seq = function(duration, interval){
  d = strsplit(duration, "/")[[1]]
  ed = as.POSIXct(ifelse(d[2] == "..", as.character(Sys.Date()), d[2]), tz = "UTC")
  seq.POSIXt(as.POSIXct(d[1], tz = 'UTC'), ed, by = interval)
}


#' Title
#'
#' @param nc
#' @param name
#' @param var
#' @return
#' @export
#' @importFrom RNetCDF att.get.nc

try_att = function(nc, name, var){
  tryCatch({
    RNetCDF::att.get.nc(nc, name, var)
  }, error = function(e) {
    NA
  }
  )
}

dap_xyxv = function(url){

  nc   = RNetCDF::open.nc(url)

  atts = ncmeta::nc_coord_var(nc)

  T_name   = omit.na(unique(atts$T))
  X_name   = omit.na(unique(atts$X))
  Y_name   = omit.na(unique(atts$Y))

  ######

  filter(atts, X == X_name, Y == Y_name, T == T_name) %>%
    select(variable, X_name = X, Y_name = Y, T_name = T)

}

#' Read from a THREDDS catalog
#' @param url URL to THREDDS catalog
#' @param id dataset identifier
#' @return
#' @export
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom dplyr mutate filter

read_tds = function(URL, id){

  dat = read_html(URL) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    data.frame()

  names(dat) = "link"

  dat %>%
    mutate(link =  gsub(".*=","", .data$link),
           id = !!id) %>%
    filter(!grepl("http|https|html", .data$link))
}


read_dap_file  = function(URL, id){

  nc   = RNetCDF::open.nc(URL)

  atts = ncmeta::nc_coord_var(nc)

  T_name   = omit.na(unique(atts$T))
  X_name   = omit.na(unique(atts$X))
  Y_name   = omit.na(unique(atts$Y))

  raw = filter(atts, X == X_name, Y == Y_name, T == T_name) %>%
    select(link = variable, X, Y, T) %>%
    mutate(URL = URL, id = !!id)

  time = data.frame(.resource_time(nc, T_name))
  time$id = id

  raw = left_join(raw, time, by = "id")

  g = .resource_grid(nc, X_name, Y_name)

  raw$proj = g$proj
  raw$ext  = I(list(g$ext))
  raw$diminsion  = I(list(g$diminsion))

  rename(raw, X_name = X, Y_name = Y, T_name = T, startDate = Tmin,  endDate = Tmax)
}

variable_meta = function(raw, names = NULL){

  tmp = group_by(raw, variable) %>%
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
      nvar <- RNetCDF::file.inq.nc(nc)$nvar
      varnames <- character(nvar)
      for(j in seq_len(nvar)) {
        varnames[j] <- RNetCDF::var.inq.nc(nc, j-1)$name
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
        units = try_att(nc, name, "units")#,
      )

      logger::log_info("[", tmp$id[i], ":", tmp$variable[i], "] (", i, "/", nrow(tmp), ")")
      RNetCDF::close.nc(nc)
    } else {
      ll[[i]] = data.frame(
        variable = raw$variable[i],
        varname = NA,
        units = NA#,
        #standard_name = NA,
        #long_name = NA,
        #description = NA
      )

      logger::log_info(basename(raw$URL[i]), " fails")
    }
  }

  bind_rows(ll) %>%
    right_join(raw, by = "variable")
}

time_meta = function(raw){

  flag = !"scenario" %in% names(raw)

  if(flag) {
    tmp = raw[1,]
  } else {
    tmp = raw %>%
      group_by(scenario) %>%
      slice(1) %>%
      ungroup()
  }

  ll = list()

  for(i in 1:nrow(tmp)){

    nc = RNetCDF::open.nc(paste0(tmp$URL[i], "#fillmismatch"))

    ll[[i]] = as.data.frame(.resource_time(nc)) %>%
      mutate(scenario = tmp$scenario[i])

    logger::log_info("[", tmp$id[i], ":", tmp$scenario[i], "] (", i, "/", nrow(tmp), ")")
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

  raw
}

grid_meta = function(raw){
  url = paste0(raw$URL[1], "#fillmismatch")
  o = dap_xyxv(url)
  g = .resource_grid(nc = RNetCDF::open.nc(url))
  raw$proj = g$proj
  raw$ext  = I(list(g$ext))
  raw$diminsion  = I(list(g$diminsion))
  raw$X_name = o$X_name
  raw$Y_name = o$T_name
  raw$T_name = o$X_name

  raw
}
