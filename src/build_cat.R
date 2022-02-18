# Setup -------------------------------------------------------------------
library(opendap.catalog)
library(dplyr)

grid_vars = c('grid.id', 'X_name', 'Y_name', 'X1', 'Xn', 'Y1', 'Yn', 'resX', 'resY', 'ncols', 'nrows', 'proj', "toptobottom")

param_vars = c('id', 'grid.id',  'URL', 'tiled',
               'variable', 'varname', 'long_name', 'units',
               'model', 'ensemble', 'scenario',
               "T_name", 'duration', 'interval', "nT")

split_grids = function(raw){
  raw$grid.id = ifelse(is.na(raw$grid.id), raw$id, raw$grid.id)
  distinct(select(raw, grid_vars))
}

split_params = function(raw){
  raw$grid.id = ifelse(is.na(raw$grid.id), raw$id, raw$grid.id)
  select(raw, param_vars)
}


sanity_check = function(raw){
  n = names(raw)
  a = unique(c(param_vars, grid_vars))
  paste(a[!a %in% n], collapse = ", ")
}

meta = list()

# Start Building  ---------------------------------------------------------

####  MACA ####
meta[['maca']] = bind_rows(read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html', 'maca_day'),
                           read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html', "maca_month")) %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA),
                  sep = "_")  %>%
  dap_meta() %>%
  mutate(tiled = "T")

####  GridMET ####
meta[['gridmet']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html', "gridmet") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA),
                  sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "")

####  TerraClim ####

meta[['terraclim']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html', "terraclim") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "")


####  LOCA ####
meta[["loca"]] = bind_rows(read_dap_file('https://cida.usgs.gov/thredds/dodsC/loca_historical', id = "loca"),
                           read_dap_file('https://cida.usgs.gov/thredds/dodsC/loca_future', id = "loca")) %>%
  tidyr::separate(varname,  into = c('variable', 'model', 'ensemble', 'scenario'), sep = "_", remove = FALSE) %>%
  mutate(tiled = "T")

####  VIC ####
meta[['vic']] = read_tds(URL = 'https://www.reacchpna.org/thredds/nw.csc.hydrology-vic.aggregated.html', id  = "vic") %>%
  tidyr::separate(link,  into = c(NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA, NA, NA), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "")

####  BCCA ####
meta[['bcca']] = bind_rows(read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future',     id = 'bcca'),
                           read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical', id = 'bcca')) %>%
  tidyr::separate(varname,  into = c(NA, NA, 'variable', NA, 'model', 'scenario', 'ensemble'), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "T")

####  BCSD VIC ####
meta[['bcsd_vic']] = read_dap_file('https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC', id = "bcsd_vic") %>%
  tidyr::separate(varname, into = c("model",  'scenario',  'ensemble', "variable"), sep = "_", extra = "merge") %>%
  dap_meta() %>%
  mutate(tiled = "")

####  BCSD ####
meta[['bcsd']] = read_dap_file('https://cida.usgs.gov/thredds/dodsC/bcsd_obs', id = "bcsd") %>%
  dap_meta() %>%
  mutate(tiled = "")

####  DAYMET ####
meta[['daymet']] = read_dap_file(URL = 'https://thredds.daac.ornl.gov/thredds/dodsC/daymet-v4-agg/na.ncml', "daymetv4") %>%
  dap_meta() %>%
  mutate(tiled = "")


####  TOPOWX ####
meta[['topowx']] = bind_rows(read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx',     id = 'topowx_day'),
                             read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx_monthly', id = 'topowx_month')) %>%
  dap_meta() %>%
  mutate(tiled = "")

#### DCP ####
meta[['dcp']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/dcp/conus_t',  id = 'dcp') %>%
  tidyr::separate(varname, into = c("model",  'scenario', "variable", NA, NA), sep = "-", extra = "merge", remove = FALSE) %>%
  dap_meta() %>%
  mutate(tiled = "")

####  Maurer ####
meta[['maurer']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml',  id = 'maurer') %>%
  tidyr::separate(varname, into = c('scenario', "model", "ensemble", "variable"), sep = "_", extra = "merge", remove = FALSE) %>%
  dap_meta() %>%
  mutate(tiled = "")

####  ssebopeta ####
meta[['ssebopeta']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly',  id = 'ssebopeta') %>%
  dap_meta() %>%
  mutate(tiled = "")

####  PRISM ####
meta[['prism_daily']] = read_dap_file(URL = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2021/PRISM_combo_20211010.nc', id = 'prism') %>%
  mutate(duration = '1981-01-01/..', variable = varname) %>%
  dap_meta() %>%
  mutate(URL =   gsub("20211010", "{YYYYMMDD}", gsub("/2021\\/", "{YYYY}", URL)), tiled = "T")

meta[['prism_monthly']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/prism_v2', id = 'prism_month') %>%
  mutate(variable = varname) %>%
  dap_meta() %>%
  mutate(tiled = "")

####  CHIRPS ####
meta[['chirps']] = bind_rows(read_dap_file("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalMonthlyP05", "chirps_month"),
                             read_dap_file("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05", "chirps_day")) %>%
  mutate(variable = varname) %>%
  dap_meta() %>%
  mutate(tiled = "")

####  JPL Wind ####
meta[['jplwind']] = read_dap_file(URL = "https://upwell.pfeg.noaa.gov/erddap/griddap/jplCcmp35aWindPentad", "jplwind") %>%
  mutate(variable = varname) %>%
  dap_meta() %>%
  mutate(tiled = "")

#### LDAS ####
library(rvest)
das = html_nodes(read_html('https://hydro1.gesdisc.eosdis.nasa.gov/dods/'), "a")
URL = unique(gsub("\\.[a-z]*$","", html_attr(das, "href")))
URL = URL[grepl("NLDAS|GLDAS", URL)]

ldas = list()

for(i in 1:length(URL)){
  ldas[[i]] = read_dap_file(paste0(URL[i], "/"), id = basename(URL[i]))
  message(i, " of ", length(URL))
}


meta[['ldas']] = bind_rows(ldas) |>
  group_by(by = nrows) |>
  mutate(grid.id = paste0("DAS_", cur_group_id()),
         variable  = varname,
         tiled = "") |>
  ungroup()

# Write  ------------------------------------------------------------------

lapply(meta, sanity_check)

raw = bind_rows(meta)

grids = raw |>
  split_grids() |>
  bind_rows(readRDS("src/modis_grids.RDS"))

jsonlite::write_json(grids, "cat_grids.json", pretty = TRUE)
usethis::use_data(grids, overwrite = TRUE)

params = raw |>
  bind_rows(mutate(readRDS("src/modis_param.RDS"), grid.id = "modis")) |>
  split_params()

jsonlite::write_json(params, "cat_params.json", pretty = TRUE)
usethis::use_data(params, overwrite = TRUE)


