meta = list()
library(dplyr)

grid_vars = c('grid.id', 'X_name', 'Y_name', 'X1', 'Xn', 'Y1', 'Yn', 'resX', 'resY', 'ncols', 'nrows', 'proj', "toptobottom")

param_vars = c('id', 'grid.id',  'URL', 'tiled',
               'variable', 'varname', 'long_name', 'units',
               'model', 'ensemble', 'scenario',
               'duration', 'interval', "nT")

split_grids = function(raw){
  raw$grid.id = ifelse(is.na(raw$grid.id), raw$id, raw$grid.id)
  distinct(select(raw, grid_vars))
}

split_params = function(raw){
  raw$grid.id = ifelse(is.na(raw$grid.id), raw$id, raw$grid.id)
  select(raw, param_vars)
}


### Params (15): # id, tiled, URL, toptobottom,
                 # variable, varname, long_name, units, model,ensemble, scenario,
                 # duration, interval, nT
### Grid (12): id, X_name, Y_name, T_name, X1, Xn, Y1, Yn, resX, resY, ncols, nrows, proj

meta[['maca']] = bind_rows(read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html', 'maca'),
                           read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html', "maca")) %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA),
                  sep = "_")  %>%
  dap_meta() %>%
  mutate(tiled = "T")

meta[['gridmet']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html', "gridmet") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA),
                  sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "")

meta[['terraclim']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html', "terraclim") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "")

meta[["loca"]] = bind_rows(read_dap_file('https://cida.usgs.gov/thredds/dodsC/loca_historical', id = "loca"),
                           read_dap_file('https://cida.usgs.gov/thredds/dodsC/loca_future', id = "loca")) %>%
  tidyr::separate(varname,  into = c('variable', 'model', 'ensemble', 'scenario'), sep = "_", remove = FALSE) %>%
  mutate(tiled = "T")

meta[['vic']] = read_tds(URL = 'https://www.reacchpna.org/thredds/nw.csc.hydrology-vic.aggregated.html', id  = "vic") %>%
  tidyr::separate(link,  into = c(NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA, NA, NA), sep = "_") %>%
  dap_meta()

meta[['bcca']] = bind_rows(read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future',     id = 'bcca'),
                           read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical', id = 'bcca')) %>%
  tidyr::separate(varname,  into = c(NA, NA, 'variable', NA, 'model', 'scenario', 'ensemble'), sep = "_") %>%
  dap_meta()

meta[['bcsd_vic']] = read_dap_file('https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC', id = "bcsd_vic") %>%
  tidyr::separate(varname, into = c("model",  'scenario',  'ensemble', "variable"), sep = "_", extra = "merge") %>%
  dap_meta()

meta[['bcsd']] = read_dap_file('https://cida.usgs.gov/thredds/dodsC/bcsd_obs', id = "bcsd") %>%
  dap_meta()

meta[['daymet']] = read_dap_file(URL = 'https://thredds.daac.ornl.gov/thredds/dodsC/daymet-v4-agg/na.ncml', "daymet") %>%
  dap_meta()

meta[['topowx']] = bind_rows(read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx',     id = 'topowx'),
                             read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx_monthly', id = 'topowx')) %>%
  dap_meta()

meta[['dcp']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/dcp/conus_t',  id = 'dcp') %>%
  tidyr::separate(varname, into = c("model",  'scenario', "variable", NA, NA), sep = "-", extra = "merge", remove = FALSE) %>%
  dap_meta()

meta[['maurer']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml',  id = 'maurer') %>%
  tidyr::separate(varname, into = c('scenario', "model", "ensemble", "variable"), sep = "_", extra = "merge", remove = FALSE) %>%
  dap_meta()

meta[['ssebopeta']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly',  id = 'ssebopeta') %>%
  dap_meta()

meta[['prism_daily']] = read_dap_file(URL = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2021/PRISM_combo_20211010.nc', id = 'prism') %>%
  mutate(duration = '1981-01-01/..', variable = varname) %>%
  dap_meta() %>%
  mutate(URL =   gsub("20211010", "{YYYYMMDD}", gsub("/2021\\/", "{YYYY}", URL)))

meta[['prism_monthly']] = read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/prism_v2', id = 'prism_monthly') %>%
  mutate(variable = varname) %>%
  dap_meta()

meta[['chirps']] = bind_rows(read_dap_file("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalMonthlyP05", "chirps"),
                             read_dap_file("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05", "chirps")) %>%
  mutate(variable = varname) %>%
  dap_meta()

meta[['jplwind']] = read_dap_file(URL = "https://upwell.pfeg.noaa.gov/erddap/griddap/jplCcmp35aWindPentad", "jplwind") %>%
  mutate(variable = varname) %>%
  dap_meta()

####

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
  mutate(grid.id = paste0("DAS_", cur_group_id())) |>
  ungroup()


# MODIS

meta[['modis']]

# p = read_dap_file(URL = "http://iridl.ldeo.columbia.edu/SOURCES/.OSU/.PRISM/.monthly/dods", "prism")
#
# p2 = read_dap_file(URL = "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.NARR/.three-hourly/.NARR-A/dods", "cru")
#

nrow(distinct(select(oo, -id)))

oo = bind_rows(meta) %>%
  split_grids() |>
  jsonlite::write_json("cat.json", pretty = TRUE)

bind_rows(meta) %>%
  arrow::write_parquet("cat.parquet")

bind_rows(meta) %>%
  select(-X_name, -Y_name, -T_name, -X1, -Xn, -Y1, -Yn, -resX, -resY, -ncols, -nrows, -proj) |>
  jsonlite::write_json("cat_no_grid.json", pretty = TRUE)


#jsonlite::fromJSON('https://mikejohnson51.github.io/opendap.catalog/cat.json')
