meta = list()
library(dplyr)

meta[['maca_daily']]   = read_tds(URL = 'http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html', 'maca_day') %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA), sep = "_") %>%
  dap_meta()

meta[['maca_monthly']]  = read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html', "maca_month") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA), sep = "_")  %>%
  dap_meta()

meta[['gridmet']]  = read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html', "gridmet") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA), sep = "_") %>%
  dap_meta()

meta[['terraclim']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html', "terraclim") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA), sep = "_") %>%
  dap_meta()

meta[['loca']] = bind_rows(read_dap_file('https://cida.usgs.gov/thredds/dodsC/loca_historical', id = "loca"),
                           read_dap_file('https://cida.usgs.gov/thredds/dodsC/loca_historical', id = "loca")) %>%
  tidyr::separate(varname,  into = c('variable', 'model', 'ensemble', 'scenario'), sep = "_", remove = FALSE) %>%
  dap_meta()

meta[['vic']] = read_tds(URL = 'https://www.reacchpna.org/thredds/nw.csc.hydrology-vic.aggregated.html', id  = "vic") %>%
  tidyr::separate(link,  into = c(NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA, NA, NA), sep = "_") %>%
  dap_meta()

meta[['bcca']] = bind_rows(read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future',     id = 'bcca'),
                           read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical', id = 'bcca')) %>%
  tidyr::separate(varname,  into = c(NA, NA, 'variable', NA, 'model', 'scenario', 'ensemble'), sep = "_") %>%
  dap_meta()

meta[['bcsd']] = read_dap_file('https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC', id = "bcsd_vic") %>%
  tidyr::separate(varname, into = c("model",  'scenario',  'ensemble', "variable"), sep = "_", extra = "merge") %>%
  dap_meta()

meta[['daymet']] = read_dap_file(URL = 'https://thredds.daac.ornl.gov/thredds/dodsC/daymet-v4-agg/na.ncml', "daymet") %>%
  mutate(variable = varname) %>%
  dap_meta()

meta[['topowx']] = bind_rows(read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx',     id = 'topowx'),
                             read_dap_file(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx_monthly', id = 'topowx')) %>%
  mutate(variable = varname) %>%
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

meta[['jplwind']] = read_dap_file(URL = "https://upwell.pfeg.noaa.gov/erddap/griddap/jplCcmp35aWindPentad", "test") %>%
  mutate(variable = varname) %>%
  dap_meta()



# p = read_dap_file(URL = "http://iridl.ldeo.columbia.edu/SOURCES/.OSU/.PRISM/.monthly/dods", "prism")
#
# p2 = read_dap_file(URL = "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.NARR/.three-hourly/.NARR-A/dods", "cru")
#


bind_rows(meta) %>%
  jsonlite::write_json("cat.json", pretty = TRUE)


#jsonlite::fromJSON('https://mikejohnson51.github.io/opendap.catalog/cat.json')
