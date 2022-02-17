## | ID | URL | varname | units | startDate | endDate | dT | proj | ext | dims |

library(dplyr)
library(glue)
meta = list()


meta[['maca_daily']]  = read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html', 'maca_day') %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA), sep = "_", remove = FALSE) %>%
  mutate(URL = glue("{base}{link}.nc", base = baseNorthwestern),
         link = NULL) %>%
  variable_meta() %>%
  time_meta() %>%
  grid_meta()


meta[['maca_monthly']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html', "maca_month") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA), sep = "_", remove = FALSE)  %>%
  mutate(URL = glue("{base}{link}.nc", base = baseNorthwestern),
         link = NULL) %>%
  variable_meta() %>%
  time_meta() %>%
  grid_meta()

meta[['gridmet']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html', "gridmet") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA), sep = "_", remove = FALSE) %>%
  mutate(URL = glue("{base}{link}.nc", base = baseNorthwestern),
         link = NULL) %>%
  variable_meta() %>%
  time_meta() %>%
  grid_meta()

meta[['terraclim']] = read_tds('http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html', "terraclim") %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, NA, NA), sep = "_", remove = FALSE) %>%
  mutate(URL = glue("{base}{link}.nc", base = baseNorthwestern),
         link = NULL) %>%
  variable_meta() %>%
  time_meta() %>%
  grid_meta()


######

meta[['vic']]= read_tds(url = 'https://www.reacchpna.org/thredds/nw.csc.hydrology-vic.aggregated.html', "vic") %>%
  tidyr::separate(link,  into = c(NA, 'variable', 'model', 'ensemble', 'scenario', NA, NA, NA, NA, NA, NA), sep = "_", remove = FALSE) %>%
  mutate(URL = glue("{base}{link}.nc", base = basePNA),
         link = NULL) %>%
  variable_meta() %>%
  time_meta() %>%
  grid_meta()

######

meta[['bcca']] = bind_rows(read_agg(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future',     id = 'bcca'),
                 read_agg(URL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical', id = 'bcca')) %>%
  tidyr::separate(link,  into = c(NA, NA, 'variable', NA, 'model', 'scenario', 'ensemble'), sep = "_", remove = FALSE) %>%
  mutate(URL = glue("{URL}?{link}"),
         link = NULL) %>%
  variable_meta()


meta[['bcsd']] = read_agg('https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC', id = "bcsd_vic") %>%
 tidyr::separate(link, into = c("model",  'scenario',  'ensemble', "variable"), sep = "_", extra = "merge", remove = FALSE) %>%
 mutate(URL = glue("{URL}?{link}")) %>%
 variable_meta()

meta[['daymet']] = read_agg('https://thredds.daac.ornl.gov/thredds/dodsC/daymet-v4-agg/na.ncml', "daymet") %>%
  mutate(URL = glue("{URL}?{link}"),
         link = NULL) %>%
  rename(variable = link) %>%
  variable_meta()


meta[['topowx']] = bind_rows(read_agg(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx',     id = 'topowx_day'),
                             read_agg(URL = 'https://cida.usgs.gov/thredds/dodsC/topowx_monthly', id = 'topowx_month')) %>%
  mutate(URL = glue("{URL}?{link}")) %>%
  rename(variable = link) %>%
  variable_meta()


meta[['dcp']] = read_agg(URL = 'https://cida.usgs.gov/thredds/dodsC/dcp/conus_t',  id = 'dcp') %>%
  tidyr::separate(link, into = c("model",  'scenario', "variable", NA, NA), sep = "-", extra = "merge", remove = FALSE) %>%
  mutate(URL = glue("{URL}?{link}"),
         link = NULL) %>%
  variable_meta()

meta[['maurer']] = read_agg(URL = 'https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml',  id = 'maurer') %>%
  tidyr::separate(link, into = c('scenario', "model", "ensemble", "variable"), sep = "_", extra = "merge", remove = FALSE) %>%
  mutate(URL = glue("{URL}?{link}"),
         link = NULL) %>%
  variable_meta()


meta[['prism']] = read_agg('http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2021/PRISM_combo_20211010.nc', 'prism') %>%
  mutate(URL = glue("{base}prism/daily/combo/2021/PRISM_combo_20211231.nc?{link}", base = baseNCSU)) %>%
  rename(variable = link) %>%
  mutate(startDate = as.POSIXct('1981-01-01'), endDate = as.POSIXct('2021-12-31') ) %>%
  variable_meta() %>%
  mutate(URL = glue("{base}prism/daily/combo/{{YYYY}}/PRISM_combo_{{YYYYMMDD}}.nc?{variable}", base = baseNCSU) )


names(meta)


meta_all  = bind_rows(meta) %>%
  select(-link)

usethis::use_data(meta_all,  overwrite = TRUE)




#####
get_predefined = function(id,
                          AOI,
                          variable,
                          model = NULL,
                          ensemble = NULL,
                          scenario = NULL,
                          startDate,
                          endDate = NULL
                          ){

  meta = get_dap(id, param = variable, model, ensemble)


  space = .local_reference_extent(AOI, list(proj = meta$proj[1], ext = meta$ext[1][[1]], diminsion = meta$diminsion[1][[1]]))

  time = .local_time_extent(startDate, endDate, list(Tmin = meta$startDate, Tmax = meta$endDate, nT = meta$nT))



  req = data.frame(url = glue("{base}?{var}{T}{Y}{X}",
                              base = dd$URL,
                              var = dd$varname,
                              T = time$T,
                              Y = space$Y,
                              X = space$X),
                   varname = dd$varname)

  get_OpenDap(req, space, time)
}




system.time({
  out = get_predefined(id = "maca_day",
                       AOI,
                       startDate = "2000-01-01",
                       endDate = "2000-01-31",
                       var = "huss")
})

system.time({
  out2 = getdap(url = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_huss_BNU-ESM_r1i1p1_historical_1950_2005_CONUS_daily.nc',
                AOI,
                startDate = "2000-01-01",
                endDate = "2000-01-31",
                var = NULL)
})


system.time({
  out2 = getMACA2(AOI,
                  param = "pr",
                  model = "BNU-ESM",
                  startDate = "2000-01-01",
                  endDate   = "2000-01-31")
})
# URL data collection
# ref_grid (ext, proj, dims)
# time bounds and step
# variable list



