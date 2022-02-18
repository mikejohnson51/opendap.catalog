library(dplyr)

dat = read_html('https://opendap.cr.usgs.gov/opendap/hyrax/') |>
  html_nodes("a")

dat2 = data.frame(link = html_attr(dat, "href")) |>
  mutate(id = dirname(link),
         link = paste0('https://opendap.cr.usgs.gov/opendap/hyrax/', gsub('contents.html', "", link)),
         tmp = paste0(link, 'h00v08.ncml')) |>
  filter(grepl("MOD", id))

modis = lapply(1:nrow(dat2), function(x) {
  tryCatch({
    nc   = RNetCDF::open.nc(dat2$tmp[x])
    raw = dap_xyzv(nc, varmeta = TRUE)
    raw$id  = dat2$id[x]

    merge(raw,  data.frame(.resource_time(nc, raw$T_name[1]), id = dat2$id[x]) , by = 'id')
    },
    error = function(e){NULL})
  })

modis_params = bind_rows(modis) |>
  mutate(URL = paste0('https://opendap.cr.usgs.gov/opendap/hyrax/', id),
         tiled = "XY",
         grid.id = "modis")

saveRDS(modis_params, "modis_param.RDS")

tds = read_tds(URL = 'https://opendap.cr.usgs.gov/opendap/hyrax/MOD16A2.006/', 'mod16') %>%
  filter(grepl("ncml", link)) %>%
  mutate(link = gsub("(ncml).*","\\1", basename(link)), URL = NULL) %>%
  group_by(link) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(URL = paste0('https://opendap.cr.usgs.gov/opendap/hyrax/MOD16A2.006/', link))

modis_grid = lapply(1:nrow(tds), function(x){
  nc   = RNetCDF::open.nc(tds$URL[x])
  raw = .resource_grid(nc, X_name = "XDim", Y_name = "YDim")
  raw$X_name = 'XDim'
  raw$Y_name = "YDim"
  raw$tile = gsub(".ncml",  "", tds$link[x])
  raw
})


modis_grids2 = modis_grid %>%
  bind_rows() |>
  mutate(grid.id = "modis")

saveRDS(modis_grids2, 'modis_grids.rds')
