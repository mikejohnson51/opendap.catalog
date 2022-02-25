library(dplyr)
library(rvest)

dat = read_html('https://opendap.cr.usgs.gov/opendap/hyrax/') |>
  html_nodes("a")

dat2 = data.frame(link = html_attr(dat, "href")) |>
  mutate(id = dirname(link),
         link = paste0('https://opendap.cr.usgs.gov/opendap/hyrax/', gsub('contents.html', "", link))
        # tmp = paste0(link, 'h00v08.ncml'
        ) |>
  filter(!grepl("http|4913|opendap|PROTOTYPE", id)) |>
  filter(id != ".") |>
  filter(grepl("MOD", id))

dat3 = list()

for(i in 1:nrow(dat2)){

  tmp = tryCatch({
    data.frame(link = html_attr(html_nodes(read_html(dat2$link[i]), "a"), "href"))
  }, error = function(e){ NULL })

  if(!is.null(tmp)){

    dat3[[i]] = data.frame(link = html_attr(html_nodes(read_html(dat2$link[i]), "a"), "href")) |>
      filter(grepl("datasetID", link)) |>
      mutate(id = sub('.+datasetID=/(.+)', '\\1', link)) |>
      tidyr::separate(id, into = c("id", "tile"), sep = "/") |>
      mutate(tile = gsub(".ncml", "", tile), link = dat2$link[i])
  } else {
    dat3[[i]] = NULL
  }

  message(i)
}

dat4 = bind_rows(dat3) |>
  group_by(id) |>
  mutate(mosaic = n(), tiled = ifelse(mosaic > 1, "XY_modis", "")) |>
  slice(1) |>
  ungroup() |>
  mutate(tmp = paste0(link, tile, ".ncml#fillmismatch"))

modis = list()

for(i in 1:nrow(dat4)){

  modis[[i]] = tryCatch({

    nc =  RNetCDF::open.nc(dat4$tmp[i])
    raw = dap_xyzv(obj = nc, varmeta = TRUE)
    raw$id  = dat4$id[i]
    raw$tiled = dat4$tiled[i]

    merge(raw,  data.frame(.resource_time(nc, raw$T_name[1]), id = dat4$id[i]) , by = 'id')
  },
  error = function(e){NULL})
    message(i)
}

modis_params = bind_rows(modis) |>
  mutate(URL = paste0('https://opendap.cr.usgs.gov/opendap/hyrax/', id),
         grid.id = " XY_modis")

## internal syntax capital RDS is for those files that have an XY tiling
# used when listing files :)
saveRDS(modis_params, "data-raw/modis_param.RDS")

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
  mutate(grid.id = "XY_modis")

saveRDS(modis_grids2, 'data-raw/modis_grids.RDS')
