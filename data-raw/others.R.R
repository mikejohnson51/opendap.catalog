meta <- list()

library(opendap.catalog)
library(dplyr)
library(rvest)

read_tds <- function(URL, base = 'https://thredds.daac.ornl.gov/thredds/dodsC/', id = NA) {
  dat <- read_html(URL)
  dat <- html_nodes(dat, "a")
  dat <- data.frame(link = html_attr(dat, "href"), id = id)

  dat$link <- gsub(".*=", "", dat$link)

  dat$URL <- paste0(base, dat$link)

  dat[!grepl("http|https|html", dat$link), ]
  dat[grepl("ncml", dat$link), ]
}


dm = read_tds(
  URL = 'https://thredds.daac.ornl.gov/thredds/catalog/daymet-v4-agg/catalog.html',
  id = 'daymet4'
)

meta[["daymet4"]] = lapply(1:nrow(dm), function(x){ dap_meta(read_dap_file(dm$URL[x], "daymet4"))}) |>
  bind_rows() |>
  mutate(tiled = "")


meta[['prism_daily']] = read_dap_file(URL = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2021/PRISM_combo_20211010.nc', id = 'prism') %>%
  mutate(duration = '1981-01-01/..', variable = varname) %>%
  dap_meta() %>%
  mutate(URL =   gsub("20211010", "{YYYYMMDD}", gsub("/2021\\/", "{YYYY}", URL)), tiled = "T")



saveRDS(bind_rows(meta), "data-raw/others.rds")


