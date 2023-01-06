library(rvest)
library(dplyr)
library(tidyr)
library(opendap.catalog)

read_tds <- function(URL, id, append = ".nc") {
  dat <- read_html(URL)
  dat <- html_nodes(dat, "a")
  dat <- data.frame(link = html_attr(dat, "href"), id = id)

  dat$link <- gsub(".*=", "", dat$link)

  dat$URL <- paste0(dirname(URL), "/dodsC/", dat$link, append)

  dat[!grepl("http|https|html", dat$link), ]
}

meta <- list()
####  MACA ####
meta[["maca"]] <- bind_rows(
  read_tds("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_catalog.html", "maca_day"),
  read_tds("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_CMIP5_aggregated_macav2_monthly_catalog.html", "maca_month")
) %>%
  tidyr::separate(link,
    into = c(NA, NA, "variable", "model", "ensemble", "scenario", NA, NA, NA, NA),
    sep = "_"
  ) %>%
  dap_meta() %>%
  mutate(tiled = "T")


####  GridMET ####
meta[["gridmet"]] <- read_tds("http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html", "gridmet") %>%
  tidyr::separate(link,
    into = c(NA, NA, "variable", NA, NA, NA),
    sep = "_") |>
  dap_meta() |>
  mutate(tiled = "")


####  TerraClim ####
meta[["terraclim"]] <- read_tds("http://thredds.northwestknowledge.net:8080/thredds/terraclimate_aggregated.html", "terraclim") %>%
  tidyr::separate(link, into = c(NA, NA, "variable", NA, NA, NA), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "")


meta[["terraclim_normals"]]  = read_tds('http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/summaries/catalog.html',
         "terraclim_normals", "") |>
  mutate(URL = paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/TERRACLIMATE_ALL/summaries/', basename(link))) |>
  mutate(link2 = gsub(".nc", "", basename(link))) %>%
  filter(link2 != "summaries") |>
  tidyr::separate(link2, into = c("scenario", 'variable'), sep = "_") %>%
  mutate(scenario = gsub("TerraClimate", "", scenario)) %>%
  filter(!is.na(variable)) |>
  dap_meta() |>
  mutate(tiled = "", interval = "monthly normal")


####  VIC ####
meta[["vic"]] <- read_tds(URL = "https://www.reacchpna.org/thredds/nw.csc.hydrology-vic.aggregated.html", id = "vic") %>%
  tidyr::separate(link, into = c(NA, "variable", "model", "ensemble", "scenario", NA, NA, NA, NA, NA, NA), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "")


#### bcsd-nmme/daily ####
meta[["bcsd-nmme-daily"]] = read_tds(URL = "http://thredds.northwestknowledge.net:8080/thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/dailyForecasts/catalog.html", id = "bcsd-nmme-daily") %>%
  filter(grepl(".nc", link)) |>
  mutate(link2 = gsub("NWCSC_IS_ALL_SCAN/bcsd-nmme/dailyForecasts/bcsd_nmme_metdata_", "", link),
         URL = paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/dailyForecasts/bcsd_nmme_metdata_',
                      link2),
         link2 = gsub(".nc", "", link2)) |>
  tidyr::separate(link2, into = c("model", "scenario", 'variable', NA), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "", duration = "../..", link = NULL, link2 = NULL, scenario  = "6 month forecast")

read_tds(URL = "http://thredds.northwestknowledge.net:8080/thredds/catalog/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/monthlyForecasts/catalog.html", id = "bcsd-nmme-monthly") %>%
  filter(grepl(".nc", link)) |>
  mutate(link2 = gsub("NWCSC_IS_ALL_SCAN/bcsd-nmme/monthlyForecasts/bcsd_nmme_metdata_", "", link),
         URL = paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/monthlyForecasts/bcsd_nmme_metdata_',
                      link2),
         link2 = gsub(".nc", "", link2)) |>
  tidyr::separate(link2, into = c("model", NA, "scenario"), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "", duration = "../..", link = NULL, link2 = NULL, scenario  = "6 month forecast")

URL = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/bcsd-nmme/monthlyForecasts/bcsd_nmme_metdata_ENSMEAN_forecast_4monthAverage.nc'
id = "tmp"
nc <- RNetCDF::open.nc(URL)
on.exit(close.nc(nc))
raw <- dap_xyzv(nc, varmeta = TRUE)
raw$URL <- URL
raw$id <- id
raw <- merge(raw, data.frame(.resource_time(nc, raw$T_name[1]), id = id), by = "id")
raw <- merge(raw, .resource_grid(nc, X_name = raw$X_name[1], Y_name = raw$Y_name[1]))
raw

read_dap_file
##### WRITE!
saveRDS(bind_rows(meta), "data-raw/tds.rds")
