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

##### WRITE!
saveRDS(bind_rows(meta), "data-raw/tds.rds")
