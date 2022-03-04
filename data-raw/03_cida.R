# CIDA
library(dplyr)
library(tidyr)
library(opendap.catalog)

meta <- list()

####  LOCA ####
meta[["loca"]] <- bind_rows(
  read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_historical", id = "loca"),
  read_dap_file("https://cida.usgs.gov/thredds/dodsC/loca_future", id = "loca")
) %>%
  tidyr::separate(varname, into = c("variable", "model", "ensemble", "scenario"), sep = "_", remove = FALSE) %>%
  mutate(tiled = "T")

####  BCCA ####
meta[["bcca"]] <- bind_rows(
  read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/future", id = "bcca"),
  read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical", id = "bcca")
) %>%
  tidyr::separate(varname, into = c(NA, NA, "variable", NA, "model", "scenario", "ensemble"), sep = "_") %>%
  dap_meta() %>%
  mutate(tiled = "T")

####  BCSD VIC ####
meta[["bcsd_vic"]] <- read_dap_file("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC", id = "bcsd_vic") %>%
  tidyr::separate(varname, into = c("model", "scenario", "ensemble", "variable"), sep = "_", extra = "merge") %>%
  dap_meta() %>%
  mutate(tiled = "")

####  BCSD ####
meta[["bcsd"]] <- read_dap_file("https://cida.usgs.gov/thredds/dodsC/bcsd_obs", id = "bcsd") %>%
  dap_meta() %>%
  mutate(tiled = "")

#### DCP ####
meta[["dcp"]] <- read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/dcp/conus_t", id = "dcp") %>%
  tidyr::separate(varname, into = c("model", "scenario", "variable", NA, NA), sep = "-", extra = "merge", remove = FALSE) %>%
  dap_meta() %>%
  mutate(tiled = "")

####  Maurer ####
meta[["maurer"]] <- read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/maurer/maurer_brekke_w_meta.ncml", id = "maurer") %>%
  tidyr::separate(varname, into = c("scenario", "model", "ensemble", "variable"), sep = "_", extra = "merge", remove = FALSE) %>%
  dap_meta() %>%
  mutate(tiled = "")

####  ssebopeta ####
meta[["ssebopeta"]] <- read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly", id = "ssebopeta") %>%
  dap_meta() %>%
  mutate(tiled = "")

####  PRISM Monthly ####
meta[["prism_monthly"]] <- read_dap_file(URL = "https://cida.usgs.gov/thredds/dodsC/prism_v2", id = "prism_month") %>%
  mutate(variable = varname) %>%
  dap_meta() %>%
  mutate(tiled = "")

saveRDS(bind_rows(meta), "data-raw/cida.rds")
