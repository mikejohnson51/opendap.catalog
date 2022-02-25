# Setup -------------------------------------------------------------------
library(opendap.catalog)
library(dplyr)
library(tidyr)

grid_vars = c("grid.id", 'X_name', 'Y_name', 'X1', 'Xn', 'Y1', 'Yn', 'resX', 'resY', 'ncols', 'nrows', 'proj', "toptobottom")

param_vars = c('id', 'grid.id',  'URL', 'tiled',
               'variable', 'varname', 'long_name', 'units',
               'model', 'ensemble', 'scenario',
               "T_name", 'duration', 'interval', "nT")

split_grids = function(raw){
  raw |>
    group_by(grid.id) |>
    slice(1) |>
    ungroup() |>
    select(!!grid_vars)
}



rds = list.files('data-raw', pattern = ".rds", full.names = TRUE)
RDS = list.files('data-raw', pattern = ".RDS", full.names = TRUE)


raw = bind_rows(lapply(rds, readRDS))

raw = raw |>
  group_by(X1, Xn, Y1,Yn, resX, resY, ncols, nrows, toptobottom) |>
  fill(proj) %>%
  fill(proj, .direction = "up") |>
  mutate(grid.id = as.character(cur_group_id())) |>
  ungroup()

grids =  raw |>
  group_by(grid.id) |>
  slice(1) |>
  ungroup() |>
  select(!!grid_vars) |>
  bind_rows(readRDS("data-raw/modis_grids.RDS"))

jsonlite::write_json(grids, "docs/cat_grids.json", pretty = TRUE)
usethis::use_data(grids, overwrite = TRUE)

params = raw |>
  bind_rows(mutate(readRDS("data-raw/modis_param.RDS"))) |>
  select(param_vars)

jsonlite::write_json(params, "docs/cat_params.json", pretty = TRUE)
usethis::use_data(params, overwrite = TRUE)
