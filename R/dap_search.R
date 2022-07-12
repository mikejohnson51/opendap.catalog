
rm(list = ls())

library(opendap.catalog)
library(dplyr)
library(terra)
library(sf)

# Source functions for creating and comparing grids in opendap.catalog::grids to input AOI
source("R/search_utils.R")

# AOI
shp <- AOI::aoi_get(state = "FL")

# Opendap grids, remove NA grid IDs, NA projections, and X1 values of 0
grid_df <-
  opendap.catalog::grids %>%
  dplyr::filter(is.na(grid_id), !is.na(proj), X1 != 0)

length(unique(opendap.catalog::grids$grid_id))
length(unique(grid_df$grid_id))

# test AOI
shp <- AOI::aoi_get(state = "CO")

# Returns Grid IDs for AOI
gridsub <- grid_subset(
  aoi        = shp,
  grid_index = grid_df
)

# Grid IDs within AOI
gridsub

# Filter grid IDs to AOI
opendap.catalog::grids[opendap.catalog::grids$grid_id %in% gsub, ]

grid_subset <- function(aoi, grid_index) {

  # aoi <- shp
  # grid_index <- grid_df
    if(methods::is(aoi, 'bbox')){
      logger::log_info("AOI is bbox")
      aoi = sf::st_as_sfc(aoi)
    }

    if(methods::is(aoi, 'sp')){
      logger::log_info("AOI is sp")
      aoi = sf::st_as_sf(aoi)
    }

    if(methods::is(aoi, 'sfc')){
      logger::log_info("AOI is sfc")
      aoi = sf::st_as_sf(aoi)
    }

    if(any(sf::st_geometry_type(aoi) == "POINT" & nrow(aoi) > 1, is.null(nrow(aoi)))){
      logger::log_info("AOI is point/null")
      aoi = sf::st_as_sfc(sf::st_bbox(aoi))
    }

    # split grids into list
    bb_index <-
      grid_index %>%
      base::split(grid_index, f = grid_index$grid_id)


    logger::log_info("Subsetting grids to AOI bounding box...")

    # create grid for each grid ID, compare to AOI, and output grid IDs of grids intersecting input AOI
    grid_select <- unname(
              unlist(
                lapply(bb_index, FUN = function(y) {
                  fetch_grid(aoi = aoi, grid_data = y) %>%
                    sf::st_as_sf() %>%
                    dplyr::mutate(
                      grid_id = y$grid_id,
                      proj    = y$proj
                      ) %>%
                    spatial_subset(aoi = aoi, grid_domain = .)
                  }
                  )
                )
              )

    return(grid_select)
}


# tmp <-
#   grid_index %>%
#   dplyr::group_by(grid_id) %>%
#   dplyr::filter(!is.na(grid_id), !is.na(proj))

# Create grid from opendap.catalog::grids
fetch_grid <- function(aoi, grid_data) {

  # X Coordinates
  xcoords <- seq(
    grid_data$X1,
    grid_data$Xn,
    length.out = grid_data$ncols
    # by = grid_data$resX
  )

  # grid_data$proj

  # # Check x coords
  # if(any(xcoords > 180.001)) {
  #   xcoords = xcoords - 360
  #   }


  if(grid_data$toptobottom == TRUE | is.na(grid_data$toptobottom)) {

    logger::log_info("Top to Bottom")

    # Y Coordinates
    ycoords <- seq(
      grid_data$Y1,
      grid_data$Yn,
      length.out = grid_data$nrows
      # by = grid_data$resY
    )

  } else if(grid_data$toptobottom == FALSE) {

    logger::log_info("Bottom to Top")

    # Y Coordinates
    ycoords <- seq(
      grid_data$Yn,
      grid_data$Y1,
      length.out = grid_data$nrows
      # by = grid_data$resY
    )

  }

  domain <-
    sf::st_as_sfc(
      sf::st_bbox(
        c(
          xmin = min(xcoords),
          xmax = max(xcoords),
          ymin = min(ycoords),
          ymax = max(ycoords)
        ),
        # crs =  sf::st_crs(bb)
        crs =  sf::st_crs(grid_data$proj)
      )
    ) %>%
    sf::st_transform(sf::st_crs(aoi)) %>%
    sf::st_as_sf()

  # mapview::mapview(domain, col.regions = "red")

  return(domain)

}
# Check grids for intersection
spatial_subset <- function(aoi, grid_domain) {

  aoi_bb   <-
    aoi %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf() %>%
    sf::st_intersects(grid_domain)

  if(length(aoi_bb[[1]]) == 1) {
    logger::log_info("\n\nAOI intersects grid_id: {grid_domain$grid_id}")

    # gid <- tibble::tibble(
    #   grid_id        = grid_domain$grid_id,
    #   intersects_aoi = TRUE
    #   )
    # return(gid)
    return(grid_domain$grid_id)

  } else if(length(aoi_bb[[1]]) == 0) {

    logger::log_info("\n\nNo intersection with grid_id: {grid_domain$grid_id}")

    # gid <- tibble::tibble(
    #   grid_id        = grid_domain$grid_id,
    #   # grid_id        = bb_df[[153]]$grid_id,
    #   intersects_aoi = FALSE )
    # return(gid)
    return(NULL)
  }

}
# rm(grid_data, grid_bb, overlap, gid, bb_index, bb_df, bb_df2, domain, AOI, aoi_bb, grid, grid_index, aoi_t, bb)
# cross

# grid create grid bb and assign grid ID name as column
bb_df <- lapply(bb_index, FUN = function(y) {
  fetch_grid(aoi = AOI, grid_data = y) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(
      grid_id = y$grid_id,
      proj    = y$proj
      ) %>%
    spatial_subset(aoi = AOI, grid_domain = .)
  }
  )

spatial_subset(aoi = AOI, grid_domain = bb_df[[1]])
grid_subs <- lapply(bb_df, FUN = function(y){
  spatial_subset(aoi = AOI, grid_domain = y)
})
unlist(grid_subs)
# %>%
  # dplyr::bind_rows()
bb_df$x[[1]]
bb_df[1]
aoi_bb <-
  AOI %>%
  st_bbox() %>%
  st_as_sfc()

bb_df$x[[1]]
aoi_bb[[1]]
aoi_bb %>%
  st_transform(st_crs(bb_df[1,]))
# mapview::mapview(bb) + AOI
X_coords <- seq(
  tmp_grid$X1,
  tmp_grid$Xn,
  by = tmp_grid$resX
  )

if(any(X_coords > 180.001)){X_coords = X_coords - 360}

Y_coords <- seq(
  tmp_grid$Y1,
  tmp_grid$Yn,
  by = tmp_grid$resY
  )

domain <-
  sf::st_as_sfc(
    sf::st_bbox(
      c(
        xmin = min(X_coords),
        xmax = max(X_coords),
        ymin = min(Y_coords),
        ymax = max(Y_coords)
        ),
      crs  = sf::st_crs(bb)
      )
    )
mapview::mapview(domain)
xxx = sf::sf_use_s2()
sf::sf_use_s2(FALSE)
dap <- dap(
  URL       = "https://cida.usgs.gov/thredds/dodsC/bcsd_obs",
  AOI       = AOI::aoi_get(state = "FL"),
  startDate = "1995-01-01"
  )


bb <- sf::st_bbox(shp)
new_crs <- 3995
bb_trans <- lapply(na.omit(grid_index$proj), FUN = function(y) {

  bb_new <-
    bb %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs = y) %>%
    sf::st_bbox() %>%
    tibble::tibble()
}
)
dplyr::bind_rows(bb_trans)

bb_new <-
  bb %>%
  st_as_sfc() %>%
  st_transform(crs = new_crs) %>%
  st_bbox()

query = NULL
source = NULL
if (!is.null(source)) {
  x <- opendap.catalog::params[opendap.catalog::params$id == source, ]
} else {
  x <- opendap.catalog::params
}


# stringdist::
dap
grid_index <- opendap.catalog::grids

# cat <- opendap.catalog::search()
# x <- opendap.catalog::params[opendap.catalog::params$id == source, ]

if (!is.null(source)) {
  x <- opendap.catalog::params[opendap.catalog::params$id == source, ]
} else {
  x <- opendap.catalog::params
}

subs <- x[, !names(x) %in% c("grid_id", "URL", "tiled", "units", "T_name", "nT", "duration")]
if (!is.null(query)) {
  .query(x, query, subs)
} else {
  x
}

query_txt <- "daily prcp"

splits <- split(
  subs,
  seq(nrow(x))
  )

q <- strsplit(query_txt, " ")[[1]]
qrow <- c(as.character(splits[[1]]))

indices <- unlist(
  lapply(splits, function(y) {

    m <- adist(q,
               # y,
               splits[[1]],
               ignore.case = TRUE,
               partial     = TRUE
               )


  sum(apply(m, 1, min, na.rm = TRUE))
}))
qsub <-
  subs %>%
  dplyr::mutate(
    query_txt = "prcp"
    # across(everything(), ~ adist(.x, query_txt), .names = "{col}_dist")
  )
query_df <- tibble::tibble(query_txt = qsub$query_txt)
qsub2 <- qsub %>%
  dplyr::select(id:long_name,)
joined <- qsub2 %>%
  stringdist_inner_join(query_df, by = c(long_name = "query_txt"))
fsub <- fuzzyjoin::fuzzy_left_join()
parm <- opendap.catalog::params

# as.character(subs[,])
fmatch <- sapply(qsub$long_name,
       function(x) {
         agrep(query_txt, x, value = TRUE)
       })
# qsub <-
#   subs %>%
#   dplyr::mutate(
#     across(everything(), ~ adist(.x, query_txt), .names = "{col}_dist")
#                 )
subs

query <- gsub("daily", "day", query_txt)
query <- gsub("monthly", "month", query_txt)
query <- gsub("hourly", "hour", query_txt)

if (is.null(subs)) {
  subs <- x
}

splits <- split(subs, seq(nrow(x)))

q <- strsplit(query, " ")[[1]]
y2 <- splits[[2]]
indices <- unlist(lapply(splits, function(y) {
  m <- adist(q,
             y,
             ignore.case = TRUE,
             partial     = TRUE
  )


  sum(apply(m, 1, min, na.rm = TRUE))
}))

x$rank <- indices

if (min(indices) > length(q) * 3) {
  warning("No likely matches found.")
}

x <- x[x$rank == min(indices), ]

x[order(x$rank), ]












