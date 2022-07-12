# Create grid from opendap.catalog::grids
fetch_grid <- function(aoi, grid_data) {

  # X Coordinates
  xcoords <- seq(
    grid_data$X1,
    grid_data$Xn,
    length.out = grid_data$ncols # by = grid_data$resX
  )

  # # Check x coords
  # if(any(xcoords > 180.001)) { xcoords = xcoords - 360}

  if(grid_data$toptobottom == TRUE | is.na(grid_data$toptobottom)) {

    # logger::log_info("Top to Bottom")

    # Y Coordinates
    ycoords <- seq(
      grid_data$Y1,
      grid_data$Yn,
      length.out = grid_data$nrows # by = grid_data$resY
    )

  } else if(grid_data$toptobottom == FALSE) {

    # logger::log_info("Bottom to Top")

    # Y Coordinates
    ycoords <- seq(
      grid_data$Yn,
      grid_data$Y1,
      length.out = grid_data$nrows # by = grid_data$resY
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
        crs =  sf::st_crs(grid_data$proj)
      )
    ) %>%
    sf::st_transform(sf::st_crs(aoi)) %>%
    sf::st_as_sf()

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

    # return Grid ID if AOI and grid intersect
    return(grid_domain$grid_id)

    # logger::log_info("\n\nAOI intersects grid_id: {grid_domain$grid_id}")
    # gid <- tibble::tibble( grid_id = grid_domain$grid_id, intersects_aoi = TRUE )
    # return(gid)

  } else if(length(aoi_bb[[1]]) == 0) {

    # return Grid ID if NO intesection between AOI and grid
    return(NULL)

    # logger::log_info("\n\nNo intersection with grid_id: {grid_domain$grid_id}")
    # gid <- tibble::tibble(grid_id = grid_domain$grid_id,# intersects_aoi = FALSE)
    # return(gid)
  }

}

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

search <- function(query = NULL, source = NULL) {
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
}

.query <- function(x, query, subs = NULL) {
  query <- gsub("daily", "day", query)
  query <- gsub("monthly", "month", query)
  query <- gsub("hourly", "hour", query)

  if (is.null(subs)) {
    subs <- x
  }

  splits <- split(subs, seq(nrow(x)))

  q <- strsplit(query, " ")[[1]]

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
}
