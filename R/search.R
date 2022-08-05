#' @title Summarize Search Results
#' @param x catalog data.frame
#' @return data.frame
#' @export

search_summary <- function(x) {
  res <- by(x, list(x$id, x$varname), function(i) {
    c(
      id = unique(i$id),
      long_name = unique(i$long_name),
      variable = unique(i$varname),
      count = nrow(i)
    )
  })

  data.frame(do.call(rbind, res))
}

#' @title Search Catalog
#' @description Search the data catalog to find an ideal source.
#' @param AOI SF object for spatial search
#' @param query a string to search for.
#' @param source If the source id you want is known, provide it to expedite search
#' @details query should be passed as a string or list of key word. If a single string is passed, it will be split at spaces.
#' @return data.frame
#' @export

search <- function(AOI = NULL, query = NULL, source = NULL) {
  if (!is.null(source)) {
    x <- opendap.catalog::params[opendap.catalog::params$id == source, ]
  } else {
    x <- opendap.catalog::params
  }

  if (!is.null(AOI)) {

    # identify grid IDs for AOI
    id_subs <- grid_subset(AOI = AOI)

    if(!is.null(id_subs)) {

      # subset x to relevant grids
      x <- x[x$grid_id %in% id_subs, ]

    } else {
      x
    }

  } else {
    x
  }

  subs <- x[, !names(x) %in% c("grid_id", "URL", "tiled", "units", "T_name", "nT", "duration")]

  if (!is.null(query)) {
    .query(x, query, subs)
  } else {
    x
  }

}

#' @title Internal query
#' @param x catalog data.frame
#' @param query User query
#' @param subs subset of x by column (optional)
#' @return data.frame
#' @export
#' @importFrom utils adist

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

#' @title Creates a grid from grid parameters found in opendap.catalog::grids
#' @description Creates a grid from grid parameters found in opendap.catalog::grids and then projects the grid to the CRS of input AOI. Internal helper function for the grid_subset function and is used in conjunction with the spatial_subset function.
#' @param AOI SF object
#' @param grid_data catalog data.frame from calling opendap.catalog::grids
#' @details grid_data parameter is built to work on a single row of data from opendap.catalog::grids
#' @return data.frame
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom sf st_bbox st_as_sfc st_crs st_transform st_as_sf

fetch_grid <- function(AOI, grid_data) {


  # X Coordinates
  xcoords <- seq(
    grid_data$X1,
    grid_data$Xn,
    length.out = grid_data$ncols
  )

  # # Check x coords
  # if(any(xcoords > 180.001)) { xcoords = xcoords - 360}

  if(grid_data$toptobottom == TRUE | is.na(grid_data$toptobottom)) {

    # Grids going from "top to bottom"

    # Y Coordinates
    ycoords <- seq(
      grid_data$Y1,
      grid_data$Yn,
      length.out = grid_data$nrows # by = grid_data$resY
    )

  } else if(grid_data$toptobottom == FALSE) {

    # Grids going from "Bottom to Top"

    # Y Coordinates
    ycoords <- seq(
      grid_data$Yn,
      grid_data$Y1,
      length.out = grid_data$nrows # by = grid_data$resY
    )

  }

  # Turn off spherical geometry (s2)
  sf::sf_use_s2(use_s2 = FALSE)

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
    sf::st_transform(sf::st_crs(AOI)) %>%
    sf::st_as_sf()

  return(domain)

}

#' @title Check grids for intersection with AOI - Internal use
#' @description Compares an input AOI and a grid created from the fetch_grid function and returns the grid ID number of any overlapping grids. Internal helper function for the grid_subset function and is used in conjunction with the fetch_grid function.
#' @param AOI SF object
#' @param grid_domain SF object representing spatial domain of grid, output of fetch_grid function
#' @details Function is designed to take an AOI and compares it to the outputs from fetch_grid, returns the intersecting grid ID number as a character, and NULL if no intersection
#' @return character
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom sf st_bbox st_as_sfc st_as_sf st_intersects

spatial_subset <- function(AOI, grid_domain) {

  aoi_bb   <-
    AOI %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf() %>%
    sf::st_intersects(grid_domain)

  if(length(aoi_bb[[1]]) == 1) {

    # return Grid ID if AOI and grid intersect
    return(grid_domain$grid_id)

  } else if(length(aoi_bb[[1]]) == 0) {

    # return Grid ID if NO intesection between AOI and grid
    return(NULL)
  }

}

#' @title Check grids for intersection with AOI - Internal use
#' @description Function is designed to take an AOI and compares it to the spatial extents of the grids found in opendap.catalog::grids, the function returns a list of grid IDs for any grid interesecting the AOI - Internal use
#' @param AOI SF object
#' @details Internal use as a spatial query for search function
#' @return list
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter mutate
#' @importFrom sf st_as_sfc st_as_sf st_geometry_type st_bbox

grid_subset <- function(AOI) {

  # remove NA grid IDs, NA projections, and grids w/ 0 in X1 column
  grid_index <-
    opendap.catalog::grids %>%
    dplyr::filter(!is.na(grid_id), !is.na(proj), X1 != 0)

  if(methods::is(AOI, 'bbox')){
    AOI = sf::st_as_sfc(AOI)
  }

  if(methods::is(AOI, 'sp')){
    AOI = sf::st_as_sf(AOI)
  }

  if(methods::is(AOI, 'sfc')){
    AOI = sf::st_as_sf(AOI)
  }

  if(any(sf::st_geometry_type(AOI) == "POINT" & nrow(AOI) > 1, is.null(nrow(AOI)))){
    message("AOI is a point or NULL")
    AOI = sf::st_as_sfc(sf::st_bbox(AOI))
  }

  # split grids into list
  bb_index <-
    grid_index %>%
    base::split(grid_index, f = grid_index$grid_id)


  message("Subsetting grids to AOI bounding box...")

  # create grid for each grid ID, compare to AOI, and output grid IDs of grids intersecting input AOI
  grid_select <- unname(
    unlist(
      lapply(bb_index, FUN = function(y) {
        fetch_grid(AOI = AOI, grid_data = y) %>%
          sf::st_as_sf() %>%
          dplyr::mutate(
            grid_id = y$grid_id,
            proj    = y$proj
          ) %>%
          spatial_subset(AOI = AOI, grid_domain = .)
      }
      )
    )
  )

  return(grid_select)
}
