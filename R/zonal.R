#' Build Weighting Grid
#' @description  Returns a data.table with columns for ID, grid_id, X, Y and weight. By default this object is
#' sorted on the grid_id
#' @param file path to a gridded file (either .tif or .nc), or SpatRast object
#' @param AOI sf object of aggregation units
#' @param ID the name of the column providing the unique identified of each geom
#' @return a list(data.table, vector)
#' @export
#' @importFrom terra rast
#' @importFrom exactextractr exact_extract
#' @importFrom data.table rbindlist

weighting_grid <- function(file, AOI, ID) {
  if (!inherits(file, "SpatRast")) {
    file <- terra::rast(file)
  }

  w = exact_extract(file[[1]],
    AOI,
    fun = NULL,
    include_cols = ID,
    progress = FALSE,
    include_cell = TRUE
  ) |>
    rbindlist()

  w$value = NULL

  w
}

#' Materialize Grid from File or inputs
#' @description  Returns an empty SpatRast obj with prescibed properties
#' @param file path to a gridded file (either .tif or .nc), or SpatRast object
#' @param ext extent (xmin, xmax, ymin, ymax) in some coordinate system
#' @param diminsion dimension (number of columns, number of rows)
#' @param projection projection - the actual coordinate system
#' @return SpatRaster object
#' @export
#' @importFrom terra rast ext ncol nrow crs
#' @importFrom exactextractr exact_extract
#' @importFrom data.table rbindlist

materilize_grid = function (file = NULL, ext = NULL, diminsion = NULL, projection = NULL) {
  if (!is.null(file)) {
    r = suppressWarnings({ terra::rast(file) })

    ext = terra::ext(r)
    diminsion = c(terra::ncol(r), terra::nrow(r))
    projection = terra::crs(r)

  } else {
    if (is.null(projection)) {
      stop("prj is required")
    }
    if (is.null(ext)) {
      stop("ext is required")
    }
    if (is.null(diminsion)) {
      stop("diminsion")
    }
}

  terra::rast(ext, nrows = diminsion[2], ncols = diminsion[1], crs = projection)

}
