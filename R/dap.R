
#' @title Get DAP
#' @description Define and get data from a DAP resource
#' @param URL local file path or dodC URL
#' @param catalog subset of open.dap catalog
#' @param AOI sf object
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate  end date (YYYY- MM-DD)
#' @param varname  name of variable to extract. If NULL, then get all
#' @param verbose  Should dap_summary be printed?
#' @details Wraps dap_get and dap_crop into one.
#' If AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @export
#' @importFrom terra vect intersect ext project

dap <- function(URL = NULL,
                catalog = NULL,
                AOI = NULL,
                startDate = NULL,
                endDate = NULL,
                varname = NULL,
                verbose = TRUE) {

  urls = c(URL, catalog$URL)

  if(any(getExtension(urls) %in% c('vrt', "tif"))){
    vrt_crop_get(urls, catalog, AOI)
  } else {
    dap <- dap_crop(
      URL = URL,
      catalog = catalog,
      AOI = AOI,
      startDate = startDate,
      endDate = endDate,
      varname = varname,
      verbose = verbose)

    dap_get(dap)

  }

}

vrt_crop_get = function(URL = NULL, catalog = NULL, AOI = NULL, verbose = TRUE){

if (is.null(URL)) { URL <- catalog$URL }

vrts =  lapply(URL, terra::rast)
AOIv = terra::vect(AOI)

fin = tryCatch({
  crop(rast(vrts), project(AOIv, crs(vrts[[1]])))
}, error = function(e) {
  lapply(1:length(vrts), function(x){ crop(vrts[[x]], project(AOIv, crs(r[[x]]))) })
})

fin

}

