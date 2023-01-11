#' @title Get Data
#' @description this function provides a consistenet data access protocol (dap) to a wide
#' range of local and remote resources including VRT, TDS, NetCDF
#' @description Define and get data from a DAP resource
#' @param URL local file path or dodC URL
#' @param catalog subset of open.dap catalog
#' @param AOI sf object
#' @param startDate start date (YYYY-MM-DD)
#' @param endDate  end date (YYYY- MM-DD)
#' @param start for non "dated" items, start can be called by index
#' @param end for non "dated" items, end can be called by index
#' @param varname  name of variable to extract. If NULL, then get all
#' @param toptobottom should data be inverse?
#' @param verbose  Should dap_summary be printed?
#' @details Wraps dap_get and dap_crop into one.
#' If AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @export

dap <- function(URL = NULL,
                catalog = NULL,
                AOI = NULL,
                startDate = NULL,
                endDate = NULL,
                varname = NULL,
                g    = NULL,
                start = NULL,
                end = NULL,
                toptobottom = FALSE,
                verbose = TRUE) {

  urls = c(URL, catalog$URL)

  if(any(getExtension(urls) %in% c('vrt', "tif")) | all(grepl("vsi", urls))){
    vrt_crop_get(URL = urls,
                 catalog = catalog,
                 AOI = AOI,
                 varname = varname,
                 g = g,
                 start = start,
                 end = end,
                 toptobottom = toptobottom,
                 verbose = verbose)
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


#' Terra Based Cropping
#' @param URL a single or set of URLs
#' @param catalog a single or set of catalog elements
#' @param AOI a spatial boundary
#' @param grid a list containing an extent (), and
#' @param varname a single or set of variables to collect, in NULL all are kept
#' @param start for non "dated" items, start can be called by index
#' @param end for non "dated" items, end can be called by index
#' @param toptobottom should data be inverse?
#' @param verbose should warning be emitted?
#' @return SpatRaster
#' @export
#' @importFrom terra rast ext crs vect crop project flip `ext<-` `crs<-`

vrt_crop_get = function(URL = NULL, catalog = NULL,
                        AOI = NULL, g = NULL, varname = NULL,
                        start = NULL, end = NULL,
                        toptobottom = FALSE, verbose = TRUE){

  if (is.null(URL)) { URL <- catalog$URL }

  vrts =  suppressWarnings({ rast(URL) })

  if(!is.null(varname)){
   vrts = vrts[[grepl(paste(varname, collapse = "|"), names(vrts))]]
  }

  if(!is.null(start) & is.null(end)){ vrts = vrts[[start]] }
  if(!is.null(start) & !is.null(end)){ vrts = vrts[[start:end]] }

  if(!is.null(g)){
      ext(vrts) <- g$extent
      crs(vrts) <- g$crs
      fin = vrts
      flag = TRUE
  } else {
    if(crs(vrts[[1]]) == "" | all(as.vector(ext(vrts[[1]])) == c(0,1,0,1))){

      if(verbose){
        warning("Defined URL(s) are aspatial and on a unit grid. Cannot be cropped", call. = FALSE)
      }

      flag = FALSE
    }

    fin = vrts
    flag = TRUE
  }

  if(!is.null(AOI) & flag){
    AOIv =  vect(AOI)

    fin = tryCatch({
      crop(fin, project(AOIv, crs(fin[[1]])))
    }, error = function(e) {
      lapply(1:length(fin), function(x){ crop(fin[[x]], project(AOIv, crs(fin[[x]]))) })
    })
  }


  if (toptobottom) { fin <- flip(fin) }

  fin

}

