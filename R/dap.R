
#' @title Get DAP
#' @description Define and get data from a DAP resource
#' @param URL local file path or dodC URL
#' @param catolog subset of open.dap catolog
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
                catolog = NULL,
                AOI = NULL,
                startDate = NULL,
                endDate = NULL,
                varname = NULL,
                verbose = TRUE) {

  dap <- dap_crop(
    URL = URL,
    catolog = catolog,
    AOI = AOI,
    startDate = startDate,
    endDate = endDate,
    varname = varname,
    verbose = verbose
  )

  dap_get(dap)
}
