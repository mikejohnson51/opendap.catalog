#' Get DAP resource data
#' @param dap data.frame from catalog or dap_crop
#' @param varname  name of variable to extract. If NULL, then get all
#' @return SpatRaster
#' @export
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom terra sprc merge units nlyr
#' @importFrom future.apply future_lapply

dap_get <- function(dap, varname = NULL) {

  if (!is.null(varname)) {
    if (!varname %in% dap$varname) {
      stop("variable in resource include:\n", paste(dap$varname, collapse = ", "))
    }

    dap <- dap[dap$varname %in% varname, ]
  }

  out <- future_lapply(1:nrow(dap), FUN = function(x) {
    go_get_dap_data(dap[x, ])
  })

  names(out) <- sub("_$", "", paste0(dap$varname, "_", dap$scenario))

  if (any(grepl("XY", dap$tiled))) {
    u <- unique(unlist(lapply(out, units)))
    if (length(u) == 1) {
      out <- suppressWarnings({ merge(sprc(out)) })
      terra::units(out) <- rep(u, nlyr(out))
      out
    } else {
      out
    }
  } else if (any(dap$tiled == "T")) {

    ll = list()
    g = expand.grid(v = unique(dap$varname), s = unique(dap$scenario))

    for(v in unique(g$v)){
      g_tmp = g[g$v == v, ]
      ind = grepl(v, names(out))
      tmp = out[ind]
      n = unlist(lapply(1:length(tmp), function(x) { paste0(names(tmp[[x]]), "_", g_tmp$s[x]) }))
      o = order(n)
      tmp = rast(tmp)
      tmp = tmp[[o]]
      names(tmp) = n
      ll[[v]] = tmp
    }

   ll
  } else {
    out
  }
}
