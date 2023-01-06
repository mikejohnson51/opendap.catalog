vrt_summary <- function(vrt = NULL, url = NULL) {


}



#' Print Summary Information About a OpenDAP Resource
#' @description Print summary information about a DAP summary
#' @param dap data.frame from catalog or dap_crop
#' @param url Unique Resource Identifier (http or local)
#' @export

dap_summary <- function(dap = NULL, url = NULL) {
  if (!is.null(url) & is.null(dap)) {
    dap <- dap_crop(url)
  } else {
    resx <- (dap$Xn - dap$X1) / (dap$ncols - 1)
    resx = ifelse(is.na(resx), 'POINT', resx)
    resy <- (dap$Yn - dap$Y1) / (dap$nrows - 1)
    resy = ifelse(is.na(resy), 'POINT', resx)

    if(resx == "POINT"){
      xmin <- dap$X1
      xmax <- dap$X1
      ymin <- dap$Y1
      ymax <- dap$Y1
      ncol <- 1
      nrow <- 1
    } else {
      xmin <- min(dap$X1 - 0.5 * resx)
      xmax <- max(dap$Xn + 0.5 * resx)
      ymin <- min(dap$Y1 - 0.5 * resy)
      ymax <- max(dap$Yn + 0.5 * resy)
      ncol <- round((xmax - xmin) / unique(resy)[1])
      nrow <- round((ymax - ymin) / unique(resx)[1])
    }

    ext <- paste0(
      paste(round(c(xmin, xmax, ymin, ymax), 2), collapse = ", "),
      " (xmin, xmax, ymin, ymax)"
    )

    minDate <- min(as.POSIXct(dap$startDate))

    maxDate <- max(as.POSIXct(dap$endDate))

    if(dap$interval[1] != 0){
      tDim <- length(seq.POSIXt(minDate, maxDate, by = dap$interval[1]))
    } else {
      tDim = 1
    }

    var <- unique(paste0(dap$varname, " [", dap$units, "] (", dap$long_name, ")"))

    a <- dap$proj[1]
    b <- strsplit(dap$URL[1], "\\?")[[1]][1]
    b <- ifelse(nchar(b) > 60, paste0(strtrim(b, 60), "..."), b)

    {
      cat("source:\t", b, "\n")
      if (max(table(dap$varname)) > 1) {
        cat("tiles:\t", max(table(dap$varname)), unique(dap$tiled), "tiles\n")
      }
      cat("varname(s):\n  ", paste(">", var, collapse = "\n   "))
      cat(paste0("\n", paste(rep("=", 50), collapse = "")))
      cat(
        "\ndiminsions: ",
        paste0(
          round(ncol),
          ", ",
          round(nrow),
          ", ",
          tDim,
          " (names: ",
          dap$X_name[1],
          ",",
          dap$Y_name[1],
          ",",
          dap$T_name[1],
          ")"
        )
      )
      cat(
        "\nresolution: ",
        paste0(
          round(dap$resX[1], 3),
          ", ",
          round(dap$resY[1], 3),
          ", ",
          max(1, dap$interval[1])
        )
      )
      cat("\nextent:     ", ext)
      cat("\ncrs:        ", ifelse(nchar(a) > 50, paste0(strtrim(a, 50), "..."), a))
      cat(
        "\ntime:       ",
        as.character(minDate),
        "to",
        as.character(maxDate) # ,
        # paste0("(by: ", dap$interval[1], ")")
      )
      cat(paste0("\n", paste(rep("=", 50), collapse = "")))
      cat(
        "\nvalues:",
        formatC(
          nrow * ncol * tDim * length(var),
          big.mark = ",",
          digits = 0,
          format = "f"
        ),
        "(vars*X*Y*T)"
      )
    }
  }
}
