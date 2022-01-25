
<!-- README.md is generated from README.Rmd. Please edit that file -->

# opendap.catalog

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-7/33-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
[![Website
deployment](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/pkgdown.yaml)
[![LifeCycle](man/figures/lifecycle/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R CMD
Check](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# TL:DR;

`opendapR` provides a generalized inter functions needed to find, and
document, the information needed to identify and form OpenDap queries
for a range of resources.

## Terminolgy

OPeNDAP is a framework that simplifies scientific data networking via
software that makes local data accessible to remote locations. [see
here](https://www.opendap.org)

For example a large NetCDF file of gridmet data can sit on the
Northwestern computing system and users - like us - can request subsets
of data from that file!

Large local files can be published to a web-based **T**HREDDS **D**ata
**S**erver from which metadata and data can be accessed using OPeNDAP,
OGC WCS, HTTP, and other data access protocols.

This allows users to stream the portion of the data set releavnat to
them!

## OpenDap Syntax

To request a subset from a TDS data using OpenDAP protocol, a common
form can be followed:

    URL?{varname}{Ymin:1:Ymax}{Xmin:1:Xmax}{Tmin:1:Tmax}

# Use Cases

Lets get data in some different ways for the state of Florida.

``` r
library(opendap.catalog)
library(terra)
#> terra 1.5.12

AOI <- AOI::aoi_get(state = "FL", county = "all")
plot(AOI$geometry)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Remote Resource

``` r
url <- "https://cida.usgs.gov/thredds/dodsC/bcsd_obs"

dap = dap_crop(URL = url, AOI = AOI, startDate = "1995-01-01")

dap.summary(dap)
#> vars:   > pr [mm/m]
#>  > prate [mm/d]
#>  > tas [C]
#>  > tasmax [C]
#>  > tasmin [C]
#>  > wind [m/s]
#> X:      62 (longitude)
#> Y:      48 (latitude)
#> T:      1 (time - 1 months)
#> values: 2,976 (vars*X*Y*T)

bcsd = dap_get(dap = dap[dap$varname == "pr",])

plot(bcsd$pr)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Local Resource

``` r
url <- '/Users/mjohnson/Downloads/NEXGDM_srad_2020_v100.nc'
utils:::format.object_size(file.size(url), "auto")
#> [1] "3.7 Gb"

system.time({
  dap = dap_crop(URL = url, AOI = AOI, 
                 startDate = "2020-01-01", endDate = "2020-01-05")
  nexgdm = dap_get(dap)
})
#> Warning in getGeoDatum(gm): Didn't find a longitude of prime meridian for datum,
#> assuming 0.
#> Warning in getGeoDatum(gm): Didn't find a semi major axis for datum, assuming
#> WGS84 6378137.0 meters
#> Warning in getGeoDatum(gm): Didn't find an inverse flattening value, assuming
#> WGS84 298.257223563
#>    user  system elapsed 
#>   0.715   0.092   0.818

dap.summary(dap)
#> vars:   > srad [MJ/day]
#> X:      807 (x)
#> Y:      693 (y)
#> T:      4 (time - 1 days)
#> values: 2,237,004 (vars*X*Y*T)

plot(nexgdm$srad)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Remote Spatially tiled reosouces

### MODIS

``` r
ref = readRDS('src/modis_map.rds') |>
  dplyr::filter(id == 'mod16', variable == 'PET_500m') |> 
  dplyr::mutate(tiled = "XY")

nrow(ref) # tiles
#> [1] 283

system.time({
  dap = dap_crop(catolog = ref, AOI = AOI::aoi_get(state = "FL"),
                 startDate = "2020-01-01", endDate = "2020-01-15")
  
  mod = dap_get(dap)
})
#>    user  system elapsed 
#>   4.286   1.063   9.945

dap.summary(dap)
#> vars:   > PET_500m [kg/m^2/8day]
#>  > PET_500m [kg/m^2/8day]
#> X:      1336 - 1336 (XDim)
#> Y:      240 - 1321 (time)
#> T:      3 (XDim - 8 days)
#> values: 12,512,976 (vars*X*Y*T)
terra::plot(mod)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" /> ##
Remote Temportally tiled reources

### LOCA
