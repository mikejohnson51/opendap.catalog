
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
#> Warning: package 'terra' was built under R version 4.1.2
#> terra 1.5.17

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

knitr::kable(dap, format="html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
id
</th>
<th style="text-align:left;">
varname
</th>
<th style="text-align:left;">
X_name
</th>
<th style="text-align:left;">
Y_name
</th>
<th style="text-align:left;">
T_name
</th>
<th style="text-align:left;">
units
</th>
<th style="text-align:left;">
long_name
</th>
<th style="text-align:left;">
URL
</th>
<th style="text-align:left;">
duration
</th>
<th style="text-align:left;">
interval
</th>
<th style="text-align:right;">
nT
</th>
<th style="text-align:left;">
proj
</th>
<th style="text-align:right;">
X1
</th>
<th style="text-align:right;">
Xn
</th>
<th style="text-align:right;">
Y1
</th>
<th style="text-align:right;">
Yn
</th>
<th style="text-align:right;">
resX
</th>
<th style="text-align:right;">
resY
</th>
<th style="text-align:right;">
ncols
</th>
<th style="text-align:right;">
nrows
</th>
<th style="text-align:left;">
toptobottom
</th>
<th style="text-align:right;">
Tdim
</th>
<th style="text-align:left;">
startDate
</th>
<th style="text-align:left;">
endDate
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
local
</td>
<td style="text-align:left;">
pr
</td>
<td style="text-align:left;">
longitude
</td>
<td style="text-align:left;">
latitude
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
mm/m
</td>
<td style="text-align:left;">
monthly_sum_pr
</td>
<td style="text-align:left;">
<https://cida.usgs.gov/thredds/dodsC/bcsd_obs?pr%5B540:1:540>\]\[0:1:47\]\[296:1:357\]
</td>
<td style="text-align:left;">
1950-01-31/1999-12-31
</td>
<td style="text-align:left;">
1 months
</td>
<td style="text-align:right;">
600
</td>
<td style="text-align:left;">
+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs
</td>
<td style="text-align:right;">
-87.6875
</td>
<td style="text-align:right;">
-80.0625
</td>
<td style="text-align:right;">
25.1875
</td>
<td style="text-align:right;">
31.0625
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1995-01-01
</td>
<td style="text-align:left;">
1995-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
local
</td>
<td style="text-align:left;">
prate
</td>
<td style="text-align:left;">
longitude
</td>
<td style="text-align:left;">
latitude
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
mm/d
</td>
<td style="text-align:left;">
monthly_avg_prate
</td>
<td style="text-align:left;">
<https://cida.usgs.gov/thredds/dodsC/bcsd_obs?prate%5B540:1:540>\]\[0:1:47\]\[296:1:357\]
</td>
<td style="text-align:left;">
1950-01-31/1999-12-31
</td>
<td style="text-align:left;">
1 months
</td>
<td style="text-align:right;">
600
</td>
<td style="text-align:left;">
+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs
</td>
<td style="text-align:right;">
-87.6875
</td>
<td style="text-align:right;">
-80.0625
</td>
<td style="text-align:right;">
25.1875
</td>
<td style="text-align:right;">
31.0625
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1995-01-01
</td>
<td style="text-align:left;">
1995-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
local
</td>
<td style="text-align:left;">
tas
</td>
<td style="text-align:left;">
longitude
</td>
<td style="text-align:left;">
latitude
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
C
</td>
<td style="text-align:left;">
monthly_avg_tas
</td>
<td style="text-align:left;">
<https://cida.usgs.gov/thredds/dodsC/bcsd_obs?tas%5B540:1:540>\]\[0:1:47\]\[296:1:357\]
</td>
<td style="text-align:left;">
1950-01-31/1999-12-31
</td>
<td style="text-align:left;">
1 months
</td>
<td style="text-align:right;">
600
</td>
<td style="text-align:left;">
+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs
</td>
<td style="text-align:right;">
-87.6875
</td>
<td style="text-align:right;">
-80.0625
</td>
<td style="text-align:right;">
25.1875
</td>
<td style="text-align:right;">
31.0625
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1995-01-01
</td>
<td style="text-align:left;">
1995-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
local
</td>
<td style="text-align:left;">
tasmax
</td>
<td style="text-align:left;">
longitude
</td>
<td style="text-align:left;">
latitude
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
C
</td>
<td style="text-align:left;">
monthly_avg_tasmax
</td>
<td style="text-align:left;">
<https://cida.usgs.gov/thredds/dodsC/bcsd_obs?tasmax%5B540:1:540>\]\[0:1:47\]\[296:1:357\]
</td>
<td style="text-align:left;">
1950-01-31/1999-12-31
</td>
<td style="text-align:left;">
1 months
</td>
<td style="text-align:right;">
600
</td>
<td style="text-align:left;">
+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs
</td>
<td style="text-align:right;">
-87.6875
</td>
<td style="text-align:right;">
-80.0625
</td>
<td style="text-align:right;">
25.1875
</td>
<td style="text-align:right;">
31.0625
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1995-01-01
</td>
<td style="text-align:left;">
1995-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
local
</td>
<td style="text-align:left;">
tasmin
</td>
<td style="text-align:left;">
longitude
</td>
<td style="text-align:left;">
latitude
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
C
</td>
<td style="text-align:left;">
monthly_avg_tasmin
</td>
<td style="text-align:left;">
<https://cida.usgs.gov/thredds/dodsC/bcsd_obs?tasmin%5B540:1:540>\]\[0:1:47\]\[296:1:357\]
</td>
<td style="text-align:left;">
1950-01-31/1999-12-31
</td>
<td style="text-align:left;">
1 months
</td>
<td style="text-align:right;">
600
</td>
<td style="text-align:left;">
+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs
</td>
<td style="text-align:right;">
-87.6875
</td>
<td style="text-align:right;">
-80.0625
</td>
<td style="text-align:right;">
25.1875
</td>
<td style="text-align:right;">
31.0625
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1995-01-01
</td>
<td style="text-align:left;">
1995-01-01
</td>
</tr>
<tr>
<td style="text-align:left;">
local
</td>
<td style="text-align:left;">
wind
</td>
<td style="text-align:left;">
longitude
</td>
<td style="text-align:left;">
latitude
</td>
<td style="text-align:left;">
time
</td>
<td style="text-align:left;">
m/s
</td>
<td style="text-align:left;">
monthly_avg_wind
</td>
<td style="text-align:left;">
<https://cida.usgs.gov/thredds/dodsC/bcsd_obs?wind%5B540:1:540>\]\[0:1:47\]\[296:1:357\]
</td>
<td style="text-align:left;">
1950-01-31/1999-12-31
</td>
<td style="text-align:left;">
1 months
</td>
<td style="text-align:right;">
600
</td>
<td style="text-align:left;">
+proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs
</td>
<td style="text-align:right;">
-87.6875
</td>
<td style="text-align:right;">
-80.0625
</td>
<td style="text-align:right;">
25.1875
</td>
<td style="text-align:right;">
31.0625
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
0.125
</td>
<td style="text-align:right;">
62
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
TRUE
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1995-01-01
</td>
<td style="text-align:left;">
1995-01-01
</td>
</tr>
</tbody>
</table>

``` r
bcsd = dap_get(dap = dap[dap$varname == "pr",])

plot(bcsd$pr)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

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
#>   0.852   0.114   1.014

dap.summary(dap)
#> vars:   > srad [MJ/day]
#> X:      807 (x)
#> Y:      693 (y)
#> T:      4 (time - 1 days)
#> values: 2,237,004 (vars*X*Y*T)

plot(nexgdm$srad)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

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
#> Warning: [src] "src" will be removed. It has been renamed to "sprc"
#>    user  system elapsed 
#>   3.994   1.339   8.543

dap.summary(dap)
#> vars:   > PET_500m [kg/m^2/8day]
#>  > PET_500m [kg/m^2/8day]
#> X:      1336 - 1336 (XDim)
#> Y:      240 - 1321 (time)
#> T:      3 (XDim - 8 days)
#> values: 12,512,976 (vars*X*Y*T)
terra::plot(mod)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## Remote Temportally tiled reources

…

### LOCA

…

## NGEN focus?

### January ET for Florida counties

``` r
ref = readRDS('src/modis_map.rds') |>
  dplyr::filter(id == 'mod16', variable == 'ET_500m') |> 
  dplyr::mutate(tiled = "XY")

AOI = AOI::aoi_get(state = "FL", county = 'all')

system.time({
  dap = dap_crop(catolog = ref, AOI = AOI,
                 startDate = "2020-01-01", endDate = "2020-01-31")
  
  mod = dap_get(dap)
})
#> Warning: [src] "src" will be removed. It has been renamed to "sprc"
#>    user  system elapsed 
#>   4.908   2.184  10.430

system.time({
 agg = zonal::execute_zonal(mod, AOI, "geoid")
})
#>    user  system elapsed 
#>   2.288   0.398   2.729

plot(agg[grep("V", names(agg))])
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

``` r
aggMax = zonal::execute_zonal(mod, AOI, "geoid", FUN = "max")

plot(aggMax[grep("V", names(aggMax))])
```

<img src="man/figures/README-unnamed-chunk-7-2.png" width="100%" />
