
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-5/26-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
[![Website
deployment](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/pkgdown.yaml)
[![LifeCycle](man/figures/lifecycle/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R CMD
Check](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<div id="top">

</div>

<br />

<div align="center">

<h1 align="center">
<strong>OpenDap</strong> Catalog
</h1>
<p align="center">
<a href="https://mikejohnson51.github.io/opendap.catalog/"><strong>«
Explore the Docs »</strong></a> <br /> <br />
<a href="https://mikejohnson51.github.io/opendap.catalog/cat_params.json">Data
Catalog</a> · <a href="">R Interface</a> ·
<a href="https://github.com/mikejohnson51/opendap.catalog/issues">Request
Feature</a>
</p>

</div>

<hr>

One of the biggest challenges with Earth System and spatial research is
extracting data. These challenges include not only finding the source
data but then downloading, managing, and extracting the partitions
critical for a given task.

Services exist to make data more readily available over the web but
introduce new challenges of identifying subsets, working across a wide
array of standards (e.g. non-standards), all without alleviating the
challenge of finding resources.

In light of this, `opendap.catolog` provides three primary services.

<hr>

#### 1. Generalized space (XY) and Time (T) subsets for *remote* and *local* NetCDF data with `dap()`

> remote

``` r
dap <- dap(URL = "https://cida.usgs.gov/thredds/dodsC/bcsd_obs", 
           AOI = AOI::aoi_get(state = "FL"), 
           startDate = "1995-01-01")
#> source:   https://cida.usgs.gov/thredds/dodsC/bcsd_obs 
#> varname(s):
#>    > pr [mm/m] (monthly_sum_pr)
#>    > prate [mm/d] (monthly_avg_prate)
#>    > tas [C] (monthly_avg_tas)
#>    > tasmax [C] (monthly_avg_tasmax)
#>    > tasmin [C] (monthly_avg_tasmin)
#>    > wind [m/s] (monthly_avg_wind)
#> ==================================================
#> diminsions:  62, 48, 1 (names: longitude,latitude,time)
#> resolution:  0.125, 0.125, 1 months
#> extent:      -87.75, -80, 25.12, 31.12 (xmin, xmax, ymin, ymax)
#> crs:         +proj=longlat +a=6378137 +f=0.00335281066474748 +p...
#> time:        1995-01-01 to 1995-01-01
#> ==================================================
#> values: 17,856 (vars*X*Y*T)

str(dap, max.level = 1)
#> List of 6
#>  $ pr    :Formal class 'SpatRaster' [package "terra"] with 1 slot
#>  $ prate :Formal class 'SpatRaster' [package "terra"] with 1 slot
#>  $ tas   :Formal class 'SpatRaster' [package "terra"] with 1 slot
#>  $ tasmax:Formal class 'SpatRaster' [package "terra"] with 1 slot
#>  $ tasmin:Formal class 'SpatRaster' [package "terra"] with 1 slot
#>  $ wind  :Formal class 'SpatRaster' [package "terra"] with 1 slot
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

> local

``` r
file <- '/Users/mjohnson/Downloads/NEXGDM_srad_2020_v100.nc'
utils:::format.object_size(file.size(file), "auto")
#> [1] "3.7 Gb"

dap = dap(URL = file, 
          AOI = AOI::aoi_get(state = "FL"), 
          startDate = "2020-01-01", endDate = "2020-01-05")
#> source:   /Users/mjohnson/Downloads/NEXGDM_srad_2020_v100.nc 
#> varname(s):
#>    > srad [MJ/day] (Shortwave radiation)
#> ==================================================
#> diminsions:  807, 693, 4 (names: x,y,time)
#> resolution:  1000, 1000, 1 days
#> extent:      795955, 1602955, 268005, 961005 (xmin, xmax, ymin, ymax)
#> crs:         +proj=aea +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +u...
#> time:        2020-01-02 to 2020-01-05
#> ==================================================
#> values: 2,237,004 (vars*X*Y*T)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

#### 2. A catalog of 14,160 web resources (as of 02/2022)

``` r
dplyr::glimpse(opendap.catalog::params)
#> Rows: 14,160
#> Columns: 15
#> $ id        <chr> "hawaii_soest_1727_02e2_b48c", "hawaii_soest_1727_02e2_b48c"…
#> $ grid.id   <chr> "71", "71", "71", "71", "71", "71", "71", "71", "71", "71", …
#> $ URL       <chr> "https://apdrc.soest.hawaii.edu/erddap/griddap/hawaii_soest_…
#> $ tiled     <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", …
#> $ variable  <chr> "nudp", "nusf", "nuvdp", "nuvsf", "nvdp", "nvsf", "sudp", "s…
#> $ varname   <chr> "nudp", "nusf", "nuvdp", "nuvsf", "nvdp", "nvsf", "sudp", "s…
#> $ long_name <chr> "number of deep zonal velocity profiles", "number of surface…
#> $ units     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ model     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ ensemble  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ scenario  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ T_name    <chr> "time", "time", "time", "time", "time", "time", "time", "tim…
#> $ duration  <chr> "2001-01-01/2022-01-01", "2001-01-01/2022-01-01", "2001-01-0…
#> $ interval  <chr> "365 days", "365 days", "365 days", "365 days", "365 days", …
#> $ nT        <int> 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, …
```

For use in other applications (e.g. [stars
proxy](https://github.com/r-spatial/stars/pull/499),
[geoknife](https://github.com/USGS-R/geoknife),
[climateR](https://github.com/mikejohnson51/climateR) or python/go/Rust
applciations) this catalog is available as a JSON artifact
[here](https://mikejohnson51.github.io/opendap.catalog/cat_params.json).

``` r
read_json('https://mikejohnson51.github.io/opendap.catalog/cat_params.json', 
          simplifyVector = TRUE)
```

### (3) The ability to pass catalog elements to the generalized toolsets:

``` r
# Find MODIS PET in Florida for January 2010
dap = dap(
    catolog = dplyr::filter(params, 
                            id == 'MOD16A2.006', 
                            varname == 'PET_500m'),
    AOI = AOI::aoi_get(state = "FL"),
    startDate = "2010-01-01",
    endDate   = "2010-01-31"
  )
#> source:   https://opendap.cr.usgs.gov/opendap/hyrax/MOD16A2.006/h10v05... 
#> tiles:    2 XY_modis tiles
#> varname(s):
#>    > PET_500m [kg/m^2/8day] (MODIS Gridded 500m 8-day Composite potential Evapotranspiration (ET))
#> ==================================================
#> diminsions:  1336, 1561, 5 (names: XDim,YDim,time)
#> resolution:  463.313, 463.313, 8 days
#> extent:      -8404261.02, -7785275.23, 2724047.12, 3447278.27 (xmin, xmax, ymin, ymax)
#> crs:         +proj=sinu +lon_0= +x_0= +y_0= +units=m +a=6371007...
#> time:        2010-01-02 to 2010-02-03
#> ==================================================
#> values: 10,427,480 (vars*X*Y*T)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />
