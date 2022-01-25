
<!-- README.md is generated from README.Rmd. Please edit that file -->

# opendap.catalog

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-5/32-orange?style=flat)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://choosealicense.com/licenses/mit/)
[![Website
deployment](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/pkgdown.yaml)
[![LifeCycle](man/figures/lifecycle/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R CMD
Check](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mikejohnson51/opendap.catalog/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# TL:DR;

opendap.cataloug provides the function needed to find, and document, the
inofrmation needed to identify and form OpenDap queries for a range of
OpenDap resources.

## Terminolgy

OPeNDAP is a framework that simplifies all aspects of scientific data
networking by provides software that makes local data accessible to
remote locations. [see here](https://www.opendap.org)

For example a large NetCDF file of gridmet data can sit on the
Northwestern comuting system and user - like us - can request subsets of
data from that file!

Large local files can be published to a web-based **T**HREDDS **D**ata
**S**erver from which metadata and data can be accessed using OPeNDAP,
OGC WCS, HTTP, and other data access protocols.

This allows users to stream the portion of the data set releavnat to
them!

## OpenDap Syntax

To request a subset from a TDS data using OpenDAP protocol, a common
form can be followed:

    URL?{varname}{Ymin:1:Ymax}{Xmin:1:Xmax}{Tmin:1:Tmax}

## Two use cases

### TDS

One predominate use case is TDS catolougs that store all variables as
independent datasets (think individual files). To archive these we need
to scan the TDS to find the available data resources. For this, we can
use `read_tds`:

``` r
tds = read_tds(URL = 'http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html', 
               id = "gridmet") 
head(tds)
```

### Aggregated files

| NAME      | DESCRITPION                                                 | EXAMPLE                                                         |
|-----------|-------------------------------------------------------------|-----------------------------------------------------------------|
| id        | Unique Dataset Identifier                                   | “maca_daily”                                                    |
| varname   | OpenDap variable name                                       | “specific_humidity”                                             |
| variable  | Variable as described by data resource                      | “huss”                                                          |
| X_name    | Variable as described by data resource                      | “huss”                                                          |
| Y_name    | Variable as described by data resource                      | “huss”                                                          |
| T_name    | Variable as described by data resource                      | “huss”                                                          |
| units     | Variable Units                                              | “kg kg-1”                                                       |
| scenario  | The climate scenario used to produce the data               | “rcp45                                                          |
| model     | If future projections then the GCM used                     | “BNU-ESM”                                                       |
| ensemble  | The ensemble memeber of the above GCM                       | “r1i1p1”                                                        |
| URL       | The web-location of the dataset                             | “<http://>…”                                                    |
| duration  | The time duration of the data resource                      | “1950-01-01/2005-12-31”                                         |
| interval  | The interval (resolution) of the time dimension             | “1 days”                                                        |
| nT        | If a non-open time diminsion then the number of time slices | “20454”                                                         |
| proj      | The spatial projection of the dataset                       | +proj=longlat +a=6378137 +f=0.00335281066474748 +pm=0 +no_defs” |
| ext       | The spatial extent of the dataset (xmin, xmax, ymin, ymax)  | c(-124.77, -67.06, 25.06, 49.40)                                |
| dimension | Variable as described by data resource                      | c(1386, 585)                                                    |

# Use Cases

## Remote Resource

### BCSD

``` r
library(terra)

AOI <- AOI::aoi_get(state = "FL", county = "all")

url <- "https://cida.usgs.gov/thredds/dodsC/bcsd_obs"

dap = dap_crop(URL = url, AOI = AOI, startDate = "1995-01-01")

print.dap(dap)

out = dap_get(dap[1,])

plot(out$pr)
```

## Local Resource

### Nex>\>

## Remote Spatially tiled reosouces

``` r
url <- '/Users/mjohnson/Downloads/NEXGDM_srad_2020_v100.nc'
file.size(url)

dap = dap_crop(URL = '/Users/mjohnson/Downloads/NEXGDM_srad_2020_v100.nc',
               AOI = AOI,
               startDate = "2020-01-01",
              endDate = "2020-01-05")  


out = dap_get(dap)

plot(out$srad)
```

### MODIS

## Remote Temportally tiled reources

### LOCA

# Uses

[climateR]() [stars NetCDF proxies]()
