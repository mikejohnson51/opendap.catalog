library(terra)

url = '/vsis3/noaa-nwm-retrospective-2-1-pds/forcing/1979/197902010000.LDASIN_DOMAIN1'

g = list(extent = c(
  -2303999.62876143,
  2304000.37123857,
  -1920000.70008381,
  1919999.29991619
),
crs = 'PROJCS["Sphere_Lambert_Conformal_Conic",GEOGCS["GCS_Sphere",DATUM["D_Sphere",SPHEROID["Sphere",6370000.0,0.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],PARAMETER["false_easting",0.0],PARAMETER["false_northing",0.0],PARAMETER["central_meridian",-97.0],PARAMETER["standard_parallel_1",30.0],PARAMETER["standard_parallel_2",60.0],PARAMETER["latitude_of_origin",40.000008],UNIT["Meter",1.0]];-35691800 -29075200 126180232.640845;-100000 10000;-100000 10000;0.001;0.001;0.001;IsHighPrecision'
)


test_that("dap_warn", {
  expect_warning(dap(URL = url))
})

test_that("nwm read", {
  x = dap(URL = url, verbose = FALSE)
  expect_true(inherits(x, "SpatRaster"))
  expect_true(all(as.vector(terra::ext(x)) == c(0,1,0,1)))
  expect_equal(ncell(x), 17694720)
})

test_that("nwm grid/crop", {

  x = dap(URL = url,
          g = g,
          verbose = FALSE)

  expect_true(all(as.vector(terra::ext(x)) == g$extent))
  expect_equal(nlyr(x), 9)

  x2 = dap(URL = url,
            g = g,
            AOI = AOI::aoi_get(state = "CA"),
           verbose = FALSE)

  expect_equal(nlyr(x2), 9)
  expect_true(terra::ncell(x) > terra::ncell(x2))

})

test_that("nwm varname", {

  x = dap(URL     = url,
           g   = g,
           AOI    = AOI::aoi_get(state = "CA"),
           varname = "RAINRATE",
          verbose = FALSE)

  x2 = dap(URL = url,
           g = g,
           AOI = AOI::aoi_get(state = "CA"),
           varname = c("RAINRATE", "U2D"),
           verbose = FALSE)

  expect_equal(nlyr(x),  1)
  expect_equal(nlyr(x2), 2)

})

test_that("nwm topbottom", {

  x = dap(URL = url,
          g = g,
          AOI = AOI::aoi_get(state = "CA"),
          varname = "U2D",
          verbose = FALSE)

  x2 = dap(URL = url,
          g = g,
          AOI = AOI::aoi_get(state = "CA"),
          varname = "U2D",
          toptobottom = TRUE,
          verbose = FALSE)

  expect_true(!all(terra::values(x$U2D) !=  terra::values(x2$U2D)))
})



test_that("era5land slice", {
  url = '/vsis3/era5-pds/2020/01/data/air_pressure_at_mean_sea_level.nc'

  x = dap(URL = url,
          g = list(extent = c(-180,180,-90,90), crs = "EPSG:4326"),
          verbose = FALSE)

  x1 = dap(URL = url,
          g = list(extent = c(-180,180,-90,90), crs = "EPSG:4326"),
          start = 2,
          verbose = FALSE)

  x2 = dap(URL = url,
          g = list(extent = c(-180,180,-90,90), crs = "EPSG:4326"),
          start = 2,
          end = 4,
          verbose = FALSE)

  expect_true(nlyr(x) == 744)
  expect_true(nlyr(x1) == 1)
  expect_true(nlyr(x2) == 3)

})


test_that("era5land set URL", {
  url = c('/vsis3/era5-pds/2020/01/data/air_pressure_at_mean_sea_level.nc',
          '/vsis3/era5-pds/2020/02/data/air_pressure_at_mean_sea_level.nc')

  x = dap(URL = url,
          g = list(extent = c(-180,180,-90,90), crs = "EPSG:4326"),
          verbose = FALSE)

  x2 = dap(URL = url,
          g = list(extent = c(-180,180,-90,90), crs = "EPSG:4326"),
          verbose = FALSE,
          start = 744,
          end   = 745)


  expect_true(nlyr(x) == 1440)
  expect_true(nlyr(x2) == 2)

})
