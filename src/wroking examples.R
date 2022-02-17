library(opendap.catalog)
library(terra)
library(dplyr)

AOI <- AOI::aoi_get(state = "FL", county = "all")
cat <- jsonlite::read_json('/Users/mjohnson/github/opendap.catalog/cat_params.json',
simplifyVector = TRUE)

##### Across Space
modis_ex = filter(cat, id == 'MOD16A2.006', varname == 'PET_500m')

system.time({
  dap = dap_crop(
    catolog = modis_ex,
    AOI = AOI,
    startDate = "2010-01-01",
    endDate = "2010-01-31"
  ) |>
    dap_get()
})

plot(dap)

##### Across Time
maca_ex = filter(cat,
    id == 'maca_day',
    variable == 'huss',
    model == 'BNU-ESM',
    scenario %in% c("historical", 'rcp85')
  )

system.time({
  dap = dap_crop(
    catolog = maca_ex,
    AOI = AOI,
    startDate = "2005-12-25",
    endDate = "2006-01-05"
  ) |>
    dap_get()
})

dap = c(dap$specific_humidity_historical,
        dap$specific_humidity_rcp85)

plot(dap)


##### Multi Grid, Hourly Reosurces
nldas_ex = filter(cat, id == 'NLDAS_FORA0125_H.002', varname == 'apcpsfc')

system.time({
  dap = dap_crop(
    catolog = nldas_ex,
    AOI = AOI,
    startDate = "2005-12-25",
    endDate = "2005-12-25"
  ) |>
    dap_get()
})

plot(dap$apcpsfc_NA)
