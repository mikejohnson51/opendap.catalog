base = 'http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/'

ids = base |>
  rvest::read_html() |>
  rvest::html_nodes("a") |>
  rvest::html_attr("href")

ids = grep("vrt$",ids, value = TRUE)

og_urls = paste0("/vsicurl/", base, ids)

parse_polaris_description = function(x){

  v = c('silt', 'silt percentage', '%',
        'sand', 'sand percentage', '%',
        'clay', 'clay percentage', '%',
        'bd', 'bulk density', 'g/cm3',
        'theta_s', 'saturated soil water content', 'm3/m3',
        'theta_r', 'residual soil water content', 'm3/m3',
        'ksat', 'saturated hydraulic conductivity', 'log10(cm/hr)',
        'ph', 'soil pH in H2O', 'N/A',
        'om', 'organic matter', 'log10(%)',
        'lambda', 'pore size distribution index (brooks-corey)', 'N/A',
        'hb', 'bubbling pressure (brooks-corey)', 'log10(kPa)',
        'n', 'measure of the pore size distribution (van genuchten)', 'N/A',
        'alpha', 'scale parameter inversely proportional to mean pore diameter (van genuchten)', 'log10(kPa-1)')

  v = matrix(v, ncol = 3, byrow = T) |>
    as.data.frame() |>
    setNames(c('varname', "description", "units"))

  m = strsplit(x, "_")[[1]]

  if(length(m) == 5){
    m = c(paste0(m[1], "_", m[2]), m[3], m[4], m[5])
  }

  v = v[v$varname == m[1], ]

  v$long_name = paste0(m[2]," ", m[1], " ", m[3], "-", m[4],  'cm' )

  v
}


polaris = list()

for(i in 1:length(og_urls)){
  r   = terra::rast(og_urls[i])
  des = parse_polaris_description(x = names(r))

  polaris[[i]] = data.frame(id = "polaris",
             URL = og_urls[i],
             varname = des$varname,
             units   = des$units,
             long_name = des$long_name,
             description = des$description,
             X1      = terra::xmin(r),
             Xn      = terra::xmax(r),
             Y1      = terra::xmin(r),
             Yn      = terra::ymax(r),
             resX    = terra::xres(r),
             resY    = terra::yres(r),
             ncols   = terra::ncol(r),
             nrows   = terra::nrow(r),
             proj    = sf::st_crs(r)$proj4string)
}

p = dplyr::bind_rows(polaris)



dep = data.frame(id = c("3DEP 30m", "3DEP 10m", "3DEP 60m"),
           URL = c('/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt',
'/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt',
'/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/2/TIFF/USGS_Seamless_DEM_2.vrt'),
           varname = rep("elevation", 3),
description = c("30m elevation", '10m elevation', "60m Alaska elevation"),
           units = rep("meters", 3))

o = make_vect(dep[1,]) |>
  as("Spatial") |>
  sf::st_as_sf()

AOI::aoi_map(o)

plot(r)

r2 = terra::rast('/Users/mjohnson/github/opendap.catalog/data-raw/ned/ned_USGS_2.vrt')

for(i in 1:nrow(dep)){
  r   = terra::rast(dep$URL[i])
  dep$X1[i]      = terra::xmin(r)
  dep$Xn[i]      = terra::xmax(r)
  dep$Y1[i]      = terra::xmin(r)
  dep$Yn[i]      = terra::ymax(r)
  dep$resX[i]    = terra::xres(r)
  dep$resY[i]    = terra::yres(r)
  dep$ncols[i]   = terra::ncol(r)
  dep$nrows[i]   = terra::nrow(r)
  dep$proj[i]    = sf::st_crs(r)$proj4string
}





saveRDS(p, "data-raw/polaris_vrt.rds")
