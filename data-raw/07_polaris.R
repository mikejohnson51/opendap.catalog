base = 'http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/'

ids = base |>
  rvest::read_html() |>
  rvest::html_nodes("a") |>
  rvest::html_attr("href")

ids = grep("vrt$",ids, value = TRUE)

og_urls = glue::glue("/vsicurl/{base}{ids}")

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


dep = data.frame(
  id = c("3DEP 30m", "3DEP 10m", "3DEP 60m Alaska", "GEBCO2019", "NASADEM"),
  URL = c(
    '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt',
    '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt',
    '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/2/TIFF/USGS_Seamless_DEM_2.vrt',
    '/vsicurl/https://public.services.aad.gov.au/datasets/science/GEBCO_2019_GEOTIFF/GEBCO_2019.tif',
    '/vsicurl/https://opentopography.s3.sdsc.edu/raster/NASADEM/NASADEM_be.vrt'
  ),
  varname = rep("elevation", 5),
  description = c("30m", '10m', "60m", "GEBCO+SRTM", "NASADEM"),
  units = rep("meters", 5)
)

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


year = c(2019, 2016, 2011, 2008, 2006, 2004, 2001)
dataset = c('Land_Cover', 'Impervious', 'Tree_Canopy')
landmass = c('L48', 'AK', 'HI', 'PR')

g = expand.grid(year, dataset, landmass)

source <- "https://storage.googleapis.com/feddata-r/nlcd/"
file <- paste0(g$Var1, "_", g$Var2, "_", g$Var3, ".tif")

df = data.frame(
  URL = paste0(source, file),
  description = paste("NLCD", g$Var2, g$Var3,  g$Var1),
  id = paste("NLCD", g$Var2, g$Var3,  g$Var1, sep = "_"),
  varname = g$Var2,
  units = ""
)

for(i in 1:nrow(df)){
  df$exists[i] =  df$URL[i] %>%
    httr::HEAD() %>%
    httr::status_code() %>%
    identical(200L)

  message(i)

}

df = filter(df, exists) %>%
  mutate(URL = paste0("/vsicurl/", URL)) %>%
  select(-exists)

for(i in 1:nrow(df)){
  r   = terra::rast(df$URL[i])
  df$X1[i]      = terra::xmin(r)
  df$Xn[i]      = terra::xmax(r)
  df$Y1[i]      = terra::xmin(r)
  df$Yn[i]      = terra::ymax(r)
  df$resX[i]    = terra::xres(r)
  df$resY[i]    = terra::yres(r)
  df$ncols[i]   = terra::ncol(r)
  df$nrows[i]   = terra::nrow(r)
  df$proj[i]    = sf::st_crs(r)$proj4string

  message(i)
}

vrts = bind_rows(list(df, dep, p))



saveRDS(vrts, "data-raw/vrts.rds")
