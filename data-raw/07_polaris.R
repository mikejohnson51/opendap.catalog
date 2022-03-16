base = 'http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/vrt/'

ids = base |>
  rvest::read_html() |>
  rvest::html_nodes("a") |>
  rvest::html_attr("href")

ids = grep("vrt$",ids, value = TRUE)

og_urls = paste0("/vsicurl/", base, ids)

r = rast(og_urls[1])

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

for(i in 331:length(og_urls)){
  r = rast(og_urls[i])
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

saveRDS(p, "data-raw/polaris_vrt.rds")
