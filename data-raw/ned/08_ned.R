
# Create meta data object of three NED resoruces
ned <- data.frame(rbind(
  c(id        = "USGS_1",
    URL       = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1",
    varname   = "30m elevation",
    long_name = "30m (1 arcsec) National Elevation Dataset",
    units     = "m"),

  c(id        = "USGS_2",
    URL       = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/2",
    varname   = "60m elevation",
    long_name = "60m (2 arcsec) National Elevation Dataset Alaska",
    units     = "m"),

  c(id        = "USGS_13",
    URL       = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13",
    varname   = "10m elevation",
    long_name = "10m (1/3th arcsec) National Elevation Dataset",
    units     = "m")
))




# Loop over the three resolutions
for (i in 1:length(ned)) {

  # Define output text file path
  txt_file  <- paste0("data-raw/ned_list_", ned$id[i], ".txt")

  # Define output VRT path
  vrt_file <- paste0(paste0("data-raw/ned_", ned$id[i], ".vrt"))

  # If VRT does NOT exist, build VRT
  if (!file.exists(vrt_file)) {
    # read the corresponding index.gpkg
    files <- sf::read_sf(ned$domain_url[i])

    # Build full HTTPS paths to "./current/"
    files <- c(file.path(ned$URL[i], "TIFF/current", gsub("[.]/", "", files$location)))

    # write list of files to text file
    write.table(files, txt_file, row.names = FALSE, col.names = FALSE, quote = FALSE)

    # build VRT from text file input using GDAL system call ...
    system(paste("gdalbuildvrt -input_file_list", txt_file, vrt_file))
  }

  logger::log_info("Finished ", ned$id[i], "...")
}
