# two attributes in a NetCDF file: use a list of functions giving the sub-datasets:
bcsd_obs = read_stars(
  list(
    function() c(foo = paste0("NETCDF:\"", system.file("nc/bcsd_obs_1999.nc", package = "stars"), "\":pr")),
    function() c(bar = paste0("NETCDF:\"", system.file("nc/bcsd_obs_1999.nc", package = "stars"), "\":tas"))
  )
)
usethis::use_data(bcsd_obs, overwrite = TRUE)

fs::file_copy(system.file("nc/bcsd_obs_1999.nc", package = "stars"),
              "inst/nc/bcsd_obs_1999.nc")


dap("inst/nc/bcsd_obs_1999.nc")
