#' ## Create example NetCDF file

# Get daily temperature EWEMBI ISIMIP2b files
tas_files <- list.files("E:/Data/ISIMIP2b/InputData/EWEMBI/", pattern="^tas_", full.names=T)

# Load data
tas_data <- lapply(tas_files, raster::stack)

# Crop data by extent of Germany
deu <- raster::getData("GADM", country="DEU", level=1, path="E:Data/GADM")
tas_deu <- lapply(tas_data, function(x) raster::mask(raster::crop(x, deu), deu))

# Get list of z units
z_units <- lapply(tas_files, function(x) ncdf4::nc_open(x)$dim[[3]]$units)

# Save data to NetCDF file
mapply(FUN=function(x,y,z){
  raster::writeRaster(x=x, filename=paste0("inst/extdata/", y), format="CDF",
           varname="tas", varunit="Kelvin", xname="lon", yname="lat", 
           zname="time", zunit=z, overwrite=T)
}, x=tas_deu, y=sub("ewembi1", "ewembi_deu", basename(tas_files)), z=z_units)

files <-  paste0("inst/extdata/", sub("ewembi1", "ewembi_deu", basename(tas_files)))
nc <- ncdf4::nc_open(files[1])
nc$dim[[3]]$units
