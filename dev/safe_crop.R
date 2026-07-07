safe_crop <- function(x, y, name = "raster") {
  
  r <- terra::crop(x, y, snap = "out")
  
  if (raster_is_empty(r)) {
    warning(paste(name, "empty after crop"))
    return(NULL)
  }
  
  r
}
