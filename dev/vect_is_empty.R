vect_is_empty <- function(v) {
  
  if (is.null(v)) return(TRUE)
  
  if (inherits(v, "SpatRaster")) {
    stop("Use raster_is_empty() for SpatRaster")
  }
  
  if (inherits(v, "SpatVector")) {
    return(terra::nrow(v) == 0)
  }
  
  if (inherits(v, "sf")) {
    return(nrow(v) == 0)
  }
  
  return(length(v) == 0)
}
