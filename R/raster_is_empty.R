raster_is_empty <- function(r) {
  
  if (is.null(r)) return(TRUE)
  if (!inherits(r, "SpatRaster")) stop("Expected SpatRaster")
  
  v <- terra::global(!is.na(r), fun = sum, na.rm = TRUE)
  
  if (is.null(v) || nrow(v) == 0) return(TRUE)
  
  v[1,1] == 0
}
