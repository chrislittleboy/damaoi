safe_extract <- function(r, p, buffer = 1000, name = "extract") {
  
  v <- terra::extract(r, terra::buffer(p, buffer))
  
  if (is.null(v) || nrow(v) == 0) {
    warning(paste(name, "returned empty"))
    return(NULL)
  }
  
  v
}
