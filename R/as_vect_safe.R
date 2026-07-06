as_vect_safe <- function(x) {
  
  if (inherits(x, "SpatVector")) return(x)
  
  terra::vect(x)
}
