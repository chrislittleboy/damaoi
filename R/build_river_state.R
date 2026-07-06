build_river_state <- function(points) {
  
  if (is.null(points) || nrow(points) == 0) {
    stop("points is empty")
  }
  
  coords <- sf::st_coordinates(points)
  
  if (is.null(coords) || nrow(coords) == 0) {
    stop("invalid coordinates extracted from points")
  }
  
  ac <- points$ac
  e  <- points$e
  
  # HARD SAFETY CHECKS
  if (length(ac) != nrow(coords)) stop("ac/coords mismatch")
  if (length(e)  != nrow(coords)) stop("e/coords mismatch")
  
  list(
    coords = coords,
    ac = ac,
    e = e
  )
}