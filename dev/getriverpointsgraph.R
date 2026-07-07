getriverpointsgraph <- function(
    ppid,
    reservoir,
    pourpoints,
    river_distance,
    ac_tolerance,
    e_tolerance,
    nn,
    fac,
    dem
) {
  
  validate_river_inputs(
    reservoir = reservoir,
    pourpoints = pourpoints,
    ppid = ppid,
    fac = fac,
    dem = dem
  )
  
  prep <- prepare_river_points(
    reservoir,
    pourpoints,
    ppid,
    river_distance,
    fac,
    dem
  )
  
  if (is.null(prep)) return(NULL)
  
  points <- prep$points
  pourpoint <- prep$pourpoint
  
  coords <- sf::st_coordinates(points)
  ac <- points$ac
  e <- points$e
  
  start_id <- which.min(
    as.numeric(sf::st_distance(pourpoint, points))
  )
  
  adj <- build_river_graph(points, nn)
  
  walk_river_graph(
    adj = adj,
    coords = coords,
    ac = ac,
    e = e,
    start_id = start_id,
    direction = pourpoint$direction,
    ac_tolerance = ac_tolerance,
    e_tolerance = e_tolerance,
    river_distance = river_distance
  )
}
