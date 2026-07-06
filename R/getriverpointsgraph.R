#' Calculation of river points
#' @export
getriverpointsgraph <- function(reservoir,
                           pourpoints,
                           ppid,
                           river_distance = 100000,
                           ac_tolerance = 2,
                           e_tolerance = 10,
                           nn = 100,
                           fac,
                           dem,
                           too_small_river_value = 50,
                           branch_min_ac = 1000,
                           branch_ratio_min = 0.05,
                           branch_persistence_distance = 500,
                           branch_fac_ratio = 10) {
  
  reservoir <- as_vect_safe(reservoir)  # ensure terra
  pourpoints <- as_vect_safe(pourpoints)
  dam_buffer <- buffer(reservoir, river_distance)
  fac_dam <- crop(fac, dam_buffer, snap = "out")
  dem_dam <- crop(dem, dam_buffer, snap = "out")
  fac_dam[fac_dam <= too_small_river_value] <- NA
  pourpoint <- pourpoints[ppid, ]
  crs(pourpoint) <- crs(reservoir)
  
  # Quick validation at pourpoint
  fac_pp <- extract(fac_dam, buffer(pourpoint, 2000))
  mx <- max(fac_pp$mean, na.rm = TRUE)
  if (!is.finite(mx) || mx < 1000) {
    warning("River extension too small")
    return(NULL)
  }
  
  # 2. Prepare points (no heavy directional masking here)
  
  
  points <- as.points(fac_dam)
  names(points) <- "ac"
  points$e <- extract(dem_dam, points, ID = FALSE) 

  if (nrow(points) < 5) {
    warning("Too few river points")
    return(NULL)
  }
  
  points$e <- round(points$e / 10)
  points$id <- seq_len(nrow(points))
  
  # 3. Find starting point
  
  start_id <- which.min(as.numeric(distance(pourpoint, points)))
  adj <- build_river_graph(points, nn = min(nn, nrow(points)))
  
  river_vect <- walk_river_graph(
    adj = adj,
    coords = st_coordinates(st_as_sf(points)),
    ac = points$ac,
    e = points$e,
    start_id = start_id,
    direction = pourpoint$direction,
    ac_tolerance = ac_tolerance,
    e_tolerance = e_tolerance,
    river_distance = river_distance,
    crs_line = st_crs(reservoir)[[2]],
    branch_min_ac = branch_min_ac,
    branch_ratio_min = branch_ratio_min,
    branch_persistence_distance = branch_persistence_distance,
    branch_fac_ratio = branch_fac_ratio)
  
  if (is.null(river_vect) || terra::nrow(river_vect) == 0) {
    warning("No valid river trace")
    return(NULL)
  }
  
  terra::crs(river_vect) <- terra::crs(reservoir)
  
  river_vect
}
