prepare_river_points <- function(reservoir,
                                 pourpoints,
                                 ppid,
                                 river_distance,
                                 fac,
                                 dem) {
  
  reservoir <- as_vect_safe(reservoir)
  pourpoints <- as_vect_safe(pourpoints)

  dam_buffer <- terra::buffer(reservoir, river_distance)
  
  fac_dam <- safe_crop(fac, dam_buffer, "FAC")
  if (is.null(fac_dam)) return(NULL)
  
  dem_dam <- safe_crop(dem, dam_buffer, "DEM")
  if (is.null(dem_dam)) return(NULL)
  
  fac_dam[fac_dam <= 50] <- NA
  
  dam_binary <- terra::rasterize(
    as_vect_safe(reservoir),
    fac_dam
  )
  
  pourpoint <- pourpoints[ppid, ]
  terra::crs(pourpoint) <- terra::crs(reservoir)
  
  fac_pp <- safe_extract(fac_dam, pourpoint, buffer = 1000, name = "FAC_PP")
  if (is.null(fac_pp)) return(NULL)
  
  mx <- suppressWarnings(max(fac_pp[,2], na.rm = TRUE))
  
  if (!is.finite(mx) || mx < 1000) {
    warning("Invalid river extent")
    return(NULL)
  }
  
  fac_dam <- fac_dam * dam_binary
  dem_dam <- terra::resample(dem_dam, dam_binary, method = "bilinear") * dam_binary
  
  fac_sf <- terra::as.data.frame(fac_dam, xy = TRUE) |>
    sf::st_as_sf(coords = c("x","y")) |>
    tidyr::drop_na()
  
  dem_sf <- terra::as.data.frame(dem_dam, xy = TRUE) |>
    sf::st_as_sf(coords = c("x","y")) |>
    tidyr::drop_na()
  
  if (nrow(fac_sf) == 0 || nrow(dem_sf) == 0) {
    warning("Empty sf after raster conversion")
    return(NULL)
  }
  
  sf::st_crs(fac_sf) <- sf::st_crs(reservoir)
  sf::st_crs(dem_sf) <- sf::st_crs(reservoir)
  
  points <- sf::st_join(fac_sf, dem_sf, join = sf::st_intersects)
  
  if (is.null(points) || nrow(points) == 0) {
    warning("No valid joined points")
    return(NULL)
  }
  
  points$e <- round(points$e / 10)
  points$id <- seq_len(nrow(points))
  
  list(
    points = points,
    pourpoint = pourpoint
  )
}
