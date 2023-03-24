getriverpoints <- function(reservoir,
                           direction,
                           river_distance,
                           ac_tolerance,
                           e_tolerance,
                           nn,
                           dams = dams,
                           fac = fac,
                           dem = dem) {
  # creates a buffer of 'river_distance' meters around the dam
  dam_buffer <- st_buffer(reservoir, river_distance)
  # crops the flow accumulation raster to the dam buffer
  fac_dam <- crop(fac, dam_buffer, snap = "out")
  # removes low/insignificant values of flow accumulation
  fac_dam[fac_dam <= 50] <- NA
  # crops the dem to the dam buffer
  dem_dam <- crop(dem, dam_buffer, snap = "out")
  # creates a raster for the dam extent
  dam_binary <- rast(fasterize(reservoir, raster(fac_dam)))
  fac_damextent <- fac_dam * dam_binary
  dem_damextent <- dem_dam * dam_binary
  facminmax <- getminmaxatdam(fac_damextent)
  if(direction == "downstream") {
    # removes upstream
    fac_dam[fac_dam <= facminmax[[3]][2]] <- NA
    # creates a relevant river binary mask
    fac_dam_binary <- fac_dam
    fac_dam_binary[!is.na(fac_dam_binary)] <- 1
    # removes upstream areas from the DEM
    dem_dam[dem_dam >= minmax(dem_damextent)[2]] <- NA
    # extracts elevations for river pixels
    dem_dam <- dem_dam * fac_dam_binary
    # creates a binary mask for dems
    dem_dam_binary <- dem_dam
    dem_dam_binary[!is.na(dem_dam_binary)] <- 1
    # removes upstream areas from river raster
    fac_dam <- fac_dam * dem_dam_binary
  } else {
    fac_dam[fac_dam >= facminmax[[3]][1]] <- NA
    # creates a relevant river binary mask
    fac_dam_binary <- fac_dam
    fac_dam_binary[!is.na(fac_dam_binary)] <- 1
    # removes upstream areas from the DEM
    dem_dam[dem_dam <= minmax(dem_damextent)[1]] <- NA
    # extracts elevations for river pixels
    dem_dam <- dem_dam * fac_dam_binary
    # creates a binary mask for dems
    dem_dam_binary <- dem_dam
    dem_dam_binary[!is.na(dem_dam_binary)] <- 1
    # removes upstream areas from river raster
    fac_dam <- fac_dam * dem_dam_binary
  }
  fac_dam[fac_dam <= 1000] <- NA
  dam_sf <- terra::as.data.frame(fac_dam, xy = T) %>% st_as_sf(coords = c("x","y")) %>% drop_na() %>% rename(ac = hyd_glo_acc_15s)
  # matches the crs with the dam crs
  st_crs(dam_sf) <- st_crs(reservoir)
  # calculates the distance of each point to the dam
  if(direction == "downstream") { startpoint <- st_as_sf(facminmax, coords = 1:2)[2,]}
  if(direction == "upstream") { startpoint <- st_as_sf(facminmax, coords = 1:2)[1,]}
  st_crs(startpoint) <- st_crs(reservoir)
  # extracts the elevation information
  dem_sf <- terra::as.data.frame(dem_dam, xy = T) %>% st_as_sf(coords = c("x","y")) %>% drop_na() %>% rename(e = hyd_glo_dem_15s)
  st_crs(dem_sf) <- st_crs(reservoir)
  #joins this with the accumulation information
  points <- st_join(dam_sf, dem_sf)
  points$e <- round(points$e/10)
  # initialises the output value data frame ready to be populated
  points$dtostart <- as.numeric(st_distance(startpoint, dam_sf))
  output <- cbind(rep(NA,nrow(dam_sf)),rep(NA,nrow(dam_sf)),rep(NA,nrow(dam_sf)),rep(NA,nrow(dam_sf)),rep(NA,nrow(dam_sf),),rep(NA,nrow(dam_sf)))
  #  initialise things
  points$id <- 1:nrow(points)
  # find midpoint of all points
  centres <- apply(matrix(unlist(points$geometry), ncol = 2, byrow = T), FUN = "mean", MARGIN = 2)
  latitude <- centres[2]
  longitude <- centres[1]
  # find utm zone based on midpoint
  espg <- getutm(latitude,longitude)
  points <- st_transform(points, espg)
  closest <- points[points$dtostart == min(points$dtostart),]
  distance <- 0
  incrementor <- 1
  damnewcrs <- st_transform(reservoir, espg)

  while(incrementor < nrow(dam_sf)){
    mp <- matrix(unlist(points$geometry), ncol = 2, byrow = T)
    nd <- get.knnx(mp,mp,ifelse(nrow(mp) <= nn, nrow(mp), nn))
    if(incrementor == 1){
      pl <- points[points$id == closest$id,]
      pl$d <- 0
    } else{
      pl <- points[points$id == pn$id,]
    }
    if(direction == "downstream") {pn <- getnextpoint(points,pl,nd,ac_tolerance,e_tolerance,"downstream",nn)}
    if(direction == "upstream") {pn <- getnextpoint(points,pl,nd,ac_tolerance,e_tolerance,"upstream",nn)}
    if(nrow(pn) != 1) {break}
    if(!is.numeric(pn$d)) {break}
    distance <- distance + pn$d
    if(distance >= river_distance) {break}
    flowchange <- (pn$ac-pl$ac)/pl$ac
    if(!is.numeric(flowchange)){break}
    output[incrementor,1:2] <- pl$geometry[[1]][1:2]
    output[incrementor,3] <- pl$d
    output[incrementor,4] <- distance
    output[incrementor,5] <- pl$ac
    output[incrementor,6] <- flowchange
    points <- points[points$id != pl$id,]
    if(direction == "downstream") {points <- points[points$ac >= pl$ac,]}
    if(direction == "upstream") {points <- points[points$ac <= pl$ac,]}
    points$id <- 1:nrow(points)
    newid <- which(points$geometry == pn$geometry)
    pn$id <- newid
    incrementor = incrementor + 1
  }
  output <- output %>% as_tibble() %>% drop_na() %>% mutate(espg = espg)
  colnames(output) <- c("x", "y", "dist", "dist_accum", "flow_accum", "flow_change", "espg")
  return(output)
}
