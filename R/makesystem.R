#' makesystem

#' @export
#' @returns A set of AOI polygons for the entire system, when dams are part of a system of dams.
#' @param aois An sf polygon, containing all AOI areas for all dams which are part of the system 
#' @param names The names of dams within the same system
#' @param dem An elevation raster covering the extent of the system. This is used to determine the uppermost and lowest dams in the system.
#' @param betweenthreshold The minimum area in km2 considered significant for between areas (to avoid small adjoining polygons already mostly contained in other AOI polygons) 
#' @param bwru Whether the areas between a dam system (i.e. connecting rivers) are within the bounding box of the reservoirs in that system. They will mostly be, though for some dam systems rivers can travel, for example, far to the east then back again to the west to rejoin another dam in the same system.

makesystem <- function(names, aois, dem, betweenthreshold = 1, bwru = TRUE) {
  system <- aois[aois$name %in% names,] # gets just aois where dams are part of the same system
  st_agr(system) = "constant"
  elev <- extract(dem, st_transform(st_centroid(system[system$area == "reservoir",]), crs(dem)))
  elev$name <- system$name[system$area == "reservoir"]
  elev <- elev[,2:3]; colnames(elev)[1] <- "elev"
  system <- merge(system, elev)
  uppermost <- system$name[which.max(system$elev)] # gets the highest dam (using elevation)
  lowest <- system$name[which.min(system$elev)] # gets the lowest dam (using elevation)
  # gets the union of all reservoirs in the system
  reservoirunion <- st_union(system[system$area == "reservoir",]) %>% st_as_sf()
  # and all areas around the reservoir which do not intersect with the reservoir
  aroundreservoirs <- system[system$area =="aroundreservoir",]
  st_agr(reservoirunion) = "constant"
  st_agr(aroundreservoirs) = "constant"
  aroundreservoirs <- st_difference(reservoirunion) %>% st_union() %>% st_as_sf()
  downstream <- system$geometry[system$name == lowest & system$area == "downstream",] %>% st_as_sf() # downstream from the lowest reservoir
  upstream <- system$geometry[system$name == uppermost & system$area == "upstream",] %>% st_as_sf() # and upstream from the highest
  all <- st_union(st_union(st_union(reservoirunion, aroundreservoirs), downstream), upstream) # a union of all areas
  between <- st_difference(st_union(st_geometry(system)), all) # gets "between areas": rivers which connect dams in a system
  # adjusts between areas to ...
  between <- between %>% st_as_sf() %>% st_cast("POLYGON")
  between$area <- drop_units(st_area(between))/1000000
  between <- between[between$area > betweenthreshold,] # be above a certain area threshold
  if(bwru == TRUE){
    st_agr(between) = "constant"
    between <- st_crop(between, reservoirunion)
    } # IF the Between areas are Within the Reservoir Union (bwru) 
  # as is the case in almost every conceivable dam system (but not the Maoergai System...), crops the between area to the union of the reservoir.
  # this prevents long thin between areas upriver and downriver of the system which are byproducts of small differences in the calculation of upstream and downstream areas.
  between <- st_union(between) %>% st_as_sf()
  # cooerces between areas into one polygon
  # joins polygons for systems with no between areas
  if(nrow(between) == 0) {
    newsystem <- rbind(reservoirunion, aroundreservoirs, downstream, upstream) %>% st_as_sf()
    newsystem$name <- rep(paste(uppermost, "System"), 4)
    newsystem$area <- c("reservoir", "aroundreservoir", "downstream", "upstream")
  }
  # joins polygons for systems with between areas
  if(nrow(between) == 1) {
    newsystem <- rbind(reservoirunion, aroundreservoirs, downstream, upstream, between) %>% st_as_sf()
    newsystem$name <- rep(paste(uppermost, "System"), 5)
    newsystem$area <- c("reservoir", "aroundreservoir", "downstream", "upstream", "between")
  }
  # properly sets and names geometry column
  st_geometry(newsystem) <- "geometry"
  newsystem <- newsystem[,c(2,3,1)]
  return(newsystem)  
}