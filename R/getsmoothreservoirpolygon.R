getsmoothreservoirpolygon <- function(reservoir, water_bodies, poss_expand) {
  rb <- st_buffer(reservoir, poss_expand)
  wb <- crop(water_bodies, rb)
  wb <- wb$WB
  wb[!is.na(wb)] <- 1
  polywb <- as.polygons(wb)
  polywb <- st_as_sf(polywb) %>%  st_cast("POLYGON") %>% mutate(id = row_number())
  polywb$area <- polywb %>% st_area()
  polywb <- polywb[polywb$area == max(polywb$area),]
  smoothres <- smooth(polywb, method = "chaikin")
  return(smoothres)
}
