getsmoothreservoirpolygon <- function(reservoir, water_bodies, poss_expand, dem, wbjc) {
# creates a buffer zone around the GRanD polygon, size defined by "poss_expand"
  rb <- st_buffer(reservoir, poss_expand)
# crops the water bodies raster by the area  
  wb <- crop(water_bodies, rb)
# strips out the quality band for the water bodies raster
  wb <- wb$WB
# all water is 1, otherwise NA
  wb[!is.na(wb)] <- 1
# crops the dem to the buffered reservoir
  demcrop <- crop(dem, rb)  
# resamples to same res/extent as water bodies
  demcrop <- resample(demcrop, wb)  
# creates a raster version of the original reservoir  
  rr <- rast(fasterize(reservoir, raster(wb)))
# gets the min/max elevation for the reservoir area
  minmaxelev <- rr * demcrop
# and extracts minimums/maximums
  mi <- min(minmaxelev[], na.rm = T)
  ma <- max(minmaxelev[], na.rm = T)
# filters the expand so only areas between the minimum and maximum reservoir area are potentially expandable areas
  demcrop[demcrop > ma] <- NA
  demcrop[demcrop < mi] <- NA
# changes all values to 1 to create a presence/absence raster of eligble elevation pixels
  demcrop[!is.na(demcrop)] <- 1
# and masks the water bodies layer by this
  wb <- wb * demcrop
# we need a polygon, so this extracts the water bodies from the raster
  polywb <- as.polygons(wb)
# wbjc = water body join correction. 
# This is a parameter to correct for erroneously non-contiguous water bodies.
# this won't be needed always, so default is 0. 
# When necessary, it should be assigned the lowest possible value  
  polywb <- buffer(polywb, wbjc)
# Joins the buffered geometries
  polywb <- aggregate(makeValid(polywb))
# Converts to an sf polygon
  polywb <- st_as_sf(polywb) %>% 
    st_cast("POLYGON", warn = F) %>% 
    mutate(id = row_number())
# caluculates the area of all the water bodies  
  polywb$area <- polywb %>% st_area()
# and selects the largest (e.g. the reservoir)
  polywb <- polywb[polywb$area == max(polywb$area),]
# smooths this to get rid of raster edge effects
  smoothres <- smooth(polywb, method = "chaikin")
  smoothres <- st_make_valid(smoothres)
  return(smoothres)
}
