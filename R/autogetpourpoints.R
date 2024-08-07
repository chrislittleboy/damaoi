#' autogetpourpoints
#' 
#' @export
#' @returns An sf multipoint where rivers flow into and out of the reservoir
#' @param reservoir An sf polygon, with an unstandardised raw reservoir
#' @param fac A rast, showing accumulated water flow along river


autogetpourpoints <- function(reservoir, fac){
  dam_buffer <- st_buffer(reservoir, 1000)
  # crops the flow accumulation raster to the dam buffer
  fac_dam <- crop(fac, dam_buffer, snap = "out")
  # removes low/insignificant values of flow accumulation
  fac_dam[fac_dam <= 50] <- NA
  # creates a raster for the dam extent
  dam_binary <- rasterize(vect(dam_buffer), fac_dam)
  x <- fac_dam * dam_binary
  # takes x, a one-band flow accumulation in rast format
  x <-   as.data.frame(x, xy = TRUE)
  # makes the column names standard
  colnames(x) <- c("x", "y", "var")
  # arranges by flow accumulation
  x <- x %>% arrange(.data$var) %>%  
    # calculates the difference with the last value
    mutate(diffvar = c(0,diff(.data$var)))
  # selects the minimum (the place where the difference in accumulated flow is the highest (pour point in))
  # and the maximum (place where the accumulated flow is the highest, pour point out)
  pourpoints <- rbind(x[x$diffvar == max(x$diffvar),1:3], 
                      x[x$var == max(x$var),1:3])
  pourpoints$var <- c(0,1)
  colnames(pourpoints)[3] <- "direction"
  pourpoints <- st_as_sf(pourpoints, coords = c("x", "y"))
  return(pourpoints)
}
