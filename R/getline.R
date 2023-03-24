getline <- function(x) {
  x <- x %>%
    mutate(cum_flow_accum = flow_accum/min(flow_accum)) %>%
    st_as_sf(coords = c("x", "y"),
             crs = x$espg[1]) %>%
    st_transform(4326)
  line <- data.frame(x = 1:nrow(x), y = 1:nrow(x))
  i <- 1
  while(i <= nrow(x)){
    line$x[i] <-x$geometry[[i]][1]
    line$y[i] <-x$geometry[[i]][2]
    i <- i + 1
  }
  line <- st_as_sf(st_sfc(st_linestring(as.matrix(line))))
  st_crs(line) <- 4326
  return(line)
}
