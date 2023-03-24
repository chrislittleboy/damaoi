getstreambuffers <- function(line, reservoir, buffer_size) {
  bufferstream <- st_buffer(line, buffer_size)
  intersection <- st_intersection(reservoir, bufferstream, dimension = "polygon")
  stream <- st_difference(bufferstream, intersection, dimension = "polygon") %>% st_cast("POLYGON")
  stream$area <- stream %>% st_make_valid() %>% st_area()
  stream <- stream[stream$area == max(stream$area),]
  return(stream)
}
