cliptobasinandbuffers <- function(reservoir,upstream,downstream,basins,streambuffersize,reservoirbuffersize){

  basearea <- rbind(reservoir,upstream,downstream)

  bufferreservoir <- st_buffer(reservoir, reservoirbuffersize)

  usb <- getstreambuffers(line = upstream,reservoir = bufferreservoir, buffer_size = streambuffersize)[,1]
  dsb <- getstreambuffers(line = downstream, reservoir = bufferreservoir, buffer_size = streambuffersize)[,1]
  impactedarea <- rbind(bufferreservoir,dsb,usb)

  cropbasin <- basins[impactedarea,]
  cropbasin$id <- 1:nrow(cropbasin)

  intersectsbasearea <- cropbasin[cropbasin$id %in% st_intersection(cropbasin, st_make_valid(st_union(basearea)))$id,]
  dissolveintersectsbasearea <- st_as_sf(st_union(intersectsbasearea))
  clippedbybasin <- st_intersection(impactedarea, dissolveintersectsbasearea)

  return(clippedbybasin)
}
