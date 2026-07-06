build_river_graph <- function(points, nn) {
  points <- st_as_sf(points)
  coords <- sf::st_coordinates(points)
  n <- nrow(coords)
  
  if (n < 2) {
    warning("Not enough points for graph")
    return(vector("list", n))
  }
  
  nn_use <- min(max(5, nn), n)   # ensure at least some neighbors
  
  nd <- RANN::nn2(coords, coords, k = nn_use)
  
  adj <- vector("list", n)
  
  for (i in seq_len(n)) {
    if (nn_use < 2) {
      adj[[i]] <- list(id = integer(0), dist = numeric(0), ac = numeric(0), e = numeric(0))
      next
    }
    
    neigh_idx  <- nd$nn.idx[i, 2:nn_use, drop = TRUE]
    neigh_dist <- nd$nn.dists[i, 2:nn_use, drop = TRUE]
    
    valid <- !is.na(neigh_idx) & neigh_idx > 0 & neigh_idx <= n
    neigh <- neigh_idx[valid]
    
    if (length(neigh) == 0) {
      adj[[i]] <- list(id = integer(0), dist = numeric(0), ac = numeric(0), e = numeric(0))
      next
    }
    
    adj[[i]] <- list(
      id   = as.integer(neigh),
      dist = as.numeric(neigh_dist[valid]),
      ac   = as.numeric(points$ac[neigh]),
      e    = as.numeric(points$e[neigh])
    )
  }
  adj
}
