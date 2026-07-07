build_river_graph <- function(points, nn) {
  
  coords <- sf::st_coordinates(points)
  
  n <- nrow(coords)
  
  if (n < 2) stop("Not enough points for graph")
  
  nn_use <- min(nn, n)
  
  nd <- RANN::nn2(coords, coords, k = nn_use)
  
  adj <- vector("list", n)
  
  for (i in seq_len(n)) {
    
    # --- SAFE GUARD 1: skip if no neighbours
    if (nn_use < 2) {
      adj[[i]] <- list(id = integer(0), dist = numeric(0),
                       ac = numeric(0), e = numeric(0))
      next
    }
    
    neigh <- nd$nn.idx[i, 2:nn_use]
    
    # --- SAFE GUARD 2: remove invalid indices
    neigh <- neigh[!is.na(neigh) & neigh > 0 & neigh <= n]
    
    if (length(neigh) == 0) {
      adj[[i]] <- list(id = integer(0),
                       dist = numeric(0),
                       ac = numeric(0),
                       e = numeric(0))
      next
    }
    
    adj[[i]] <- list(
      id = neigh,
      dist = nd$nn.dists[i, seq_along(neigh) + 1],
      ac = points$ac[neigh],
      e  = points$e[neigh]
    )
  }
  
  adj
}
