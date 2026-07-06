branch_survives <- function(start_id,
                            adj,
                            ac,
                            max_distance,
                            fac_ratio_threshold) {
  
  current <- start_id
  visited <- rep(FALSE, length(adj))
  visited[current] <- TRUE
  
  total_dist <- 0
  
  max_ac <- ac[current]
  min_ac <- ac[current]
  
  repeat {
    
    edges <- adj[[current]]
    if (is.null(edges) || length(edges$id) == 0)
      break
    
    # pick strongest continuation (greedy)
    valid <- which(!visited[edges$id] & !is.na(edges$ac))
    
    if (length(valid) == 0)
      break
    
    best <- valid[which.max(edges$ac[valid])]
    
    nxt <- edges$id[best]
    d <- edges$dist[best]
    
    total_dist <- total_dist + d
    
    if (!is.finite(total_dist) || total_dist > max_distance)
      break
    
    max_ac <- max(max_ac, ac[nxt], na.rm = TRUE)
    min_ac <- min(min_ac, ac[nxt], na.rm = TRUE)
    
    visited[nxt] <- TRUE
    current <- nxt
  }
  
  if (min_ac == 0 || is.na(min_ac))
    return(FALSE)
  
  fac_ratio <- max_ac / min_ac
  
  return(fac_ratio <= fac_ratio_threshold)
}
