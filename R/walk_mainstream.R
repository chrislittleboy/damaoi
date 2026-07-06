walk_mainstem <- function(adj,
                          coords,
                          ac,
                          e,
                          start_id,
                          ac_tolerance,
                          e_tolerance,
                          river_distance,
                          crs_line) {
  
  current <- start_id
  distance <- 0
  
  path <- current
  visited <- rep(FALSE, length(adj))
  visited[current] <- TRUE
  
  repeat {
    
    edges <- adj[[current]]
    
    if (is.null(edges) || length(edges$id) == 0)
      break
    
    valid <- logical(length(edges$id))
    
    for (i in seq_along(edges$id)) {
      
      nxt <- edges$id[i]
      
      if (is.na(nxt) ||
          nxt < 1 ||
          nxt > length(adj) ||
          visited[nxt]) {
        next
      }
      
      valid[i] <- valid_move(
        ac[current],
        e[current],
        edges$ac[i],
        edges$e[i],
        1,
        ac_tolerance,
        e_tolerance
      )
    }
    
    valid_idx <- which(valid)
    
    if (length(valid_idx) == 0)
      break
    
    # choose the largest flow accumulation
    best <- valid_idx[
      which.max(edges$ac[valid_idx])
    ]
    
    nxt <- edges$id[best]
    
    distance <- distance + edges$dist[best]
    
    if (distance > river_distance)
      break
    
    path <- c(path, nxt)
    
    visited[nxt] <- TRUE
    current <- nxt
  }
  
  if (length(path) < 2)
    return(NULL)
  
  pts <- coords[path, , drop = FALSE]
  
  terra::vect(
    pts,
    type = "lines",
    crs = crs_line
  )
}
