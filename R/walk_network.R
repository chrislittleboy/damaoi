walk_network <- function(adj,
                         coords,
                         ac,
                         e,
                         start_id,
                         direction = 0,
                         ac_tolerance,
                         e_tolerance,
                         river_distance,
                         crs_line,
                         branch_min_ac,
                         branch_ratio_min,
                         branch_persistence_distance,
                         branch_fac_ratio) {
  
  n <- length(adj)
  
  if (is.na(start_id) || start_id < 1 || start_id > n)
    stop("Invalid start_id")
  
  visited <- rep(FALSE, n)
  queue <- list(list(id = start_id, dist = 0))
  
  edge_from <- integer()
  edge_to <- integer()
  
  visited[start_id] <- TRUE
  
  while (length(queue) > 0) {
    
    front <- queue[[1]]
    queue <- queue[-1]
    
    current <- front$id
    distance <- front$dist
    
    if (distance >= river_distance)
      next
    
    edges <- adj[[current]]
    
    if (is.null(edges) || length(edges$id) == 0)
      next
    
    max_branch <- max(edges$ac, na.rm = TRUE)
    
    for (i in seq_along(edges$id)) {
      
      nxt <- edges$id[i]
      
      if (is.na(nxt) || nxt < 1 || nxt > n || visited[nxt])
        next
      
      to_ac <- edges$ac[i]
      to_e  <- edges$e[i]
      d     <- edges$dist[i]
      
      if (is.na(to_ac) || is.na(to_e) || is.na(d))
        next
      
      # ---------------------------------------------------------
      # 1. absolute FAC filter
      # ---------------------------------------------------------
      if (to_ac < branch_min_ac)
        next
      
      # ---------------------------------------------------------
      # 2. relative importance filter
      # ---------------------------------------------------------
      if (max_branch > 0 &&
          (to_ac / max_branch) < branch_ratio_min)
        next
      
      # ---------------------------------------------------------
      # 3. persistence filter
      # ---------------------------------------------------------
      if (!branch_survives(
        start_id = nxt,
        adj = adj,
        ac = ac,
        max_distance = branch_persistence_distance,
        fac_ratio_threshold = branch_fac_ratio
      )) {
        next
      }
      
      # ---------------------------------------------------------
      # movement rule
      # ---------------------------------------------------------
      ok <- valid_move(
        ac[current],
        e[current],
        to_ac,
        to_e,
        direction,
        ac_tolerance,
        e_tolerance
      )
      
      if (!ok)
        next
      
      new_dist <- distance + d
      
      if (!is.finite(new_dist) || new_dist > river_distance)
        next
      
      edge_from <- c(edge_from, current)
      edge_to   <- c(edge_to, nxt)
      
      queue <- c(queue, list(list(id = nxt, dist = new_dist)))
      
      visited[nxt] <- TRUE
    }
  }
  
  if (length(edge_from) == 0)
    return(NULL)
  
  lines <- lapply(seq_along(edge_from), function(i) {
    rbind(
      coords[edge_from[i], ],
      coords[edge_to[i], ]
    )
  })
  
  terra::vect(lines, type = "lines", crs = crs_line)
}
