walk_river_graph <- function(adj,
                             coords,
                             ac,
                             e,
                             start_id,
                             direction,
                             ac_tolerance,
                             e_tolerance,
                             river_distance) {
  
  n <- length(adj)
  
  # --- hard validation of start node
  if (is.na(start_id) || start_id < 1 || start_id > n) {
    stop("Invalid start_id")
  }
  
  visited <- rep(FALSE, n)
  
  current <- start_id
  distance <- 0
  out_i <- 1
  
  out <- matrix(NA_real_, nrow = n, ncol = 6)
  
  repeat {
    
    # =========================================================
    # 🔒 INVARIANT 1: current must ALWAYS be valid
    # =========================================================
    if (is.na(current) || current < 1 || current > n) break
    if (visited[current]) break
    
    visited[current] <- TRUE
    
    edges <- adj[[current]]
    
    # =========================================================
    # 🔒 INVARIANT 2: must have neighbours
    # =========================================================
    if (is.null(edges) || length(edges$id) == 0) break
    
    found <- FALSE
    
    for (i in seq_along(edges$id)) {
      
      nxt <- edges$id[i]
      
      # =========================================================
      # 🔒 INVARIANT 3: neighbour must be valid index
      # =========================================================
      if (is.na(nxt) || nxt < 1 || nxt > n) next
      if (visited[nxt]) next
      
      # safe attribute access (prevents length-0 bugs)
      to_ac <- edges$ac[i]
      to_e  <- edges$e[i]
      d     <- edges$dist[i]
      
      if (is.na(to_ac) || is.na(to_e) || is.na(d)) next
      
      # =========================================================
      # movement rule (pure numeric logic)
      # =========================================================
      ok <- if (direction == 1) {
        
        to_ac >= ac[current] &&
          to_e <= e[current] + e_tolerance &&
          to_ac / ac_tolerance <= ac[current]
        
      } else {
        
        to_ac <= ac[current] &&
          to_e >= e[current] - e_tolerance &&
          to_ac * ac_tolerance >= ac[current]
        
      }
      
      if (!ok) next
      
      # =========================================================
      # update distance safely
      # =========================================================
      distance <- distance + d
      
      if (!is.finite(distance)) break
      
      if (distance >= river_distance) {
        return(out[seq_len(out_i - 1), , drop = FALSE])
      }
      
      # =========================================================
      # coordinate extraction (NO sf dependency)
      # =========================================================
      xy <- coords[current, , drop = FALSE]
      
      if (length(xy) == 0 || anyNA(xy)) break
      
      # =========================================================
      # flow change (safe numeric)
      # =========================================================
      denom <- ac[current]
      
      if (is.na(denom) || denom == 0) next
      
      flowchange <- (ac[nxt] - ac[current]) / denom
      
      if (!is.finite(flowchange)) next
      
      # =========================================================
      # write output row (guaranteed length-6 vector)
      # =========================================================
      out[out_i, ] <- c(
        xy[1,1],
        xy[1,2],
        d,
        distance,
        ac[current],
        flowchange
      )
      
      # =========================================================
      # advance state (ONLY valid transition allowed)
      # =========================================================
      current <- nxt
      out_i <- out_i + 1
      found <- TRUE
      break
    }
    
    if (!found) break
  }
  
  out[seq_len(out_i - 1), , drop = FALSE]
}