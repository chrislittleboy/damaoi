walk_river_graph <- function(adj,
                             coords,
                             ac,
                             e,
                             start_id,
                             direction,
                             ac_tolerance,
                             e_tolerance,
                             river_distance,
                             crs_line,
                             branch_min_ac,
                             branch_ratio_min,
                             branch_persistence_distance,
                             branch_fac_ratio) {
  
  if (direction == 1) {
    
    return(
      walk_mainstem(
        adj = adj,
        coords = coords,
        ac = ac,
        e = e,
        start_id = start_id,
        ac_tolerance = ac_tolerance,
        e_tolerance = e_tolerance,
        river_distance = river_distance,
        crs_line = crs_line
      )
    )
  }
  
  return(
    walk_network(
      adj = adj,
      coords = coords,
      ac = ac,
      e = e,
      start_id = start_id,
      ac_tolerance = ac_tolerance,
      e_tolerance = e_tolerance,
      river_distance = river_distance,
      crs_line = crs_line,
      branch_min_ac = branch_min_ac,
      branch_ratio_min = branch_ratio_min,
      branch_persistence_distance = branch_persistence_distance,
      branch_fac_ratio = branch_fac_ratio
    )
  )
}
