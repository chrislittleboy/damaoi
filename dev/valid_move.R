valid_move <- function(from_ac, from_e,
                       to_ac, to_e,
                       direction,
                       ac_tolerance,
                       e_tolerance) {
  
  if (direction == 1) {
    return(
      to_ac >= from_ac &&
        to_e <= from_e + e_tolerance &&
        to_ac / ac_tolerance <= from_ac
    )
  }
  
  return(
    to_ac <= from_ac &&
      to_e >= from_e - e_tolerance &&
      to_ac * ac_tolerance >= from_ac
  )
}
