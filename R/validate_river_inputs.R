validate_river_inputs <- function(reservoir,
                                  pourpoints,
                                  ppid,
                                  fac,
                                  dem,
                                  ...) {
  
  if (is.null(reservoir)) stop("reservoir is NULL")
  if (is.null(pourpoints)) stop("pourpoints is NULL")
  
  if (!(ppid %in% seq_len(nrow(pourpoints))))
    stop("invalid ppid")
  
  if (!inherits(fac, "SpatRaster"))
    stop("fac must be SpatRaster")
  
  if (!inherits(dem, "SpatRaster"))
    stop("dem must be SpatRaster")
  
  if (!inherits(reservoir, "SpatVector"))
    reservoir <<- terra::vect(reservoir)
  
  if (!inherits(pourpoints, "SpatVector"))
    pourpoints <<- terra::vect(pourpoints)
  
  TRUE
}
