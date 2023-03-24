#' @export
#' @import smoothr
#' @import dplyr
#' @import tidyr
#' @import terra
#' @import sf
#' @import FNN
#' @import fasterize

getimpactedarea <- function(reservoir,
                          poss_expand = 20000,
                          river_distance = 100000,
                          nn = 100,
                          ac_tolerance = 2,
                          e_tolerance = 5,
                          streambuffersize = 2000,
                          reservoirbuffersize = 5000) {

  down <- getriverpoints(reservoir = reservoir,
                         direction = "downstream",
                         river_distance = river_distance,
                         nn = nn,
                         ac_tolerance = ac_tolerance,
                         e_tolerance = e_tolerance,
                         dams = dams,
                         fac = fac,
                         dem = dem)
  up <- getriverpoints(reservoir = reservoir,
                       direction = "upstream",
                       river_distance = river_distance,
                       nn = nn,
                       ac_tolerance = ac_tolerance,
                       e_tolerance = e_tolerance,
                       dams = dams,
                       fac = fac,
                       dem = dem)

  downline <- getline(down)
  upline <- getline(up)
  colnames(downline)[1] <- colnames(upline)[1] <- "geometry"
  st_geometry(downline) <- st_geometry(upline) <- "geometry"

  reservoir <- dams %>% filter(name == dam) %>% st_make_valid()
  reservoir <- getsmoothreservoirpolygon(reservoir, water_bodies, poss_expand) %>% select()
  basearea <- rbind(damsmooth,upline,downline)
  impactedarea <- cliptobasinandbuffers(damsmooth, upline, downline,basins,streambuffersize,reservoirbuffersize)
  impactedarea <- smooth(impactedarea, method = "ksmooth", smoothness = 3)
  return(impactedarea)
}
