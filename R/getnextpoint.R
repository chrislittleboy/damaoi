getnextpoint <- function(points,pl,nd,ac_tolerance,e_tolerance,direction,nn){
  intersect <- dplyr::intersect
  knn <- ifelse(nrow(points) <= nn, nrow(points), nn)
  if(direction == "downstream"){
    index <- min(
      intersect(intersect(intersect(
        which(
          points$ac[nd$nn.index[pl$id,2:nn]] >= pl$ac),
        which(
          points$e[nd$nn.index[pl$id,2:nn]] <= pl$e)),
        which(points$ac[nd$nn.index[pl$id,2:nn]] / ac_tolerance <= pl$ac)),
        which(
          abs(points$e[nd$nn.index[pl$id,2:nn]]-pl$e) <= e_tolerance))
    )
  }
  if(direction == "upstream") {
    index <- min(
      intersect(intersect(intersect(
        which(
          points$ac[nd$nn.index[pl$id,2:nn]] <= pl$ac),
        which(
          points$e[nd$nn.index[pl$id,2:nn]] >= pl$e)),
        which(
          points$ac[nd$nn.index[pl$id,2:nn]] * ac_tolerance >= pl$ac)),
        which(
          abs(points$e[nd$nn.index[pl$id,2:nn]]-pl$e) <= e_tolerance)
      )
    )
  }
  pn <- points[points$id == nd$nn.index[pl$id,index+1],]
  d <- nd$nn.dist[pl$id,index+1]
  pn$d <- d
  return(pn)
}
