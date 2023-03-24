getmaxextent <- function(path){
  l <- list.files(path, full.names= T)
  e <- c(1000000000,0,1000000000,0)
  i <- 1
  while(i <= length(l)){
    r <- rast(l[i])
    ex <- ext(r)[1:4]
    ext <- c(ex[[1]],ex[[2]],ex[[3]],ex[[4]])
    if(e[1] >= ext[1]){e[1] <- ext[1]}
    if(e[2] <= ext[2]){e[2] <- ext[2]}
    if(e[3] >= ext[3]){e[3] <- ext[3]}
    if(e[4] <= ext[4]){e[4] <- ext[4]}
    i <- i + 1
  }
  e <- ext(e)
  return(e)
}
