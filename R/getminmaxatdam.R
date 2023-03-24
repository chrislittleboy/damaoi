getminmaxatdam <- function(x) {
  x <-   as.data.frame(x, xy = T)
  colnames(x) <- c("x", "y", "var")
  x <- x %>% arrange(var) %>%  
    mutate(diffvar = c(0,diff(var)))
  minmax <- rbind(x[x$diffvar == max(x$diffvar),1:3], 
                  x[x$var == max(x$var),1:3])
  return(minmax)
}