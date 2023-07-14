getminmaxatdam <- function(x) {
# takes x, a one-band flow accumulation in rast format
  x <-   as.data.frame(x, xy = T)
# makes the column names standard
  colnames(x) <- c("x", "y", "var")
# arranges by flow accumulation
  x <- x %>% arrange(.data$var) %>%  
# calculates the difference with the last value
    mutate(diffvar = c(0,diff(.data$var)))
# selects the minimum (the place where the difference in accumulated flow is the highest (pour point in))
# and the maximum (place where the accumulated flow is the highest, pour point out)
  minmax <- rbind(x[x$diffvar == max(x$diffvar),1:3], 
                  x[x$var == max(x$var),1:3])
  return(minmax)
}
