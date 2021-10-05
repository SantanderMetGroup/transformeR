setTimeResolution <- function(grid, time_resolution){
  
  time_resolution <- match.arg(time_resolution, c("1h","3h","6h","12h","DD","MM","YY","unknown"))
  attr(grid[["Variable"]], "time_resolution") <- time_resolution
  
  return(grid)
}
