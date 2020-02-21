#     mergeGrid.R Flexible grid aggregation along selected dimensions
#
#     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' @title Flexible grid aggregation along selected dimensions
#' @description Merge multiple grids in space (latitude and longitude).
#' @param grid a grid or multigrid to be aggregated.
#' @param aggr.fun Aggregation function to the intersection areas among grids. The default option computes the mean
#' aggr.fun = list(FUN = "mean", na.rm = TRUE).
#' @template templateParallelParams
#' @return A single grid result of merging the input grids.
#' @details
#' 
#' \strong{Aggregation function definition}
#' 
#' The aggregation functions are specified in the form of a named list of the type \code{FUN = "function", ...}, where
#' \code{...} are further arguments passes to FUN. This allows for a flexible definition of aggregation functions, that are 
#' internally passes to \code{\link{tapply}}. Note that the name of the function is indicated as a character string.
#' 
#' @author J. Baño-Medina, M. Iturbide
#' @export
#' @importFrom magrittr %<>% 
#' @examples 
mergeGrid <- function(...,aggr.fun = list(FUN = "mean", na.rm = TRUE)) {
  grid.list <- list(...)
  if (!isGrid(grid.list[[1]])) grid.list <- unlist(grid.list, recursive = FALSE)
  if (!isGrid(grid.list[[1]])) stop("Wrong input")
  
  res <- sapply(grid.list, FUN = function(z) {
    c(attr(getGrid(z),"resX"),attr(getGrid(z),"resY"))
  }) %>% apply(MARGIN = 2,unique)
  if (is.list(res) | nrow(res) > 1) stop("Grid resolutions do not match")
  
  mins <- sapply(grid.list, FUN = function(z) {
    c(getGrid(z)$x[1],getGrid(z)$y[1])
  }) %>% apply(MARGIN = 2,min)
  
  maxs <- sapply(grid.list, FUN = function(z) {
    c(getGrid(z)$x[1],getGrid(z)$y[1])
  }) %>% apply(MARGIN = 2,max)
  
  lons <- seq(mins[1],maxs[1],res[1])
  lats <- seq(mins[2],maxs[2],res[2])
  
  template <- grid.list[[1]]
  n.mem <- getShape(template,"member")
  lapply(1:n.mem, FUN = function(z){
    lapply(grid.list, FUN = function(zz) {
      zz <- zz %>% redim() %>% subsetGrid(members = z)
      aux <- zz
      aux$Data <- array(dims(getShape(zz,"var"),1,getShape(zz,"time"),length(lats),length(lons)))
      minX <- which(min(zz$xyCoords$x) == lons)
      maxX <- which(max(zz$xyCoords$x) == lons)
      minY <- which(min(zz$xyCoords$y) == lats)
      maxY <- which(max(zz$xyCoords$y) == lats)
      aux$Data[,,,minY:maxY,minX:maxX] <- zz$Data
      aux$xyCoords$x <- c(mins[1],maxs[1]); aux$xyCoords$y <- c(mins[2],maxs[2])
      return(aux)
    }) %>% aggregateGrid(aggr.mem = aggr.fun)
  }) %>% bindGrid(dimension = "member") %>% redim(drop = TRUE)
}