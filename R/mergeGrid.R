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


#' @title Merge multiple grids in space 
#' @description Merge multiple grids in space (latitude and longitude).
#' @param ... Input grids to be merged.
#' @param aggr.fun Aggregation function to the intersection areas among grids. The default option computes the mean
#' aggr.fun = list(FUN = "mean", na.rm = TRUE).
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
#' @examples \donttest{
#' require(climate4R.datasets)
#' # We load the dataset (temperature)
#' data("CFS_Iberia_tas")
#' # We take a look at the domain of the dataset
#' library(visualizeR)
#' spatialPlot(climatology(CFS_Iberia_tas),backdrop.theme = "coastline")
#' getGrid(CFS_Iberia_tas)
#' # We divide the dataset in 2 regions
#' region1 <- subsetGrid(CFS_Iberia_tas,lonLim = c(-10,-6), latLim = c(36,38))
#' spatialPlot(climatology(region1),backdrop.theme = "coastline")
#' region2 <- subsetGrid(CFS_Iberia_tas,lonLim = c(-5,3), latLim = c(37,43))
#' spatialPlot(climatology(region2),backdrop.theme = "coastline")
#' # We merge the regions and apply the mean function to the intersection
#' merged <- mergeGrid(region1,region2,aggr.fun = list(FUN = "mean", na.rm = TRUE))
#' spatialPlot(climatology(merged),backdrop.theme = "coastline")
#' }

mergeGrid <- function(...,aggr.fun = list(FUN = "mean", na.rm = TRUE)) {
  grid.list <- list(...)
  if (!isGrid(grid.list[[1]])) grid.list <- unlist(grid.list, recursive = FALSE)
  if (!isGrid(grid.list[[1]])) stop("Wrong input")
  
  res <- sapply(grid.list, FUN = function(z) {
    c(attr(getGrid(z),"resX"),attr(getGrid(z),"resY"))
  }) %>% apply(MARGIN = 1,unique)
  if (is.list(res) | length(res) > 2) stop("Grid resolutions do not match")
  
  mins <- sapply(grid.list, FUN = function(z) {
    c(getGrid(z)$x[1],getGrid(z)$y[1])
  }) %>% apply(MARGIN = 1,min)
  
  maxs <- sapply(grid.list, FUN = function(z) {
    c(getGrid(z)$x[2],getGrid(z)$y[2])
  }) %>% apply(MARGIN = 1,max)
  
  lons <- seq(mins[1],maxs[1],res[1]) %>% round(digits = 3)
  lats <- seq(mins[2],maxs[2],res[2]) %>% round(digits = 3)
  
  template <- grid.list[[1]] %>% redim(var = TRUE)
  dimNames <- attr(template$Data,"dimensions")
  template$Data <- array(dim = c(getShape(template,"var"),getShape(template,"member"),getShape(template,"time"),length(lats),length(lons)))
  attr(template$Data,"dimensions") <- dimNames
  template$xyCoords$x <- lons; template$xyCoords$y <- lats
  
  
  grid.list <- lapply(grid.list, FUN = function(z) {
    z <- z %>% redim(var = TRUE)
    minX <- which(round(min(z$xyCoords$x),digits = 3) == lons)
    maxX <- which(round(max(z$xyCoords$x),digits = 3) == lons)
    minY <- which(round(min(z$xyCoords$y),digits = 3) == lats)
    maxY <- which(round(max(z$xyCoords$y),digits = 3) == lats)
    template$Data[,,,minY:maxY,minX:maxX] <- z$Data
    attr(template$Data,"dimensions") <- dimNames
    return(template)
  }) 
  lapply(1:getShape(template,"member"), FUN = function(z) {
    lapply(grid.list, FUN = function(zz) {
      subsetGrid(zz,members = z)
    }) %>% bindGrid(dimension = "member") %>% aggregateGrid(aggr.mem = aggr.fun)
  }) %>% bindGrid(dimension = "member") %>% redim(drop = TRUE)
}