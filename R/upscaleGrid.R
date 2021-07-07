#     upscaleGrid.R Horizontal resolution upscaling
#
#     Copyright (C) 2018 Santander Meteorology Group (http://www.meteo.unican.es)
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


#' @title Horizontal grid upscaling
#' @description Aggregates a grid along the target dimensions using user-defined functions.
#' @param grid a grid or multigrid to be aggregated.
#' @param times Degradation times. For instance, if the resolution of \code{grid}
#' is 1 degree and \code{times = 2}, the resolution of the output grid will be 2 
#' degrees.
#' @param aggr.fun Upscale function. Default retains the maximum value.
#' @return A grid or multigrid aggregated along the chosen dimension(s).
#' 
#' @details The development of a more eficient and fast version of the function is planned.
#' 
#' @author M. Iturbide 
#' @export
#' @examples \dontrun{
#' require(climate4R.datasets)
#' data("EOBS_Iberia_pr")
#' library(visualizeR)
#' spatialPlot(climatology(EOBS_Iberia_pr))
#' newgrid <- upscaleGrid(redim(EOBS_Iberia_pr), 
#'                        times = 2,
#'                        aggr.fun = list(FUN = "min", na.rm = TRUE))
#' spatialPlot(climatology(newgrid))
#' }


upscaleGrid <- function(grid, times = 5,
                        aggr.fun = list(FUN = max, na.rm = TRUE)) {
      x <- grid$xyCoords$x
      fac0 <- rep(1:floor(length(x)/times), each = times)
      nfac <- length(x) - length(fac0)
      indfac <- max(fac0) + 1
      fac <- c(fac0, rep(indfac, nfac))
      coords <- lapply(split(x, fac), function(k) range(k))
      newcoords <- unlist(lapply(split(x, fac), function(k) mean(k)))
      newcoords[indfac] <- newcoords[(indfac - 1)] + (newcoords[(indfac - 1)] - newcoords[(indfac - 2)])
      grid.list <- lapply(coords, function(k) subsetGrid(grid, lonLim = k))
      suppressMessages(suppressWarnings(
            grid.list.lon <- lapply(grid.list, function(k) aggregateGrid(k, aggr.lon = aggr.fun))
      ))
      grid <- bindGrid(grid.list.lon, dimension = "lon")
      grid$xyCoords$x <- unname(newcoords)
      y <- grid$xyCoords$y
      fac0 <- rep(1:floor(length(y)/times), each = times)
      nfac <- length(y) - length(fac0)
      indfac <- max(fac0) + 1
      fac <- c(fac0, rep(indfac, nfac))
      coords <- lapply(split(y, fac), function(k) range(k))
      newcoords <- unlist(lapply(split(y, fac), function(k) mean(k)))
      newcoords[indfac] <- newcoords[(indfac - 1)] + (newcoords[(indfac - 1)] - newcoords[(indfac - 2)])
      grid.list <- lapply(coords, function(k) subsetGrid(grid, latLim = k))
      suppressMessages(suppressWarnings(
            grid.list.lat <- lapply(grid.list, function(k) aggregateGrid(k, aggr.lat = aggr.fun, weight.by.lat = FALSE))
      ))
      grid <- bindGrid(grid.list.lat, dimension = "lat")
      grid$xyCoords$y <- unname(newcoords)
      if (!is.null(attr(grid$xyCoords, "resX"))) attr(grid$xyCoords, "resX") <- attr(grid$xyCoords, "resX") * times
      if (!is.null(attr(grid$xyCoords, "resY"))) attr(grid$xyCoords, "resY") <- attr(grid$xyCoords, "resY") * times
      return(grid)
}


