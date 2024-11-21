#     refineGrid.R Horizontal resolution refining
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


#' @title Horizontal grid refining
#' @description  Increasing the grid coordinate density without altering the original pixel values.
#' @param grid a grid or multigrid to be aggregated.
#' @param times Refinement times. For instance, if the resolution of \code{grid}
#' is 1 degree and \code{times = 2}, the resolution of the output grid will be 0.5 
#' degrees.
#' @return A grid or multigrid.
#' 
#' @author M. Iturbide 
#' @export
#' @examples \dontrun{
#' require(climate4R.datasets)
#' data("EOBS_Iberia_pr")
#' library(visualizeR)
#' spatialPlot(climatology(EOBS_Iberia_pr))
#' newgrid <- refineGrid(redim(EOBS_Iberia_pr), 
#'                        times = 2)
#' spatialPlot(climatology(newgrid))
#' }


refineGrid <- function(grid, times = 5) {
  x <- grid$xyCoords$x
  res <- (x[2] - x[1]) / times
  new.x <- seq((x[1] - (res * (times - 1))), (rev(x)[1] + (res * (times - 1))), res)
  y <- grid$xyCoords$y
  res <- (y[2] - y[1]) / times
  new.y <- seq((y[1] - (res * (times - 1))), (rev(y)[1] + (res * (times - 1))), res)
  out <- interpGrid(grid, new.coordinates = list(x = new.x, y = new.y))
  return(out)
}

#end


