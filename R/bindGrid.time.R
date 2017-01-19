#     bindGrid.time.R Grid binding by member dimension
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

#' @title Grid binding by time dimension
#' @description Flexible binding of (spatial consistent) grids by their time dimension
#' @param ... Input grids to bind by their time dimension. These must be compatible in members and space (see details).
#' @param spatial.tolerance numeric. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @details 
#' 
#' The function makes a number of checks in order to test the spatiotemporal compatibility of the input multi-member grids.
#'  The spatial consistency of the input grids is also checked. In order to avoid possible errors from the user, the spatial
#'   consistency (i.e., equal XY coordinates) of the input grids must be ensured before attempting the creation of the multigrid,
#'   otherwise giving an error. This can be achieved either through the specification of the same 'lonLim' and 'latLim' argument
#'   values when loading the grids, or using the \code{\link{interpGrid}} interpolator in conjuntion with the \code{\link{getGrid}}
#'   method.
#' @examples 
#' data("tasmin_forecast")
#' # We first diaggregate in various grids with different members
#' members1_2 <- subsetGrid(tasmin_forecast, members = 1:2)
#' members3_4 <- subsetGrid(tasmin_forecast, members = 3:4)
#' member7 <- subsetGrid(tasmin_forecast, members = 7)
#' member8 <- subsetGrid(tasmin_forecast, members = 8)
#' # The function is insensitive to the number of members per input grid
#' bindedGrid <- bindGrid.member(members1_2, members3_4, member7, member8)
#' plotClimatology(climatology(bindedGrid), backdrop.theme = "coastline")

#' @importFrom abind abind
#' @author M De Felice, J Bedia
#' @export

bindGrid.time <- function(..., spatial.tolerance = 1e-3) {
  grid.list <- list(...)
  if (length(grid.list) < 2) {
    stop("The input must be a list of at least two grids")
  }
  grid.list <- lapply(grid.list, "redim")
  
  aux.list <- NULL
  tol <- spatial.tolerance
  for (i in 2:length(grid.list)) {
    # Spatial test
    if (!isTRUE(all.equal(grid.list[[1]]$xyCoords, grid.list[[i]]$xyCoords, check.attributes = FALSE, tolerance = tol))) {
      stop("Input data is not spatially consistent")
    }
    # Member
    if (getShape(grid.list[[1]])[match('member', getDim(grid.list[[1]]))] != getShape(grid.list[[i]])[match('member', getDim(grid.list[[i]]))]) {
      stop("Member dimension is not spatially consistent")
    }
  }
  
  ref <- grid.list[[1]]
  dimNames <- getDim(ref) 
  dim.bind <- grep("time", dimNames)
  data.list <- lapply(grid.list, FUN = "[[", "Data")
  ref[["Data"]] <- unname(do.call("abind", c(data.list, along = dim.bind)))
  
  start.list <- lapply(grid.list, FUN = function(x) {
    x$Dates$start
  })
  end.list <- lapply(grid.list, FUN = function(x) {
    x$Dates$end
  })
  grid.list <- NULL
  ref[["Dates"]] = list(start = do.call(c, start.list),
                      end = do.call(c, end.list))
  
  inits.list <- member.list <- data.list <- NULL
  attr(ref[["Data"]], "dimensions") <- dimNames
  return(ref)
}




