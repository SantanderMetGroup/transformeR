#     bindGrid.member.R Grid binding by member dimension
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

#' @title Grid binding by member dimension
#' @description Flexible binding of (spatiotemporally consistent) grids by their member dimension
#'  useful to handle sets of predictors as a single block.
#' @param ... Input grids to bind by their member dimension. These must be compatible in time and space (see details).
#' @param spatial.tolerance numeric. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @details 
#' 
#' The function can be useful for handling loading large domains and multimembers, that are difficult or impossible to load
#'  at once with the loading functions. The task of loading can be efficiently parallelized by splitting the request by
#'  ensemble member subsets, and then binded with this function. 
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
#' @author J Bedia
#' @export

bindGrid.member <- function(..., spatial.tolerance = 1e-3) {
      grid.list <- list(...)
      if (length(grid.list) < 2) {
            stop("The input must be a list of at least two grids")
      }
      grid.list <- lapply(grid.list, "redim")
      # Disaggregation in single members
      mem.index <- sapply(grid.list, "getShape", "member")
      aux.list <- list()
      for (h in 1:length(grid.list)) {
            subgrid <- grid.list[[h]]
            n.mem <- mem.index[h]
            for (i in 1:n.mem) {
                  aux.list[[length(aux.list) + 1]] <- redim(subsetGrid(subgrid, members = i, drop = F))
            }
      }
      grid.list <- aux.list
      aux.list <- NULL
      tol <- spatial.tolerance
      for (i in 2:length(grid.list)) {
            # Spatial test
            if (!isTRUE(all.equal(grid.list[[1]]$xyCoords, grid.list[[i]]$xyCoords, check.attributes = FALSE, tolerance = tol))) {
                  stop("Input data is not spatially consistent")
            }
            # temporal test
            if (!identical(as.POSIXlt(grid.list[[1]]$Dates$start)$yday, as.POSIXlt(grid.list[[i]]$Dates$start)$yday) 
                | !identical(as.POSIXlt(grid.list[[1]]$Dates$start)$year, as.POSIXlt(grid.list[[i]]$Dates$start)$year)) {
                        stop("Input data is not temporally consistent")
            }
            # data dimensionality test
            if (!identical(getShape(grid.list[[1]]), getShape(grid.list[[i]]))) {
                  stop("Incompatible data array dimensions")
            }
            if (!identical(getDim(grid.list[[1]]), getDim(grid.list[[i]]))) {
                  stop("Inconsistent 'dimensions' attribute")
            }
      }
      #Backwards compatibility fix
      if (!is.list(grid.list[[1]][["InitializationDates"]])) {
            for (i in 1:length(grid.list)) {
                  grid.list[[i]][["InitializationDates"]] <- list(grid.list[[i]][["InitializationDates"]])
            }
      }
      ref <- grid.list[[1]]
      dimNames <- getDim(ref) 
      dim.bind <- grep("member", dimNames)
      data.list <- lapply(grid.list, FUN = "[[", "Data")
      inits.list <- vapply(grid.list, FUN.VALUE = list(1L), FUN = "[[", "InitializationDates")
      member.list <- vapply(grid.list, FUN.VALUE = character(1L), FUN = "[[", "Members")
      grid.list <- NULL
      ref[["Members"]] <- member.list
      ref[["InitializationDates"]] <- inits.list
      names(ref[["InitializationDates"]]) <- member.list
      ref[["Data"]] <- unname(do.call("abind", c(data.list, along = dim.bind)))
      inits.list <- member.list <- data.list <- NULL
      attr(ref[["Data"]], "dimensions") <- dimNames
      return(ref)
}




