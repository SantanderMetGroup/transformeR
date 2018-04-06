##     bindGrid.time.R Grid binding by member dimension
##
##     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.


#' @title Grid binding by spatial dimension
#' @description Flexible binding of (spatially/member consistent) grids by their longitude or latitude dimension
#' @param ... Input grids to bind by their time dimension. These must be compatible in members and space (see details).
#' For flexibility, they can be introduced as a list or directly as consecutive arguments.
#' @param dimension latitude ("lat") or longitude ("lon")
#' @param spatial.tolerance numeric. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @details 
#' 
#' \strong{Input grids consistency checks}
#' 
#' The function makes a number of checks in order to test the spatiotemporal compatibility of the input multi-member grids.
#'  The spatial consistency of the input grids is also checked. In order to avoid possible errors from the user, the spatial
#'   consistency (i.e., equal XY coordinates) of the input grids must be ensured before attempting the creation of the multigrid,
#'   otherwise giving an error. This can be achieved either through the specification of the same 'lonLim' and 'latLim' argument
#'   values when loading the grids, or using the \code{\link{interpGrid}} interpolator in conjuntion with the \code{\link{getGrid}}
#'   method, for instance
#'   
#' 
#' @importFrom abind abind
#' @family internal.helpers
#' @author M Iturbide
#' @export

bindGrid.spatial <- function(...,  dimension = c("lat", "lon"), spatial.tolerance = 1e-3) {
      dimension <- match.arg(dimension, choices = c("lat", "lon"))
      dimsort <- "y"
      if(dimension == "lon") dimsort <- "x"
      grid.list <- list(...)
      if (length(grid.list) == 1) {
            grid.list <- unlist(grid.list, recursive = FALSE)
      }
      if (length(grid.list) < 2) {
            stop("The input must be a list of at least two grids")
      }
      grid.list <- lapply(grid.list, "redim", var = TRUE)
      tol <- spatial.tolerance
      for (i in 2:length(grid.list)) {
            # Temporal test
            if (!isTRUE(all.equal(grid.list[[1]]$Dates, grid.list[[i]]$Dates, check.attributes = FALSE, tolerance = tol))) {
                  stop("Input data is not temporally consistent")
            }
            # Member
            if (getShape(grid.list[[1]])[match('member', getDim(grid.list[[1]]))] != getShape(grid.list[[i]])[match('member', getDim(grid.list[[i]]))]) {
                  stop("Member dimension is not spatially consistent")
            }
      }
      ref <- grid.list[[1]]
      dimNames <- getDim(ref) 
      dim.bind <- grep(dimension, dimNames)
      data.list <- lapply(grid.list, FUN = "[[", "Data")
      ref[["Data"]] <- unname(do.call("abind", c(data.list, along = dim.bind)))
      data.list <- NULL
      lat <- lapply(grid.list, FUN = function(x) {
            getCoordinates(x)[[dimsort]]
      })
      grid.list <- NULL
      lats <- do.call(c, lat)
      attr(ref[["Data"]], "dimensions") <- dimNames
      n.vars <- getShape(ref, "var")
      #if (n.vars > 1) lats <- rep(list(lats), n.vars)
      ref[["xyCoords"]][[dimsort]] <- lats
      # ref %<>% sortDim.spatial()
      redim(ref, drop = TRUE)
      return(ref)
}


#' @title Grid sorting by latitude or longitude
#' @description Returns a grid with time sorted in ascending order. For internal use mainly, 
#' the function is useful for instance when subsetting along time and binding with \code{\link{bindGrid.time}}, 
#' so the user does not need to worry about the ordering of the input.
#' @param grid Input grid
#' @param dimension longitude ("x") or latitude ("y")
#' @return A grid fo similar characteristics, but with time dimension re-arranged in ascending ordered if needed.
#' @keywords internal
#' @family internal.helpers
#' @importFrom magrittr %<>%
#' @importFrom abind asub 
#' @author M. Iturbide

sortDim.spatial <- function(grid, dimension = c("y", "x")) {
      dimension <- match.arg(dimension, choices = c("y", "x"))
      lats <- grid %>% getCoordinates() #%>% as.Date() %>% as.integer()
      lats <- lats[[dimension]]
      ind <- sort.int(lats, index.return = TRUE)$ix
      attrs <- attributes(grid$xyCoords)
      reflats <- getCoordinates(grid)$y
      #reflats %<>% lapply("[", ind)
      n.vars <- getShape(grid, "var")
      #if (n.vars > 1) refdates <- rep(list(refdates), n.vars)
      grid$xyCoords$y <- reflats
      attributes(grid$xyCoords) <- attrs
      dimNames <- getDim(grid)
      lat.ind <- grep("^lat", dimNames)
      grid$Data %<>% asub(idx = ind, dims = lat.ind, drop = FALSE)
      attr(grid$Data, "dimensions") <- dimNames
      return(grid)
}

