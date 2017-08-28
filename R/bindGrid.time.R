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


#' @title Grid binding by time dimension
#' @description Flexible binding of (spatially/member consistent) grids by their time dimension
#' @param ... Input grids to bind by their time dimension. These must be compatible in members and space (see details).
#' For flexibility, they can be introduced as a list or directly as consecutive arguments.
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
#' \strong{Time dimension ordering}
#' 
#' The "time" dimension will be always returned in ascending order, regardless of the ordering of the input grids.
#' The internal helper \code{\link{sortDim.time}} is applied to this aim.
#' 
#' @importFrom abind abind
#' @family internal.helpers
#' @author M De Felice, J Bedia
#' @export
#' @examples 
#' data("EOBS_Iberia_tas")
#' eobs.1998 <- subsetGrid(EOBS_Iberia_tas, years = 1998)
#' eobs.1999_2000 <- subsetGrid(EOBS_Iberia_tas, years = 1999:2000)
#' eobs1 <- bindGrid.time(eobs.1998, eobs.1999_2000)
#' eobs2 <- bindGrid.time(eobs.1999_2000, eobs.1998)
#' identical(eobs1, eobs2)
#' 
#' # For convenience while programming, it also accepts a list as input:
#' year.list <- list(1998, 1999:2000)
#' grid.list <- lapply(year.list, function(x) {
#'     subsetGrid(EOBS_Iberia_tas, years = x)
#'     # perform any other tasks with the subsets...
#' }) 
#' eobs3 <- do.call("bindGrid.time", grid.list)
#' identical(eobs1, eobs3)
#' 
#' data("CFS_Iberia_tas")
#' # We first diaggregate in various grids with different time periods
#' period1 <- subsetGrid(CFS_Iberia_tas, years = 1983:1985)
#' period2 <- subsetGrid(CFS_Iberia_tas, years = 1986:1988)
#' # Then we aggregate and compare to the original data (containing the full continuous period)
#' bindedGrid <- bindGrid.time(period1, period2)
#' plotClimatology(climatology(bindedGrid), backdrop.theme = "coastline")
#' plotClimatology(climatology(CFS_Iberia_tas), backdrop.theme = "coastline")


bindGrid.time <- function(..., spatial.tolerance = 1e-3) {
    grid.list <- list(...)
    if (length(grid.list) == 1) {
        grid.list <- unlist(grid.list, recursive = FALSE)
    }
    if (length(grid.list) < 2) {
        stop("The input must be a list of at least two grids")
    }
    grid.list <- lapply(grid.list, "redim")
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
    data.list <- NULL
    start.list <- lapply(grid.list, FUN = function(x) {
        getRefDates(x)
    })
    end.list <- lapply(grid.list, FUN = function(x) {
        getRefDates(x, "end")
    })
    grid.list <- NULL
    ref[["Dates"]] = list(start = do.call(c, start.list),
                          end = do.call(c, end.list))
    attr(ref[["Data"]], "dimensions") <- dimNames
    ref %<>% sortDim.time()
    return(ref)
}


#' @title Grid sorting by time
#' @description Returns a grid with time sorted in ascending order. For internal use mainly, 
#' the function is useful for instance when subsetting along time and binding with \code{\link{bindGrid.time}}, 
#' so the user does not need to worry about the ordering of the input.
#' @param grid Input grid
#' @return A grid fo similar characteristics, but with time dimension re-arranged in ascending ordered if needed.
#' @keywords internal
#' @family internal.helpers
#' @importFrom magrittr %<>%
#' @importFrom abind asub 
#' @author J Bedia

sortDim.time <- function(grid) {
    dates <- grid %>% getRefDates() %>% as.Date() %>% as.integer()
    ind <- sort.int(dates, index.return = TRUE)$ix
    attrs <- attributes(grid$Dates)
    grid$Dates %<>% lapply("[", ind)
    attributes(grid$Dates) <- attrs
    dimNames <- getDim(grid)
    time.ind <- grep("^time", dimNames)
    grid$Data %<>% asub(idx = ind, dims = time.ind, drop = FALSE)
    attr(grid$Data, "dimensions") <- dimNames
    return(grid)
}

