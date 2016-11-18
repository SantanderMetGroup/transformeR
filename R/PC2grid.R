## PC2grid.R Convert a PC to a time series grid

## Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>. 

#' @title Principal Component grid
#' @description Convert a Principal Component to a 'time-series' grid.
#' @param prinCompObj A PCA object as returned by \code{\link{prinComp}}
#' @param var Character string. Name of the variable for which to extract the PC. Defaults to the first variable in \code{prinCompObj}
#' @param pc.idx Index position of the PC to be converted to a grid
#' @return a 'time series' grid (i.e., a grid with dimensions lon and lat of length 1). It can be a multimember.
#' @details The XY coordinates correspond to the centroid of the window used for the PCA analysis. Details on the extent of this
#'  window are provided by the attributes \code{domX} and \code{domY} within the \code{xyCoords} component.
#'  
#'  Currently, only the start days are returned. The end dates are not returned by the \code{prinComp} output.
#' @export
#' @importFrom abind abind
#' @importFrom stats median
#' @examples \dontrun{
#' data("tasmax_forecast")
#' pc <- prinComp(tasmax_forecast, n.eofs = 2)
#' a <- PC2grid(prinCompObj = pc, pc.idx = 2)
#' str(a)
#' }

PC2grid <- function(prinCompObj, var = names(prinCompObj)[1], pc.idx = 1) {
      attrs <- attributes(prinCompObj)
      var.ind <- match(var, attrs$names)
      if (is.na(var.ind)) stop("Variable name '", var,"' not found", call. = FALSE)
      pcobj <- prinCompObj[[var.ind]]
      prinCompObj <- NULL
      if (pc.idx > ncol(pcobj[[1]]$PCs)) stop("Requested PC not available. Maximum number of available PCs: ", ncol(pcobj[[1]]$PCs), call. = FALSE)
      Variable <- list("varName" = attrs$names[var.ind], "level" = NULL)
      attr(Variable, "definition") <- paste("Principal Component", ncol(pcobj[[1]]$PCs))
      attr(Variable, "units") <- "none"
      mem.list <- lapply(1:length(pcobj), function(x) {
            pcobj[[x]]$PCs[,pc.idx]
      })
      Data <- unname(do.call("abind", c(mem.list, along = -1L)))
      attr(Data, "dimensions") <- c("member", "time")
      mem.list <- NULL
      if (length(attrs$xCoords) > 1) {
            x <- median(attrs$xCoords)
            domX <- range(attrs$xCoords)
      } else {
            x <- attrs$xCoords
            domX <- NULL
      }
      if (length(attrs$yCoords) > 1) {
            y <- median(attrs$yCoords)
            domY <- range(attrs$yCoords)
      } else {
            x <- attrs$xCoords
            domY <- NULL
      }
      xyCoords <- list("x" = x, "y" = y)
      if (!is.null(domX)) attr(xyCoords, "domX") <- domX
      if (!is.null(domY)) attr(xyCoords, "domY") <- domY
      Dates <- list("start" = attrs$dates_start, "end" = NA)      
      out <- list("Variable" = Variable, "Data" = Data, "xyCoords" = xyCoords, "Dates" = Dates)
      attr(out, "timeseries") <- "pc"
      out <- redim(out)
      return(out)
}



