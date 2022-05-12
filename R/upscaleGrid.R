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
      if (nfac != 0)  newcoords[indfac] <- newcoords[(indfac - 1)] + (newcoords[(indfac - 1)] - newcoords[(indfac - 2)])
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
      if (nfac != 0)  newcoords[indfac] <- newcoords[(indfac - 1)] + (newcoords[(indfac - 1)] - newcoords[(indfac - 2)])
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


#end


#' @title Grid data upscaling for each grid-box
#' @description Applies an aggregation function (typically the mean) to each grid-box by considering surrounding grid-boxes.
#' @param grid a grid or multigrid.
#' @param times numeric. An odd number.
#' @param aggr.fun Spatial aggregation function. A list indicating the name of the
#'  aggregation function in first place, and other optional arguments to be passed to the aggregation function.
#'  To be on the safe side, the function in \code{FUN} should be always indicated as a character string. 
#' @param weight.by.lat Logical. Should latitudinal averages be weighted by the cosine of latitude?.
#' Default to \code{TRUE}. Ignored if a function different from \code{"mean"} is applied.
#' @template templateParallelParams
#' @return A grid or multigrid of the same resolution.
#' @template templateParallel
#' @author M. Iturbide
#' @export

upscaleData <- function(grid, 
                        times = 5,
                        aggr.fun = list(FUN = "mean", na.rm = TRUE),
                        weight.by.lat = TRUE,
                        parallel = FALSE,
                        max.ncores = 16,
                        ncores = NULL) {
  aggr.list <- list("aggr.fun" = aggr.fun, "weight.by.lat" = weight.by.lat, "parallel" = parallel, "max.ncores" = max.ncores, "ncores" = ncores)
  if ((times %% 2) != 1) {
    warning("argument times must be an odd number. times set to ", floor(times) + 1) 
    times <- floor(times) + 1
  }
  grid <- redim(grid)
  out <- grid
  out$Data <- out$Data * NA
  message("[", Sys.time(), "] - Aggregating data...This process may tke several minutes.")
  for(i in 1:getShape(grid, "lat")) {
    print(i)
    for(j in 1:getShape(grid, "lon")) {
      grid.aux <- grid
      indlat <- (i - ((times-1)/2)) : (i + ((times-1)/2))
      indlon <- (j - ((times-1)/2)) : (j + ((times-1)/2))
      indlat <- indlat[indlat > 0 & indlat < getShape(grid, "lat")]
      indlon <- indlon[indlon > 0 & indlon < getShape(grid, "lon")]
      grid.aux$Data <- grid$Data[,,indlat, indlon, drop = FALSE]
      attr(grid.aux$Data, "dimensions") <- getDim(grid)
      grid.aux$xyCoords$y <- grid$xyCoords$y[indlat]
      grid.aux$xyCoords$x <- grid$xyCoords$y[indlon]
      aggr.list[["grid"]] <- grid.aux
      grid.aux <- NULL
      fun <- spatialAggregation
      out$Data[,,i, j] <- suppressMessages(do.call("fun", aggr.list))$Data
      }
  }
 return(out)
}

#End