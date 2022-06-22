#' @title Grid smoothing.
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

smoothGrid <- function(grid, 
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