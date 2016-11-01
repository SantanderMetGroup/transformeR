#' @title Anomalies
#' @description Recalculates, at the gridbox scale, the grid values in terms of their anomaly w.r.t. the mean
#' @param grid Input grid
#' @template templateParallelParams
#' 
#' @details
#' 
#' The function computes for each grid point (and member if more than one), the temporal mean.
#'  Then, this mean is subtracted to each value in the time series. 
#'  
#'  An attribute \code{cellfun = anomaly} is added to the \code{$Variable} element of the grid.
#' 
#' @template templateParallel
#' @export 

anomalyGrid <- function(grid,
                        parallel = FALSE,
                        max.ncores = 16,
                        ncores = NULL) {
      parallel.pars <- transformeR::parallelCheck(parallel, max.ncores, ncores)
      if (parallel.pars$hasparallel) {
            apply_fun <- function(...) {
                  parallel::parApply(cl = parallel.pars$cl, ...)
            }  
            on.exit(parallel::stopCluster(parallel.pars$cl))
      } else {
            apply_fun <- apply
      }
      attrs <- attributes(grid$Data)
      dimNames <- getDim(grid)
      mar <- grep("^time", dimNames, invert = TRUE)
      perm <- match(dimNames, c("time", dimNames[mar]))
      arr <- apply_fun(grid$Data, MARGIN = mar, FUN = function(x) x - mean(x, na.rm = TRUE))
      arr <- aperm(arr, perm = perm)
      grid$Data <- arr
      attributes(grid$Data) <- attrs
      attr(grid$Variable, "cellfun") <- "anomaly"
      return(grid)
}