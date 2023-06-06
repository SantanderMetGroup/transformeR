##     climatology.R Compute a grid climatology
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


#' @title Compute a grid climatology
#' @description Calculates the climatology (i.e., complete temporal aggregation, typically the mean)
#' of the input grid.
#' @param grid Input grid
#' @param clim.fun Function to compute the climatology. This is specified as a list,
#'  indicating the name of the aggregation function in first place (as character), and other optional arguments
#'   to be passed to the aggregation function. Default to mean (i.e., \code{clim.fun = list(FUN="mean",na.rm = TRUE)}).
#' @param by.member Logical. In case of multimember grids, should the climatology be computed sepparately
#' for each member (\code{by.member=TRUE}), or a single climatology calculated from the ensemble mean
#'  (\code{by.member=FALSE})?. Default to \code{TRUE}.
#' @template templateParallelParams
#' @return A grid corresponding to the climatology. See details.
#' @template templateParallel
#' @details Two attributes are appended to the grid: 
#' \itemize{
#' \item \code{climatology:fun}, added to the \code{Data} component of the grid,
#' indicating the function used to compute the climatology.
#' \item \code{season}, added to the \code{Dates} component (if not yet existing), in order to provide information
#'  on the season for which the climatology has been computed.
#' }
#' @importFrom parallel stopCluster
#' @importFrom parallel parApply
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @author J. Bedia
#' @seealso \code{\link[visualizeR]{spatialPlot}}, for plotting climatologies.
#' \code{\link{persistence}}, for a special case in which the temporal autocorrelation function is applied.
#' @export
#' @examples \donttest{ 
#' require(climate4R.datasets)
#' # Station data:
#' # Mean surface temperature
#' data("VALUE_Iberia_tas")
#' st_mean_clim <- climatology(VALUE_Iberia_tas)
#' str(st_mean_clim)
#' require(visualizeR)
#' spatialPlot(st_mean_clim, backdrop.theme = "coastline")
#' # Standard deviation of surface temperature
#' st_sd_clim <- climatology(VALUE_Iberia_tas, clim.fun = list(FUN = sd, na.rm = TRUE))
#' spatialPlot(st_sd_clim, backdrop.theme = "coastline")
#' 
#' # July surface temp forecast climatology
#' data("CFS_Iberia_tas")
#' # Aggregate all members before computing the climatology
#' t_mean.clim <- climatology(CFS_Iberia_tas,
#'                            by.member = FALSE)
#' # Note the new attributes, and that time dimension is preserved as a singleton
#' str(t_mean.clim$Data)
#' str(t_mean.clim$Dates)
#' # Compute a climatology for each member sepparately
#' t_mean_9mem.clim <- climatology(CFS_Iberia_tas,
#'                                 by.member = TRUE)
#' str(t_mean_9mem.clim$Data)
#' # 9 different climatologies, one for each member
#' }

climatology <- function(grid,
                        clim.fun = list(FUN = "mean", na.rm = TRUE),
                        by.member = TRUE,
                        parallel = FALSE,
                        max.ncores = 16,
                        ncores = NULL) {
      parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
      grid <- redim(grid, member = FALSE, runtime = FALSE)
      dimNames <- getDim(grid)
      ## Member aggregation 
      if (!isTRUE(by.member)) {
            grid <- memberAggregation(grid,
                                      aggr.mem = list(FUN = "mean", na.rm = TRUE),
                                      parallel,
                                      max.ncores,
                                      ncores)
            dimNames <- getDim(grid)
      }
      mar <- grep("^time$", dimNames, invert = TRUE)
      if (length(dimNames) == length(mar)) stop("Time dimension not found", call. = FALSE)
      arg.list <- c(clim.fun, list("MARGIN" = mar), list("X" = grid[["Data"]]))
      pply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "apply")
      if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
      message("[", Sys.time(), "] - Computing climatology...")      
      clim <- do.call("pply_fun", arg.list)
      message("[", Sys.time(), "] - Done.")
      clim <- abind(clim, along = -1L)
      ## Attributes of Data
      dimNames <- c("time", dimNames[-grep("^time", dimNames)])
      grid[["Data"]] <- unname(clim)
      attr(grid$Data, "dimensions") <- dimNames
      grid %<>% redim()
      ## Date adjustment
      attr(grid$Dates, "season") <- getSeason(grid)
      grid$Dates$start <- grid$Dates$start[1]
      grid$Dates$end <- tail(grid$Dates$end, 1)
      grid <- redim(grid, drop = TRUE) 
      if (length(attr(grid$Data,"dimensions")) == 0) attr(grid$Data,"dimensions") <- "time"
      grid <- redim(grid, member = FALSE, var = FALSE, drop = FALSE)
    
      if(!isRegular(grid)) grid <- redim(grid, loc = TRUE, member = FALSE)
      attr(grid$Data, "climatology:fun") <- clim.fun[["FUN"]]
      return(grid)
}


#' @title Linear trend analysis
#' @description Atomic function for (linear) trend analysis
#' @param ts Numeric vector containing a time series
#' @param dates Dates vector associated to the records in \code{ts}.
#'  Standard format, as returned for instance by \code{\link{getRefDates}}
#' @param method Correlation method. Passed to \code{\link{cor.test}}.
#' @param return.pvalue Logical. Should p-values be returned instead of correlation coefficient estimates?.
#' Default to \code{FALSE}. 
#' @param conf.level Numeric value indicating the confidence level for the significance test.
#'  Passed to \code{\link{cor.test}}.
#' @details The function can be conveniently wrapped using \code{climatology}. See examples.
#' @export
#' @importFrom stats cor.test
#' @author J Bedia
#' @seealso \code{\link{climatology}}, for computing climatological trend maps from grids
#' @examples \donttest{
#' require(climate4R.datasets)
#' require(visualizeR)
#' # Simulate a positive trend
#' set.seed(1)
#' ts <- runif(min = 21, max = 23.5, n = 100) + seq(.1, .15, length.out = 100) * 10
#' dates <- paste0(1901:2000,"-01-01 00:00:00 GMT")
#' plot(1901:2000, ts, ty = 'l', ylab = "Simulated Mean Temp (degC)", xlab = "Year")
#' abline(reg = lm(ts ~ I(1901:2000)), col = "red", lty = 2)
#' trend.1D(ts, dates, return.pvalue = FALSE) # Default to Pearson's r
#' trend.1D(ts, dates, return.pvalue = TRUE)
#' # An example of how to compute a trend map using climatology as a wrapper for trend:
#' data("EOBS_Iberia_tas")
#' dates <- getRefDates(EOBS_Iberia_tas)
#' tau.estimate <- climatology(EOBS_Iberia_tas,
#'                             clim.fun = list(FUN = "trend.1D", dates = dates, method = "kendall"))
#' spatialPlot(tau.estimate, backdrop.theme = "coastline")
#' # Adding significant trend points is usually needed: 
#' pval.estimate <- climatology(EOBS_Iberia_tas,
#'                              clim.fun = list(FUN = "trend.1D",
#'                                              dates = dates,
#'                                              method = "kendall",
#'                                              return.pvalue = TRUE))
#' sig.points <- visualizeR::map.stippling(clim = pval.estimate, threshold = 0.05, condition = "LT", 
#'                                         pch = 19, cex = .25, col = "purple")
#' spatialPlot(tau.estimate, backdrop.theme = "coastline", sp.layout = list(sig.points))
#' }

trend.1D <- function(ts, dates, method = c("pearson","spearman","kendall"),
                     return.pvalue = FALSE, conf.level = 0.95) {
    if (length(dates) != length(ts)) stop("Input and date vector sizes do not match")
    method <- match.arg(method, c("pearson","spearman","kendall"))
    x <- as.numeric(as.Date(dates))
    i <- ifelse(return.pvalue, "p.value", "estimate")
    tryCatch(cor.test(x = ts, y = x, method = method, conf.level = conf.level)[[i]],
             error = function(err) NA)
}
