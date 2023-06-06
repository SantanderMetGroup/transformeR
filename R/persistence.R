#     persistence.R Persistence climatology
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

#' @title Persistence climatology (Temporal autocorrelation at given time lag)
#' @description Computes the persistence climatology (i.e., the temporal autocorrelation at a given time lag)
#' @param grid Input grid (most likely annually aggregated)
#' @param lag Time lag at which to calculate persistence. Default is 1.
#' @param ci Coverage probability for confidence interval (in the range (0-1)). Default to \code{NULL}, and ignored.
#' @return A climatology grid (i.e., \code{"time"} dimension size = 1).
#' @template templateParallelParams
#' 
#' @details 
#' The function is a wrapper of \code{\link[stats]{acf}} to compute the autocorrelation function. Significance at the given confidence 
#' interval is calculated as in \code{\link[stats]{plot.acf}}
#' 
#' In case of any missing values within the series, NA will be returned.
#' 
#' If \code{ci} is specified (e.g. \code{ci=0.95}), two global attributes are appended: 
#' \itemize{
#' \item \code{"signif:ci"}, indicating the confidence interval chosen
#' \item \code{"is.signif"}, which is a logical matrix of dimension \code{lat x lon} indicating which points exhibit a 
#' significant persistence
#' }
#'  
#' @template templateParallel
#' 
#' @seealso \code{\link[visualizeR]{spatialPlot}}, for conveniently plotting persistence climatology maps.
#' 
#' @importFrom stats acf qnorm
#' @importFrom utils tail
#' @importFrom parallel stopCluster
#' @importFrom abind abind
#' @author J Bedia
#' @export

persistence <- function(grid,
                        lag = 1,
                        ci = NULL,
                        parallel = FALSE,
                        max.ncores = 16, 
                        ncores = NULL) {
    parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
    apply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "apply")
    if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
    dimNames <- getDim(grid)
    mar <- grep("time", dimNames, invert = TRUE)
    cormat <- apply_fun(grid$Data, MARGIN = mar, FUN = function(x) {
        a <- tryCatch(acf(x, lag.max = lag, plot = FALSE), error = function(err) list(acf = NA))
        corr <- tail(a$acf, 1)      
        return(corr)
    })
    if (!is.null(ci)) {
        sigmat <- apply_fun(grid$Data, MARGIN = mar, FUN = function(x) {
            a <- tryCatch(acf(x, lag.max = lag, plot = FALSE), error = function(err) list(acf = NA))
            corr <- tail(a$acf, 1)      
            is.signif <- FALSE
            if (!is.na(corr)) {
                conf.int <- qnorm((1 + ci)/2)/sqrt(a$n.used)
                is.signif <- ifelse(abs(conf.int) > abs(corr), FALSE, TRUE)
            }
            return(is.signif)
        })
        attr(grid, "signif:ci") <- ci
        attr(grid, "is.signif") <- sigmat
    }
    arr <- unname(abind(cormat, along = -1L))
    attr(arr, "dimensions") <- c("time", dimNames[mar])
    attr(arr, "climatology:fun") <- "acf"
    grid$Data <- arr
    grid <- redim(grid)
    attr(grid, "lagged_corr:lag") <- lag
    return(grid)
}

