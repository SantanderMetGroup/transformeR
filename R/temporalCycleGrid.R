##     temporalCycleGrid.R  Daily to monthly temporal cycles 
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
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Temporal cycle calculation
#' @description A function to compute temporal cycles. See Details.
#' @param grid Input grid. This must be a daily grid, possibly previously filtered. See details
#' @param time.frame Character string. Temporal unit on which the cycle is based on. Currently acceped values are \code{"daily"}
#' (the default, requires a daily input grid) or monthly
#' @param clim.fun Function used to aggregate the values. Default to \code{\link{mean}}, but can be flexibly defined.
#' @param ... Further arguments passed to \code{clim.fun} (e.g. \code{na.rm = TRUE}, ...)
#' @template templateParallelParams
#' @details This function is intended for the computation of reference daily/ monthly climatologies 
#' (i.e., a climatological reference for each -julian- day of the year/month). It thus has a maximum temporal 
#' length of 365 i.e., one value for each day of the year, in case of grids encompassing an annual season
#' (see next subsection regarding leap years) or 12, in the case of monthly annual cycles.
#'  
#' \strong{Leap Years}
#' 
#' The function currently treats the 29th February as 28th februaries.
#' 
#' \strong{Moving average filter}
#' 
#' Daily average climatologies are typically calculated after the application of a moving average filter, thus
#' reducing the noise introduced by the large inter-annual variability at this temporal resolution. It is therefore
#' recommended the application of \code{\link{filterGrid}} (See examples).
#' 
#' @return A temporal cycle grid
#' 
#' @template templateParallel
#' @importFrom magrittr %>% 
#' @importFrom parallel stopCluster
#' @importFrom utils head
#' @author J Bedia
#' @export
#' @seealso \code{\link{filterGrid}} for the application of moving averages to grids.
#' @examples 
#' data("EOBS_Iberia_tas")
#' 
#' ## DAILY CYCLE (example with winter EOBS mean temperature)
#' grid <- EOBS_Iberia_tas
#' Mn <- temporalCycleGrid(grid, time.frame = "daily")
#' # We compute the areal mean to plot the results as a time-series
#' aggr.fun <- list(FUN = "mean", na.rm = TRUE)
#' Mn.agg <- aggregateGrid(Mn, aggr.lon = aggr.fun, aggr.lat = aggr.fun)
#' plot(Mn.agg$Data, ty = 'l', ylim = c(0,15), axes = FALSE,
#'      xlab = "month-day", ylab = "Mean Temp (degC)",
#'      main = "Daily Cycle DJF 1997-2000")
#' axis(2)
#' axis(1, at = seq(1,length(Mn.agg$Data),5), las = 2,
#'      labels = substr(getRefDates(Mn.agg), 6, 10)[seq(1,length(Mn.agg$Data),5)])
#' # It is possible to apply any other function apart from the default mean. For instance:
#' # Minimum
#' mn <- temporalCycleGrid(grid, clim.fun = "min", na.rm = TRUE)
#' mn.agg <- aggregateGrid(mn, aggr.lon = aggr.fun, aggr.lat = aggr.fun, weight.by.lat = TRUE)
#' # Maximum
#' mx <- temporalCycleGrid(grid, clim.fun = "max", na.rm = TRUE)
#' mx.agg <- aggregateGrid(mx, aggr.lon = aggr.fun, aggr.lat = aggr.fun, weight.by.lat = TRUE)
#' # Range envelope
#' ix <- 1:length(Mn.agg$Data)
#' polygon(x = c(ix, rev(ix)), y = c(mn.agg$Data, rev(mx.agg$Data)),
#'         border = "transparent", col = rgb(.2,.2,.2,.5))
#' legend("top", c("Daily mean cycle", "range"),
#'        pch = 22, pt.bg = c("transparent", "grey60"), pt.cex = c(0,2.5),
#'        lwd = c(1,0), bty = "n")
#' 
#' ## Smoothing daily data
#' ## A moving average window of 1 week is used here to smooth the daily series:
#' fMn <- filterGrid(Mn, window.width = 7, sides = 2)
#' fMn.agg <- aggregateGrid(fMn, aggr.lat = aggr.fun, aggr.lon = aggr.fun)
#' lines(fMn.agg$Data, col = "red")
#' legend("bottom", "Smoothed daily mean cycle", lty = 1, col = "red", bty = "n")
#' 
#' ## ANNUAL CYCLE (time.frame = "monthly")
#' # A monthly input grid is required.
#' # An attemp to compute a monthly time cycle with daily data will yield an error:
#' try(temporalCycleGrid(grid, time.frame = "monthly"))
#' # Thus, monthly aggregation is required:
#' mgrid <- aggregateGrid(grid, aggr.m = aggr.fun)
#' mcyc <- temporalCycleGrid(mgrid, time.frame = "monthly")
#' # The spatial mean is calculated to plot the time series:
#' mcyc.agg <- aggregateGrid(mcyc, aggr.lat = aggr.fun, aggr.lon = aggr.fun)
#' plot(mcyc.agg$Data, ty = 'b', xlab = "Month",  ylab = "Mean Temp (degC)",
#'      axes = FALSE, main = "Monthly Cycle DJF 1997-2000")
#' axis(2)
#' axis(1, at = 1:3, labels = month.abb[c(12,1,2)])
#' ## Also, note the special attributes in $Variable:
#' # type of cycle:
#' attributes(mcyc$Variable)$'temporal.cycle::time.frame'
#' # aggregation function:
#' attributes(mcyc$Variable)$'temporal.cycle::fun'

temporalCycleGrid <- function(grid, time.frame = c("daily", "monthly"),
                          clim.fun = "mean", ...,
                          parallel = FALSE, max.ncores = 16, ncores = NULL) {
    time.frame <- match.arg(time.frame, choices = c("daily", "monthly"))
    if (time.frame == "daily") {
        # leap.years: 29th februaries are forced to 28th
        grid$Dates$end <- gsub("-02-29", "-02-28", grid$Dates$end)
        grid$Dates$start <- gsub("-02-29", "-02-28", grid$Dates$start)
        tindex <- getRefDates(grid) %>% as.Date() %>% format("%j") %>% as.integer()
        if (tindex %>%  diff() %>% head(1) != 1L) stop("The input grid is not daily")
        # Remove possible duplicated 28th februaries
        grid$Dates  <- lapply(grid$Dates, "unique")
    } else if (time.frame == "monthly") {
        tindex <- getRefDates(grid) %>% as.Date() %>% format("%m") %>% as.integer()
        if (tindex %>% diff() %>% head(1) == 0) {
            stop("The input grid is not monthly\nConsider the application of 'aggregateGrid' prior to 'temporalCycleGrid'", call. = FALSE)
        }
    }
    parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
    pply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "apply")
    if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
    grid <- adjustTemporalCycleDates(grid)
    dimNames <- getDim(grid)
    mar <- grep("^time", dimNames, invert = TRUE)
    newDim <- c("time", dimNames[mar])
    message("[", Sys.time(),"] - Calculating temporal cycle...")
    suppressWarnings({
        grid$Data <- pply_fun(grid$Data, MARGIN = mar, FUN = temporalCycle.1D,
                              time.ref.INDEX = tindex, aggr.fun = clim.fun, ...) %>% unname()})
    message("[", Sys.time(),"] - Done.")
    attr(grid$Data, "dimensions") <- newDim
    grid <- redim(grid, drop = TRUE)
    attr(grid$Variable, "temporal.cycle::time.frame") <- time.frame
    attr(grid$Variable, "temporal.cycle::fun") <- clim.fun
    invisible(grid)
}


#' @keywords internal
#' @author J Bedia    

temporalCycle.1D <- function(x, time.ref.INDEX, aggr.fun, ...) {
    out <- tapply(x, INDEX = time.ref.INDEX, aggr.fun, ...)
    out[which(is.infinite(out))] <- NA
    return(out)
}


#' @title Adjust temporal cycle dates
#' @description Prepare the dates of an input grid to a suitable format for temporal cycle grids. Internally 
#' used by \code{\link{temporalCycleGrid}}
#' @keywords internal
#' @author J Bedia 

adjustTemporalCycleDates <- function(grid) {
    start <- grid$Dates$start
    end <- grid$Dates$end
    ref.years <- getYearsAsINDEX(grid)
    grid$Dates$start <- start[ref.years %in% range(ref.years)[1]]
    grid$Dates$end <- end[ref.years %in% range(ref.years)[2]]
    return(grid)
}


