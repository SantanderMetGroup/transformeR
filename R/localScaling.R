##     localScaling.R Grid local 
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
##     along with this program. If not, see <http://www.gnu.org/licenses/>.


#' @title Grid local scaling
#' @description Scale a grid (or compute anomalies)
#' @param grid Input grid to be rescaled
#' @param base Reference climatology (baseline) to be subtracted to the input grid. If \code{NULL} (the default),
#' the climatology is directly computed from the grid via \code{\link{climatology}}, either member-by-member
#' or using the ensemble mean climatology, as specified by the \code{by.member} flag.
#' @param ref Reference grid. After subtracting to grid its climatology (as defined by \code{grid.clim}), the mean
#' climatology of this additional grid is added. Default to NULL, so no reference grid is used.
#' @param clim.fun Function to compute the climatology. Default to mean. 
#' See \code{\link{climatology}} for details on function definition.
#' @param by.member Logical. In case of multimember grids, should the climatology be computed sepparately
#' for each member (\code{by.member=TRUE}), or a single climatology calculated from the ensemble mean
#'  (\code{by.member=FALSE})?. Default to \code{TRUE}. Argument passed to \code{\link{climatology}}.
#' @param time.frame Character indicating the time frame to perform the scaling. Possible values are
#'  \code{"none"}, which considers the climatological mean of the whole period given in 
#'  \code{base} and/or \code{ref}, \code{"monthly"}, that performs the calcuation on a monthly basis
#'  and \code{"daily"}, for a julian day-based approach.
#' @template templateParallelParams
#' @details The reference grid (\code{ref}) is used to correct the input grid, as follows:
#' 
#' \deqn{grid' = grid - base_clim + ref_clim}
#' 
#' , where \emph{base_clim} corresponds to the baseline climatology (by default the grid climatology), thus yielding an anomaly
#' w.r.t. the base, and \emph{ref_clim} is a reference climatological value added to the previously calculated anomaly. Thus, 
#'  if both \code{base} and \code{ref} are set to \code{NULL}, the output grid corresponds to the anomaly field of the 
#'  input \code{grid} w.r.t. its own mean. The way \emph{base_clim} and \emph{ref_clim_grid} are computed in case of multimember grids is controlled
#'  by the argument \code{by.member}. By default the climatological mean is used, but this can be changed
#'   by the user through the argument \code{clim.fun}, that is passed to \code{\link{climatology}}.
#' 
#' 
#' The \code{ref} usually corresponds to the control run of the GCM in the training period in climate change applications,
#' or the hindcast data for the training period in s2d applications. Note that by default \code{ref = NULL}. In this 
#' case it will be assumed to be the \code{pred} grid. This can be used for instance when train and test correspond
#' to the same model.
#' 
#' @importFrom abind abind asub
#' @importFrom stats na.omit
#' @importFrom magrittr %>% %<>% 
#' @importFrom parallel stopCluster
#' @return A locally scaled grid
#' @author J. Bedia
#' @export
#' @examples 
#' ## ANOMALIES
#' data("iberia_ncep_psl")
#' # Define average aggregation function
#' f = list(FUN = "mean", na.rm = TRUE)
#' psl <- aggregateGrid(iberia_ncep_psl, aggr.y = f) # get interannual DJF series 
#' 
#' ## When 'base' and 'ref' are not supplied,
#' ## the input grid climatology (by default the mean) is subtracted, thus yielding anomalies:
#' psl.anom <- localScaling(psl)
#' # spatial aggregation of the output (for plotting the time series)
#' psl.anom.iberia <- aggregateGrid(psl.anom, aggr.lat = f, aggr.lon = f)
#' redim(psl.anom.iberia, drop = TRUE)
#' plot(getYearsAsINDEX(psl.anom.iberia), psl.anom.iberia$Data, ty = 'b',
#'      ylab = "MSLP anomalies (Pa)", xlab = "year",
#'      main = "NCEP Reanalysis - Mean SLP anomalies (DJF)")
#' grid()
#' abline(h = 0, lty = 2, col = "blue")
#' 
#' # In the particular case of multimember grids, the anomalies are computed for each member
#' # by subtracting either their own mean (by.member = TRUE) or the 
#' # multimember mean climatology (by.member = FALSE)
#' data("tasmax_forecast")
#' a <- localScaling(tasmax_forecast, by.member = FALSE)
#' aa <- aggregateGrid(a, aggr.lat = f, aggr.lon = f)
#' redim(aa, drop = TRUE)
#' plot(as.Date(getRefDates(aa)), aa$Data[4,], ty = "o", xlab = "Date", 
#'      ylab = "Tmax anomaly (degC)", main = "Tmax anomaly, Member 4", ylim = c(-2,2))
#' grid()
#' b <- localScaling(tasmax_forecast, by.member = TRUE)
#' bb <- aggregateGrid(b, aggr.lat = f, aggr.lon = f)
#' redim(bb, drop = TRUE)
#' lines(as.Date(getRefDates(aa)), bb$Data[4,], col = "red", ty = "o")
#' legend("bottomright", c("by.member = FALSE", "by.member = TRUE"), lty = 1, col = c(1,2))
#' 
#' # In this example, the anomalies are calculated using a different period specifying a "base".
#' # Note that "base" could be also a grid of a different dataset, for instance a reanalysis
#' grid <- subsetGrid(EOBS_Iberia_tas, years = 1999:2000)
#' base <- subsetGrid(EOBS_Iberia_tas, years = 1998)
#' lc <- localScaling(grid = grid, base = base)
#' plot(lc$Data[,,15,15], ty = 'l', ylim = c(-10,10),
#'      xlab = "time", ylab = "Anomaly (degC)",
#'      main = "Anomalies w.r.t. 1998")
#' grid()
#' abline(h = 0, lty = 2, col = "grey30")
#' # The anomalies are calculated on a monthly basis using the 'time.frame' argument:
#' lc.m <- localScaling(grid = grid, base = base, time.frame = "monthly")
#' lines(lc.m$Data[,,15,15], col = "red")
#' lc.d <- localScaling(grid = grid, base = base, time.frame = "daily")
#' lines(lc.d$Data[,,15,15], col = "blue")
#' legend("topleft", c("none", "monthly","daily"), title = "'time.frame'",
#'        lty = 1, col = c(1,2,4), bty = "n")
#'
#' # An example in which the reference climatology is added to the anomalies:
#' # (not quite meaningful, though)
#' grid <- subsetGrid(EOBS_Iberia_tas, years = 1999)
#' base <- subsetGrid(EOBS_Iberia_tas, years = 1998)
#' ref <- subsetGrid(EOBS_Iberia_tas, years = 2000)
#' lc.ref <- localScaling(grid = grid, base = base, ref = ref)
#' lc.ref.m <- localScaling(grid = grid, base = base, ref = ref, time.frame = "monthly")
#' plot(grid$Data[,15,15], ty = "l", ylim = c(0,15))
#' lines(lc.ref$Data[,,15,15], col = "blue")
#' lines(lc.ref.m$Data[,,15,15], col = "red")
#' plotClimatology(climatology(grid))
#' plotClimatology(climatology(lc.ref))

localScaling <- function(grid,
                         base = NULL,
                         ref = NULL,
                         clim.fun = list(FUN = "mean", na.rm = TRUE),
                         by.member = TRUE,
                         time.frame = c("none", "monthly", "daily"),
                         parallel = FALSE,
                         max.ncores = 16,
                         ncores = NULL) {
    time.frame <- match.arg(time.frame, choices = c("none", "monthly", "daily"))
    if (time.frame == "none") {
        message("[", Sys.time(), "] - Scaling ...")
        out <- .localScaling(grid, base, ref, clim.fun, by.member, parallel, max.ncores, ncores)
        message("[", Sys.time(), "] - Done")
    } else if (time.frame == "monthly") {
        message("[", Sys.time(), "] - Scaling by months ...")
        months <- getSeason(grid)
        aux.list <- lapply(1:length(months), function(x) {
            grid  %<>%  subsetGrid(season = months[x])
            if (!is.null(base)) base %<>% subsetGrid(season = months[x])
            if (!is.null(ref)) ref %<>% subsetGrid(season = months[x])
            .localScaling(grid, base, ref, clim.fun, by.member, parallel, max.ncores, ncores)
        })
        out <- do.call("bindGrid.time", aux.list)
        message("[", Sys.time(), "] - Done")
    } else if (time.frame == "daily") {
        doys.grid <- grid %>% getRefDates() %>% substr(6,10)
        doys.grid <- gsub("02-29", "02-28", doys.grid)
        if (!is.null(base)) {
            doys.base <- base %>% getRefDates() %>% substr(6, 10)
            doys.base <- gsub("02-29", "02-28", doys.base)
        }
        if (!is.null(ref)) {
            doys.ref <- ref %>% getRefDates() %>% substr(6, 10)
            doys.ref <- gsub("02-29", "02-28", doys.ref)
        }
        message("[", Sys.time(), "] - Scaling by julian days ...")
        aux.list <- lapply(unique(doys.grid), function(x) {
            grid %<>% subsetDimension(dimension = "time", indices = which(doys.grid == x))
            if (!is.null(base)) base %<>% subsetDimension(dimension = "time", indices = which(doys.base == x))
            if (!is.null(ref)) ref %<>% subsetDimension(dimension = "time", indices = which(doys.ref == x))
            .localScaling(grid, base, ref, clim.fun, by.member, parallel, max.ncores, ncores)
        })
        out <- do.call("bindGrid.time", aux.list)
        message("[", Sys.time(), "] - Done")
    }
    invisible(out)
}


#' @title Local scaling internal    
#' @description Internal workhorse for local scaling
#' @keywords internal
#' @importFrom magrittr %>% 
#' @importFrom abind abind
#' @importFrom parallel stopCluster
#' @author J Bedia

.localScaling <- function(grid, base, ref, clim.fun, by.member, parallel, max.ncores, ncores) {
    grid <- redim(grid)
    if (is.null(base)) {
        base <- suppressMessages({
            climatology(grid, clim.fun, by.member, parallel, max.ncores, ncores)
        }) %>% redim()
    } else {
        checkSeason(grid, base)
        checkDim(grid, base, dimensions = c("lat", "lon"))
        base <- suppressMessages({
            climatology(base, clim.fun, by.member, parallel, max.ncores, ncores)
        }) %>% redim()
    }
    if (!is.null(ref)) {
        checkDim(grid, ref, dimensions = c("lat", "lon"))
        checkSeason(grid, ref)
        ref <- suppressMessages({
            climatology(ref, clim.fun, by.member, parallel, max.ncores,ncores)
        }) %>% redim()
    } else {
        ref <- list()
        ref[["Data"]] <- array(0, getShape(base))
        attr(ref[["Data"]], "dimensions") <- getDim(base)
    }    
    parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
    lapply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "lapply")
    if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
    clim <- grid[["Data"]]
    dimNames <- getDim(grid)
    ind.time <- grep("^time", dimNames)
    n.times <- getShape(grid, "time")
    Xc <- base[["Data"]]
    Xref <- ref[["Data"]]
    aux.list <- lapply_fun(1:n.times, function(x) {
        X <- asub(clim, idx = x, dims = ind.time, drop = FALSE)
        out <- if (dim(X)[1] != dim(Xc)[1]) {
            aux <- lapply(1:dim(X)[1], function(i) X[i, , , , drop = FALSE] - Xc + Xref)
            do.call("abind", c(aux, along = 1)) %>% unname()
        } else {
            X - Xc + Xref
        }
        return(out)
    })
    X <- Xc <- Xref <- base <- ref <- NULL
    grid[["Data"]] <- do.call("abind", c(aux.list, along = ind.time)) %>% unname()
    aux.list <- NULL
    attr(grid[["Data"]], "dimensions") <- dimNames
    return(grid)
}






