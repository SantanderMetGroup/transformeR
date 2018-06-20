##     scaleGrid.R Grid scaling
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


#' @title Grid scaling
#' @description Scale a grid (or compute anomalies)
#' @param grid Input grid to be rescaled
#' @param base Reference baseline whose climatology will be subtracted to the input grid. If \code{NULL} (the default),
#' the climatology is directly computed from the input \code{grid} via \code{\link{climatology}}, either member-by-member
#' or using the ensemble mean climatology, as specified by the \code{by.member} flag. Note that \code{base} 
#' must NOT be a climatology (the base climatology is internally calculated via the argument \code{clim.fun}).
#' @param ref Reference grid. After subtracting to grid its climatology (as defined by \code{grid.clim}), the mean
#' climatology of this additional grid is added. Default to NULL, so no reference grid is used.
#' @param clim.fun Function to compute the climatology. Default to mean. 
#' See \code{\link{climatology}} for details on function definition.
#' @param by.member Logical. In case of multimember grids, should the climatology be computed sepparately
#' for each member (\code{by.member=TRUE}), or a single climatology calculated from the ensemble mean
#'  (\code{by.member=FALSE})?. Default to \code{TRUE}. Argument passed to \code{\link{climatology}}.
#' @param spatial.frame Character string indicating whether to perform the local scaling considering the climatolopgical value 
#' at the field scale (\code{"field"}) or per gridbox (\code{"gridbox"}, the default). 
#' @param time.frame Character indicating the time frame to perform the scaling. Possible values are
#'  \code{"none"}, which considers the climatological mean of the whole period given in 
#'  \code{base} and/or \code{ref}, \code{"monthly"}, that performs the calculation on a monthly basis
#'  and \code{"daily"}, for a julian day-based approach. See details.
#' @param type Character string. Either \code{"center"}, \code{"standardize"} or \code{"ratio"}, depending on wheter the correction factor are anomalies (e.g. temperature...),
#' estandardized anomalies or applied as a ratio (e.g. precipitation, wind speed...). See details
#' @param skip.season.check Logical flag. Should the internal checker \code{\link{checkSeason}} be skipped?. By
#' default, the function undertakes an automatic seasonal check. This can be skipped if needed, mainly for internal use
#' (e.g. in cross-validation setups)
#'   
#' @template templateParallelParams
#' @details In the \code{type = "center"} set up the reference grid (\code{ref}) is used to correct the input grid, as follows:
#' 
#' \deqn{grid'_{t} = grid - base_{clim} + ref_{clim}}
#' 
#' In the case of \code{type = "standardize"}:
#' 
#' \deqn{grid'_{t} = (grid - base_{clim})/ base_{sd} * ref_{sd} + ref_{clim}}
#' 
#' In the case of \code{type = "ratio"}:
#' 
#' \deqn{grid'_{t} = (grid / base_{clim}) * ref_{clim}}
#'  
#' , where \emph{grid'_{t}} is each time slice \emph{t} of the input grid, and \emph{base_clim} corresponds to the baseline climatology (by default the grid climatology), \emph{base_sd} is the standard deviation of the baseline climate signal,thus yielding an anomaly
#' w.r.t. the base, \emph{ref_clim} is a reference climatological value added to the previously calculated anomaly and \emph{ref_sd} is the standard deviation of the reference climate signal. The \code{"center"} or \code{"standardize"} version
#' is preferably applied to unbounded variables (e.g. temperature) and the \code{"ratio"} to variables with a lower bound 
#' (e.g. precipitation, because it also preserves the frequency). 
#' 
#'   Depending on the value of the argument \code{time.frame}, the baseline and reference climatologies are calculated for
#'   the whole season (\code{time.frame = "none"}), month by month \code{time.frame = "monthly"} 
#'   or by day-of-the-year (julian days) (\code{time.frame = "daily"}). Thus, 
#'  if both \code{base} and \code{ref} are set to \code{NULL}, the output grid corresponds to the anomaly field of the 
#'  input \code{grid} w.r.t. its own mean. The way \emph{base_clim} and \emph{ref_clim_grid} are computed in case of multimember grids is controlled
#'  by the argument \code{by.member}. By default the climatological mean is used, but this can be changed
#'   by the user through the argument \code{clim.fun}, that is passed to \code{\link{climatology}}.
#' 
#' The \code{ref} usually corresponds to the control (historical, 20C3M...) run of the GCM in the training period in climate change applications,
#' or the hindcast data for the training period in s2d applications. Note that by default \code{ref = NULL}. 
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
#' data("NCEP_Iberia_psl")
#' # Define average aggregation function
#' f = list(FUN = "mean", na.rm = TRUE)
#' psl <- aggregateGrid(NCEP_Iberia_psl, aggr.y = f) # get interannual DJF series
#' 
#' ## When 'base' and 'ref' are not supplied,
#' ## the input grid climatology (by default the mean) is subtracted, thus yielding anomalies:
#' psl.anom <- scaleGrid(psl)
#' # spatial aggregation of the output (for plotting the time series)
#' psl.anom.iberia <- aggregateGrid(psl.anom, aggr.lat = f, aggr.lon = f)
#' plot(getYearsAsINDEX(psl.anom.iberia), psl.anom.iberia$Data, ty = 'b',
#'      ylab = "MSLP anomalies (Pa)", xlab = "year",
#'      main = "NCEP Reanalysis - Mean SLP anomalies (DJF)")
#' grid()
#' abline(h = 0, lty = 2, col = "blue")
#' # In the particular case of multimember grids, the anomalies are computed for each member
#' # by subtracting either their own mean (by.member = TRUE) or the
#' # multimember mean climatology (by.member = FALSE)
#' data("CFS_Iberia_tas")
#' a <- scaleGrid(CFS_Iberia_tas, by.member = FALSE)
#' aa <- aggregateGrid(a, aggr.lat = f, aggr.lon = f, aggr.m = f, aggr.y = f)
#' # Example, member 4 time series
#' plot(as.Date(getRefDates(aa)), aa$Data[4,], ty = "o", xlab = "Date",
#'      ylab = "Tmean anomaly (degC)", main = "Tmean DJF anomalies, Member 4")
#' abline(h = 0, lty = 2, col = "blue")
#' grid()
#' b <- scaleGrid(CFS_Iberia_tas, by.member = TRUE) # almost identical in this case
#' bb <- aggregateGrid(b, aggr.lat = f, aggr.lon = f, aggr.m = f, aggr.y = f)
#' lines(as.Date(getRefDates(aa)), bb$Data[4,], col = "red", ty = "l")
#' legend("bottomright", c("by.member = FALSE", "by.member = TRUE"), lty = 1, col = c(1,2))
#' 
#' # In this example, the anomalies are calculated using a different period specifying a "base".
#' # Note that "base" could also be a grid of a different dataset, for instance a 
#' # reanalysisdata(EOBS_Iberia_tas)
#' grid <- subsetGrid(EOBS_Iberia_tas, years = 1999:2000)
#' base <- subsetGrid(EOBS_Iberia_tas, years = 1998)
#' lc <- scaleGrid(grid = grid, base = base)
#' plot(lc$Data[,,15,15], ty = 'l', ylim = c(-10,10),
#'      xlab = "time", ylab = "Anomaly (degC)",
#'      main = "Anomalies w.r.t. 1998")
#' abline(h = 0, lty = 2, col = "grey30")
#' str(lc)
#' # The anomalies are calculated on a monthly basis using the 'time.frame' argument:
#' lc.m <- scaleGrid(grid = grid, base = base, time.frame = "monthly")
#' lines(lc.m$Data[,,,15,15], col = "red")
#' lc.d <- scaleGrid(grid = grid, base = base, time.frame = "daily")
#' lines(lc.d$Data[,,,15,15], col = "blue")
#' legend("topleft", c("none", "monthly","daily"), title = "'time.frame'",
#'        lty = 1, col = c(1,2,4), bty = "n")
#' 
#' # An example in which the reference climatology is added to the anomalies:
#' # (not quite meaningful, though)
#' grid <- subsetGrid(EOBS_Iberia_tas, years = 1999)
#' base <- subsetGrid(EOBS_Iberia_tas, years = 1998)
#' ref <- subsetGrid(EOBS_Iberia_tas, years = 2000)
#' lc.ref <- scaleGrid(grid = grid, base = base, ref = ref)
#' lc.ref.m <- scaleGrid(grid = grid, base = base, ref = ref, time.frame = "monthly")
#' plot(grid$Data[,15,15], ty = "l", ylim = c(-7.5,10))
#' lines(lc.ref$Data[,,15,15], col = "blue")
#' lines(lc.ref.m$Data[,,,15,15], col = "red")
#' 
#' # An example using the "ratio" type:
#' data("EOBS_Iberia_pr")
#' grid <- subsetGrid(EOBS_Iberia_pr, years = 1999)
#' base <- subsetGrid(EOBS_Iberia_pr, years = 1998)
#' ref <- subsetGrid(EOBS_Iberia_pr, years = 2000)
#' fun <- list(FUN = "sum")
#' # We plot the climatologies (total accumulated precip) to have a visual comparison of
#' # the input grid, the grid used as base and the reference
#' mg <- makeMultiGrid(climatology(grid, clim.fun = fun),
#'                     climatology(base, clim.fun = fun),
#'                     climatology(ref, clim.fun = fun), skip.temporal.check = TRUE)
#' plotClimatology(mg, names.attr = c("input_grid", "base", "ref"))
#' # Note that for precipitation we use a scaling factor rather than an addition:
#' grid.corr <- scaleGrid(grid = grid, base = base, ref = ref,
#'                           time.frame = "monthly", type = "ratio")
#' mg.corr <- makeMultiGrid(climatology(grid, clim.fun = fun), climatology(grid.corr, clim.fun = fun))
#' plotClimatology(mg.corr, at = seq(0,350,10), names.attr = c("Raw","Scaled"))
#' # An example using the "standardize" type:
#' data("EOBS_Iberia_pr")
#' grid <- subsetGrid(EOBS_Iberia_pr, years = 1999)
#' base <- subsetGrid(EOBS_Iberia_pr, years = 1998)
#' ref <- subsetGrid(EOBS_Iberia_pr, years = 2000)
#' grid.corr <- scaleGrid(grid = grid, base = NULL, ref = NULL,
#'                        type = "standardize")
#' head(apply(grid.corr$Data,MARGIN = c(3,4),mean))
#' head(apply(grid.corr$Data,MARGIN = c(3,4),sd))
#'                        


scaleGrid <- function(grid,
                      base = NULL,
                      ref = NULL,
                      clim.fun = list(FUN = "mean", na.rm = TRUE),
                      by.member = TRUE,
                      time.frame = c("none", "monthly", "daily"),
                      spatial.frame = c("gridbox","field"),
                      type = "center",
                      parallel = FALSE,
                      max.ncores = 16,
                      ncores = NULL,
                      skip.season.check = FALSE) {
    time.frame <- match.arg(time.frame, choices = c("none", "monthly", "daily"))
    type <- match.arg(type, choices = c("center", "standardize", "ratio"))
    spatial.frame <- match.arg(spatial.frame, choices = c("gridbox", "field"))
    stopifnot(is.logical(skip.season.check))
    if (time.frame == "none") {
        message("[", Sys.time(), "] - Scaling ...")
        out <- gridScale.(grid, base, ref, clim.fun, by.member, type, parallel, max.ncores, ncores, spatial.frame, skip.season.check)
        message("[", Sys.time(), "] - Done")
    } else if (time.frame == "monthly") {
        message("[", Sys.time(), "] - Scaling by months ...")
        months <- getSeason(grid)
        aux.list <- lapply(1:length(months), function(x) {
            grid1 <- subsetGrid(grid, season = months[x])
            base1 <- if (!is.null(base)) {
                subsetGrid(base, season = months[x])
            } else {
                NULL
            }
            ref1 <- if (!is.null(ref)) {
                subsetGrid(ref, season = months[x])
            } else {
                NULL
            }
            gridScale.(grid1, base1, ref1, clim.fun, by.member, type, parallel, max.ncores, ncores, spatial.frame, skip.season.check)
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
            grid1 <- subsetDimension(grid, dimension = "time", indices = which(doys.grid == x))
            if (!is.null(base)) {
                base1 <- subsetDimension(base, dimension = "time", indices = which(doys.base == x))
            } else {
                base1 <- base
            }
            if (!is.null(ref)) {
                ref1 <- subsetDimension(ref, dimension = "time", indices = which(doys.ref == x))
            } else {
                ref1 <- ref
            }
            gridScale.(grid1, base1, ref1, clim.fun, by.member, type, parallel, max.ncores, ncores, spatial.frame, skip.season.check)
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

gridScale. <- function(grid, base, ref, clim.fun, by.member, type, parallel, max.ncores, ncores,
                       spatial.frame, skip.season.check) {
  grid <- redim(grid)
  if (is.null(base)) {
    base.m <- suppressMessages({
      climatology(grid, clim.fun, by.member, parallel, max.ncores, ncores)
    }) %>% redim()
    base.std <- 1
    ref.std <- 1
    if (type == "standardize") {
      base.std <- suppressMessages({
        climatology(grid, clim.fun = list(FUN = "sd", na.rm = TRUE), by.member, parallel, max.ncores, ncores)
      }) %>% redim()
      base.std <- base.std$Data}
    if (spatial.frame == "field") {
      getDim.base <- attr(base.m$Data, "dimensions")
      ind <- c(which(getDim(base.m) == "time"), which(getDim(base.m) == "lat"), which(getDim(base.m) == "lon"))
      base.m$Data <- array(data = apply(base.m$Data, MARGIN = -ind, FUN = function(Z) {mean(Z,na.rm = TRUE)}),dim = dim(base.m$Data))
      attr(base.m$Data,"dimensions") <- getDim.base
      if ((type == "standardize")) {
        base.std <- array(data = apply(grid$Data, MARGIN = -ind, FUN = function(Z) {sd(Z,na.rm = TRUE)}),dim = dim(base.m$Data))
      }
    }
    
  } else {
    if (!skip.season.check) checkSeason(grid, base)
    checkDim(grid, base, dimensions = c("lat", "lon"))
    base.m <- suppressMessages({
      climatology(base, clim.fun, by.member, parallel, max.ncores, ncores)
    }) %>% redim()
    base.std <- 1
    ref.std <- 1
    if ((type == "standardize")) {
      base.std <- suppressMessages({
        climatology(base, clim.fun = list(FUN = "sd", na.rm = TRUE), by.member, parallel, max.ncores, ncores)
      }) %>% redim()
      base.std <- base.std$Data}
    if (spatial.frame == "field") {
      getDim.base <- attr(base.m$Data, "dimensions")
      ind <- c(which(getDim(base.m) == "time"), which(getDim(base.m) == "lat"), which(getDim(base.m) == "lon"))
      base.m$Data <- array(data = apply(base.m$Data, MARGIN = -ind, FUN = function(Z) {mean(Z,na.rm = TRUE)}),dim = dim(base.m$Data))
      attr(base.m$Data,"dimensions") <- getDim.base
      if (type == "standardize") {
        base.std <- array(data = apply(redim(base)$Data, MARGIN = -ind, FUN = function(Z) {sd(Z,na.rm = TRUE)}),dim = dim(base.m$Data))
      }
    }
  }
  if (!is.null(ref)) {
    checkDim(grid, ref, dimensions = c("lat", "lon"))
    if (!skip.season.check) checkSeason(grid, ref)
    ref.m <- suppressMessages({
      climatology(ref, clim.fun, by.member, parallel, max.ncores,ncores)
    }) %>% redim()
    if (type == "standardize") {
      ref.std <- suppressMessages({
        climatology(ref, clim.fun = list(FUN = "sd", na.rm = TRUE), by.member, parallel, max.ncores, ncores)
      }) %>% redim()
      ref.std <- ref.std$Data}
    if (spatial.frame == "field") {
      getDim.ref <- attr(ref.m$Data, "dimensions")
      ind <- c(which(getDim(ref.m) == "time"), which(getDim(ref.m) == "lat"), which(getDim(ref.m) == "lon"))
      ref.m$Data <- array(data = apply(ref.m$Data, MARGIN = -ind, FUN = function(Z) {mean(Z,na.rm = TRUE)}),dim = dim(ref.m$Data))
      attr(ref.m$Data,"dimensions") <- getDim.ref
      if (type == "standardize") {
        ref.std <- array(data = apply(redim(ref)$Data, MARGIN = -ind, FUN = function(Z) {sd(Z,na.rm = TRUE)}),dim = dim(ref.m$Data))
      }
    }
    ref <- ref.m
  } else {
    ref <- list()
    ref[["Data"]] <- array(0, getShape(base.m))
    attr(ref[["Data"]], "dimensions") <- getDim(base.m)
  }    
  parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
  lapply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "lapply")
  if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
  clim <- grid[["Data"]]
  dimNames <- getDim(grid)
  ind.time <- grep("^time", dimNames)
  n.times <- getShape(grid, "time")
  Xc <- base.m[["Data"]]
  Xref <- ref[["Data"]]
  aux.list <- gridScale.type(clim, n.times, ind.time, Xc, Xref, type, lapply_fun, base.std, ref.std)
  Xc <- Xref <- base <- base.m <- base.std <- ref <- ref.std <- NULL
  grid[["Data"]] <- do.call("abind", c(aux.list, along = ind.time)) %>% unname()
  aux.list <- NULL
  attr(grid[["Data"]], "dimensions") <- dimNames
  return(grid)
}

#' @title Local scaling type internal    
#' @description Internal for center/standardize/ratio local scaling
#' @keywords internal
#' @importFrom magrittr %>% 
#' @importFrom abind abind
#' @importFrom parallel stopCluster
#' @author J Bedia

gridScale.type <- function(clim, n.times, ind.time, Xc, Xref, type, lapply_fun, base.std, ref.std) {
    if (type != "ratio") {
        lapply_fun(1:n.times, function(x) {
            X <- asub(clim, idx = x, dims = ind.time, drop = FALSE)
            if (dim(X)[1] != dim(Xc)[1]) {
                aux <- lapply(1:dim(X)[1], function(i) {((X[i, , , , drop = FALSE] - Xc) / base.std) * ref.std + Xref})
                do.call("abind", c(aux, along = 1)) %>% unname()
            } else {
                ((X - Xc) / base.std ) * ref.std + Xref
            }
        })
    } else {
        lapply_fun(1:n.times, function(x) {
            X <- asub(clim, idx = x, dims = ind.time, drop = FALSE)
            if (dim(X)[1] != dim(Xc)[1]) {
                aux <- lapply(1:dim(X)[1], function(i) (X[i, , , , drop = FALSE] / Xc) * Xref)
                do.call("abind", c(aux, along = 1)) %>% unname()
            } else {
                (X / Xc) * Xref
            }
        })
    }
}
