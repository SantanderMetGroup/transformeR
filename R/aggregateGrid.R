#     aggregateGrid.R Flexible grid aggregation along selected dimensions
#
#     Copyright (C) 2021 Santander Meteorology Group (http://www.meteo.unican.es)
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


#' @title Flexible grid aggregation along selected dimensions
#' @description Aggregates a grid along the target dimensions using user-defined functions.
#' @param grid a grid or multigrid to be aggregated.
#' @param aggr.d Daily aggregation function (for sub-daily data only). A list indicating the name of the
#'  aggregation function in first place, and other optional arguments to be passed to the aggregation function.
#'  To be on the safe side, the function in \code{FUN} should be always indicated as a character string. See the examples.
#' @param aggr.m Same as \code{aggr.d}, but indicating the monthly aggregation function. 
#' @param aggr.y Same as \code{aggr.d}, but indicating the annual aggregation function. 
#' @param aggr.s Same as \code{aggr.d}, but indicating the seasonal aggregation function. The season can be indicated
#' as shown in this example:  aggr.s = list(FUN = list("mean", na.rm = TRUE), season = c(12,1,2)) 
#' @param aggr.mem Same as \code{aggr.d}, but indicating the function for computing the member aggregation.
#' @param aggr.spatial Same as \code{aggr.d}, but indicating the aggregation function in case of rectangular domains to be aggregated
#' as a unique time series grid (or multimember time series grid.)
#' @param aggr.lat Same as \code{aggr.d}, indicating the aggregation function to be applied along latitude only.
#' @param weight.by.lat Logical. Should latitudinal averages be weighted by the cosine of latitude?.
#' Default to \code{TRUE}. Ignored if no \code{aggr.lat} or \code{aggr.spatial} function is indicated,
#'  or a function different from \code{"mean"} is applied.
#' @param aggr.lon Same as \code{aggr.lat}, but for longitude.
#' @param aggr.loc Same as \code{aggr.d}, indicating the aggregation function to be applied along the loc dimension.
#' @template templateParallelParams
#' @return A grid or multigrid aggregated along the chosen dimension(s).
#' @details
#' 
#' \strong{Aggregation function definition}
#' 
#' The aggregation functions are specified in the form of a named list of the type \code{FUN = "function", ...}, where
#' \code{...} are further arguments passes to FUN. This allows for a flexible definition of aggregation functions, that are 
#' internally passes to \code{\link{tapply}}. Note that the name of the function is indicated as a character string.
#' 
#' \strong{Member aggregation}
#' 
#' The function preserves the metadadata associated with member information (i.e. initialization dates and member names) after
#' aggregation. In addition, an attribute indicating the member aggregation function is added to the \code{Variable} component.
#' 
#' \strong{Temporal aggregation}
#'  
#' To annually or monthly aggregate data, \code{aggr.d} and/or \code{aggr.m} functions are specified.
#' Aggregations need to be specified from bottom to top, so for instance, if the data in the grid is sub-daily
#' and \code{aggr.d} is not specified, an error will be given for monthly or annual aggregation attempts. Similarly,
#' annual aggregations require a previous specification of daily and monthly aggregation, when applicable. Special 
#' attributes in the \code{Variable} component indicate the aggregation undertaken.
#' 
#' In order to preserve the information of the season in annual aggregations, the attribute \code{season} is
#' added to the \code{Dates} component.
#' 
#' @template templateParallel
#' @author M. Iturbide, M. de Felice, J. Bedia 
#' @export
#' @importFrom magrittr %<>% 
#' @examples \donttest{
#' require(climate4R.datasets)
#' data("CFS_Iberia_tas")
#' ## Aggregating members
#' # Ensemble mean
#' mn <- aggregateGrid(grid = CFS_Iberia_tas, aggr.mem = list(FUN = "mean", na.rm = TRUE))
#' require(visualizeR)
#' spatialPlot(climatology(mn, by.member = FALSE),
#'                 backdrop.theme = "coastline", main = "Ensemble mean tmax climatology")
#' # Ensemble 90th percentile
#'  ens90 <- aggregateGrid(grid = CFS_Iberia_tas,
#'                         aggr.mem = list(FUN = quantile, probs = 0.9, na.rm = TRUE))
#' spatialPlot(climatology(ens90, by.member = FALSE),
#'                 backdrop.theme = "coastline", main = "Ensemble 90th percentile tmax climatology")
#' 
#' ## Monthly aggregation
#' monthly.mean <- aggregateGrid(CFS_Iberia_tas, aggr.m = list(FUN = mean, na.rm = TRUE))
#' spatialPlot(climatology(monthly.mean), backdrop.theme = "coastline",
#'                 main = "Mean tmax climatology")
#'
#' ## Several dimensions ca be aggregated in one go:
#' mm.mean <- aggregateGrid(CFS_Iberia_tas,
#'                          aggr.mem = list(FUN = "mean", na.rm = TRUE),
#'                          aggr.m = list(FUN = "mean", na.rm = TRUE))
#' }


aggregateGrid <- function(grid,
                          aggr.mem = list(FUN = NULL),
                          aggr.d = list(FUN = NULL),
                          aggr.m = list(FUN = NULL),
                          aggr.y = list(FUN = NULL),
                          aggr.s = list(FUN = NULL, season = NULL),
                          aggr.spatial = list(FUN = NULL),
                          aggr.lat = list(FUN = NULL),
                          weight.by.lat = TRUE,
                          aggr.lon = list(FUN = NULL),
                          aggr.loc = list(FUN = NULL),
                          parallel = FALSE,
                          max.ncores = 16,
                          ncores = NULL) {
    grid$Data[is.infinite(grid$Data)] <- NA
    if (!is.null(aggr.d$FUN)) {
        grid <- timeAggregation(grid, "DD", aggr.d, parallel, max.ncores, ncores)
    }
    if (!is.null(aggr.m$FUN)) {
        grid <- timeAggregation(grid, "MM", aggr.m, parallel, max.ncores, ncores)
    }
    if (!is.null(aggr.y$FUN)) {
        grid <- timeAggregation(grid, "YY", aggr.y, parallel, max.ncores, ncores)
    }
    if (!is.null(aggr.s$FUN)) {
        if (is.null(aggr.s$season)) stop("Please indicate the desired season using the aggr.s argument")
        grid <- subsetGrid(grid,season = aggr.s$season)
        months <- sapply(getRefDates(grid),FUN = function(z) substr(z,start = 6,stop = 7)) %>% as.numeric()
        indLastMonth <- which(months == aggr.s$season[length(aggr.s$season)])
        indCut <- c(0,indLastMonth[which(diff(indLastMonth) > 1)],length(months))
        
        grid <- lapply(1:(length(indCut)-1), FUN = function(z) {
            subsetDimension(grid,dimension = "time", indices = (indCut[z]+1):(indCut[z+1])) %>%
                climatology(clim.fun = aggr.s$FUN)  
        }) %>% bindGrid(dimension = "time")
    }
    if (!is.null(aggr.spatial$FUN)) {
        grid <- spatialAggregation(grid, aggr.fun = aggr.spatial,
                                   weight.by.lat = weight.by.lat,
                                   parallel, max.ncores, ncores)
    }
    if (!is.null(aggr.lat$FUN)) {
        grid <- latAggregation(grid, aggr.lat, weight.by.lat, parallel, max.ncores, ncores)
    }
    if (!is.null(aggr.lon$FUN)) {
        grid <- lonAggregation(grid, aggr.lon, parallel, max.ncores, ncores)
    }
    if (!is.null(aggr.loc$FUN)) {
          grid <- locAggregation(grid, aggr.loc, weight.by.lat, parallel, max.ncores, ncores)
    }
    if (!is.null(aggr.mem$FUN)) {
        grid <- memberAggregation(grid, aggr.mem, parallel, max.ncores, ncores)
    }
    if (!is.array(grid$Data)) grid$Data %<>% as.array()
    return(grid)
}

#' @title Member aggregation
#' @description Aggregate a grid along its member dimension
#' @param grid A multimember grid to apply the aggregation
#' @param aggr.mem Character string indicatins the aggregation function
#' @param parallel.pars Arguments defining the parallelization options, as passed by \code{\link{parallelCheck}}
#' @details The function preserves the metadadata associated with member information (i.e. initialization dates and member names). In addition,
#' an attribute indicating the member aggregation function is added to the \code{Variable} component.
#' @importFrom parallel parApply
#' @importFrom parallel stopCluster
#' @keywords internal
#' @author J. Bedia

memberAggregation <- function(grid, aggr.mem, parallel, max.ncores, ncores) {
    dimNames <- getDim(grid)
    if (!"member" %in% dimNames) {
        message("Not a multimember grid: 'aggr.mem' option was ignored.")
    } else {
        parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
        attr.all <- attributes(grid$Data)
        mar <- grep("member", dimNames, invert = TRUE)
        attr.all$dim <- attr.all$dim[mar]
        attr.all$dimensions <- dimNames[mar]
        aggr.mem[["MARGIN"]] <- mar
        aggr.mem[["X"]] <- grid$Data
        out <- if (parallel.pars$hasparallel) {
            message("[", Sys.time(), "] - Aggregating members in parallel...")
            on.exit(parallel::stopCluster(parallel.pars$cl))
            aggr.mem[["cl"]] <- parallel.pars$cl
            do.call("parApply", aggr.mem)
        } else {
            message("[", Sys.time(), "] - Aggregating members...")
            do.call("apply", aggr.mem)
        }
        message("[", Sys.time(), "] - Done.")
        grid[["Data"]] <- out
        # Data attributes
        attrs <- setdiff(names(attr.all), c("dim", "dimensions"))
        if (length(attrs) > 0) {
            for (i in 1:length(attrs)) {
                ind <- grep(attrs[i], names(attr.all))
                attr(grid[["Data"]], attrs[i]) <- attr.all[ind]
            }
        }      
        dimNames <- dimNames[-grep("member", dimNames)]
        attr(grid$Data, "dimensions") <- dimNames
        attr(grid$Variable, "member_agg_cellfun") <- aggr.mem[[1]]
    }
    return(grid)
}


#' @title Time aggregation
#' @description Aggregate a grid along its time dimension
#' @param grid A multimember grid to apply the aggregation
#' @param aggr.type Character string indicating the type of temporal aggregation: 
#' daily (\code{"DD"}), monthly (\code{"MM"}) or annual (\code{"YY"}).
#' @param aggr.fun One of \code{aggr.d}, \code{aggr.m} or \code{aggr.y} arguments, as passed by \code{aggregateGrid}
#' @param parallel.pars Arguments defining the parallelization options, as passed by \code{\link{parallelCheck}}
#' @importFrom parallel stopCluster
#' @keywords internal
#' @author J. Bedia, M. Iturbide, M. de Felice


timeAggregation <- function(grid, aggr.type = c("DD","MM","YY"), aggr.fun, parallel, max.ncores, ncores) {
    aux.dates <- getRefDates(grid) 
    time.res <- getTimeResolution(grid)
    cellfun <- aggr.fun$FUN
    if (aggr.type == "DD" & time.res == "DD") {
        message("Data is already daily: 'aggr.d' option was ignored.")
    } else if (aggr.type == "MM" & time.res == 'MM') {
        message("Data is already monthly: 'aggr.m' option was ignored.")
    } else if (aggr.type == "YY" & time.res == 'YY') {
        message("Data is already annual: 'aggr.y' option was ignored.")
    } else {
        dimNames <- getDim(grid)
        # Attributes
        season <- getSeason(grid)
        attr.all <- attributes(grid$Data)
        mar <- grep("^time", dimNames, invert = TRUE)
        day <- substr(aux.dates,9,10)
        mon <- substr(aux.dates,6,7)
        yr <- getYearsAsINDEX(grid)
        fac <- switch(aggr.type,
                      "DD" = paste0(yr,mon,day),
                      "MM" = paste0(yr,mon),
                      "YY" = yr)
        day <- mon <- yr <- aux.dates <- NULL
        fac <- factor(fac, levels = unique(fac), ordered = TRUE)
        arg.list <- c(aggr.fun, list("INDEX" = fac))
        type <- switch(aggr.type,
                       "DD" = "daily",
                       "MM" = "monthly",
                       "YY" = "annual")
        parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
        apply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "apply")
        if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
        message("[", Sys.time(), "] Performing ", type, " aggregation...")
        arr <- apply_fun(grid$Data, MARGIN = mar, FUN = function(x) {
            arg.list[["X"]] <- x
            do.call("tapply", arg.list)
        })
        message("[", Sys.time(), "] Done.")
        # Array attributes -----------------
        # Preserve time dimension if lost
        if (length(dim(arr)) != length(dimNames)) arr <- abind(arr, along = -1) 
        # 'time' is now the most external
        grid$Data <- unname(arr)
        attr(grid$Data, "dimensions") <- dimNames[c(grep("^time", dimNames), grep("^time", dimNames, invert = TRUE))]
        if ("loc" %in% dimNames) grid <- redim(grid, member = FALSE, loc = TRUE)
        if (any(names(attr.all) != "dim" & names(attr.all) != "dimensions")) {
            attributes(grid$Data) <- attr.all[grep("^dim$|^dimensions$", names(attr.all), invert = TRUE)]
        }
        # Date adjustment
        if (gridDepth(grid$Dates) > 1) {
            grid$Dates <- lapply(1:length(grid$Dates), function(x) {
                list("start" = unname(tapply(grid$Dates[[x]]$start, INDEX = fac, FUN = min)),
                     "end" = unname(tapply(grid$Dates[[x]]$end, INDEX = fac, FUN = max)))
            })
        } else {
            grid$Dates <- list("start" = unname(tapply(grid$Dates$start, INDEX = fac, FUN = min)),
                               "end" = unname(tapply(grid$Dates$end, INDEX = fac, FUN = max)))
        }
        dimInd <- match(dimNames, names(getShape(grid)))
        grid$Data <- aperm(grid$Data, dimInd)
        # Temporal aggregation attributes 
        attr(grid$Data, "dimensions") <- dimNames
        attr(grid$Variable, paste0(type,"_agg_cellfun")) <- cellfun
        if (aggr.type == "YY") attr(grid$Dates, "season") <- season
        if (getShape(grid, "time") == 1L) attr(grid$Data, "climatology:fun") <- cellfun
    }
    return(grid)
}


latAggregation <- function(grid, aggr.fun, weight.by.lat, parallel, max.ncores, ncores) {
    stopifnot(is.logical(weight.by.lat))
    dimNames <- getDim(grid)
    if (!"lat" %in% dimNames) {
        message("There is not lat dimension: 'aggr.lat' option was ignored.")
    } else {
        if (isTRUE(weight.by.lat)) {
            message("Calculating areal weights...")
            lat.weights <- latWeighting(grid)
            if (aggr.fun[["FUN"]] == "mean") {
                aggr.fun <- list(FUN = "weighted.mean", w = lat.weights, na.rm = TRUE)
            }
        }
        parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
        mar <- grep("lat", dimNames, invert = TRUE)
        aggr.fun[["MARGIN"]] <- mar
        aggr.fun[["X"]] <- grid$Data
        out <- if (parallel.pars$hasparallel) {
            message("[", Sys.time(), "] - Aggregating lat dimension in parallel...")
            on.exit(parallel::stopCluster(parallel.pars$cl))
            aggr.fun[["cl"]] <- parallel.pars$cl
            do.call("parApply", aggr.fun)
        } else {
            message("[", Sys.time(), "] - Aggregating lat dimension...")
            do.call("apply", aggr.fun)
        }
        grid$Data <- out
        attr(grid$Data, "dimensions") <- dimNames[mar]
        message("[", Sys.time(), "] - Done.")
    }
    return(grid)
}


lonAggregation <- function(grid, aggr.fun, parallel, max.ncores, ncores){
    dimNames <- getDim(grid)
    if (!"lon" %in% dimNames) {
        message("There is not lat dimension: 'aggr.lon' option was ignored.")
    } else {
        parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
        mar <- grep("lon", dimNames, invert = TRUE)
        aggr.fun[["MARGIN"]] <- mar
        aggr.fun[["X"]] <- grid$Data
        out <- if (parallel.pars$hasparallel) {
            message("[", Sys.time(), "] - Aggregating lon dimension in parallel...")
            on.exit(parallel::stopCluster(parallel.pars$cl))
            aggr.fun[["cl"]] <- parallel.pars$cl
            do.call("parApply", aggr.fun)
        } else {
            message("[", Sys.time(), "] - Aggregating lon dimension...")
            do.call("apply", aggr.fun)
        }
        grid$Data <- out
        attr(grid$Data, "dimensions") <- dimNames[mar]
        message("[", Sys.time(), "] - Done.")
    }
    return(grid)
}


#' @title Weight as a function of latitude cosine
#' @description Computes weights as a function of latitude to compute areal averages
#' @author J. Bedia, S. Siegert
#' @keywords internal

latWeighting <- function(grid) {
    if (!is.null(grid$xyCoords$lat)) {
        stop("Rotated grids are not yet supported")
    }
    
    lats <- grid$xyCoords$y
    cos(lats / 360 * 2 * pi)
}



locAggregation <- function(grid, aggr.fun, weight.by.lat, parallel, max.ncores, ncores) {
      dimNames <- getDim(grid)
      if (!"loc" %in% dimNames) {
            message("There is not lat dimension: 'aggr.loc' option was ignored.")
      } else {
            if (isTRUE(weight.by.lat)) {
                  message("Calculating areal weights...")
                  lat.weights <- latWeighting(grid)
                  if (aggr.fun[["FUN"]] == "mean") {
                        aggr.fun <- list(FUN = "weighted.mean", w = lat.weights, na.rm = TRUE)
                  }
            }
            parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
            mar <- grep("loc", dimNames, invert = TRUE)
            aggr.fun[["MARGIN"]] <- mar
            aggr.fun[["X"]] <- grid$Data
            out <- if (parallel.pars$hasparallel) {
                  message("[", Sys.time(), "] - Aggregating lat dimension in parallel...")
                  on.exit(parallel::stopCluster(parallel.pars$cl))
                  aggr.fun[["cl"]] <- parallel.pars$cl
                  do.call("parApply", aggr.fun)
            } else {
                  message("[", Sys.time(), "] - Aggregating lat dimension...")
                  do.call("apply", aggr.fun)
            }
            grid$Data <- out
            attr(grid$Data, "dimensions") <- dimNames[mar]
            message("[", Sys.time(), "] - Done.")
      }
      return(grid)
}

#' @title Spatial Aggregation
#' @description Spatial aggregation for rectangular domains. 
#' @param grid Input grid. 
#' @param aggr.fun Aggregation function
#' @param weight.by.lat Logical flag
#' @return A spatially averaged time series grid (possibly multimember)
#' @keywords internal
#' @author J Bedia

spatialAggregation <- function(grid, aggr.fun, weight.by.lat, parallel, max.ncores, ncores) {
  stopifnot(is.logical(weight.by.lat))
  dimNames <- getDim(grid)
  if (!any(c("lon", "lat") %in% dimNames)) {
    message("\'lon\' and/or \'lat\' dimensions are missing in the input grid: 'aggr.spatial' option was ignored.")
  } else {
    if (isTRUE(weight.by.lat)) {
      lat.weights <- latWeighting(grid)
      weight.matrix <- matrix(lat.weights, nrow = getShape(grid, "lat"), ncol = getShape(grid, "lon"))
      if (aggr.fun[["FUN"]] == "mean") {
        message("Calculating areal weights...")
        aggr.fun <- list(FUN = "weighted.mean", w = weight.matrix, na.rm = TRUE)
      } else {
        message("Spatial weighting skipped: It only applies to \'mean\' aggregation function")
      }
    }
    parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
    mar <- grep("lat|lon", dimNames, invert = TRUE)
    aggr.fun[["MARGIN"]] <- mar
    aggr.fun[["X"]] <- grid$Data
    out <- if (parallel.pars$hasparallel) {
      message("[", Sys.time(), "] - Aggregating lat dimension in parallel...")
      on.exit(parallel::stopCluster(parallel.pars$cl))
      aggr.fun[["cl"]] <- parallel.pars$cl
      do.call("parApply", aggr.fun)
    } else {
      message("[", Sys.time(), "] - Aggregating spatially...")
      do.call("apply", aggr.fun)
    }
    grid$Data <- out
    attr(grid$Data, "dimensions") <- dimNames[mar]
    message("[", Sys.time(), "] - Done.")
  }
  return(grid)
}
