##     helpers.R Helper functions of the climate4R bundle
##
##     Copyright (C) 2018 Santander Meteorology Group (http://www.meteo.unican.es)
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


#' @title Get regular grid definition 
#' @description Get the (regular) grid definition from an existing (gridded) dataset
#' @param gridData A grid data object coming from \code{loadGridData} (package \pkg{loadeR}) or \code{\link{interpGrid}}
#'  or the function \code{loadECOMS} of package \pkg{loadeR.ECOMS}.
#' @return A list of two named components, \code{x} and \code{y}, consisting of a vector of length two each one, defining
#' the x/y lower and upper bounds. The grid-cell resolution is given by the attributes \code{'resX'} and
#'  \code{'resY'} respectively.
#' @details In case of irregular grid definitions, the function forces the grid to regularity.
#' The returned grid object inherits the attributes from the input \code{xyCoords} definition.
#' @export
#' @importFrom utils tail
#' @family get.helpers
#' @author S. Herrera and J. Bedia 
#' @examples 
#' # Regular grid
#' data("NCEP_Iberia_hus850")
#' str(getGrid(NCEP_Iberia_hus850))
#' # Regular rotated grid with correpondence with a non regular grid in the 
#' # lat/lon domain.
#' data("CORDEX_Iberia_tas")
#' str(getGrid(CORDEX_Iberia_tas))
#' # Station data
#' data("VALUE_Iberia_tas")
#' str(getGrid(VALUE_Iberia_tas))
#' 
#' # Interpolate NCEP onto the System4 grid:
#' data("CFS_Iberia_pr")
#' NCEP_interpolated <- interpGrid(NCEP_Iberia_hus850, getGrid(CFS_Iberia_pr), "bilinear")
#' # Note the warnings because of the non-overlapping domain extents (longitudes)
#' plotClimatology(climatology(NCEP_Iberia_hus850), backdrop.theme = "countries")
#' plotClimatology(climatology(NCEP_interpolated), backdrop.theme = "countries")
#' str(getGrid(NCEP_interpolated))
#' str(getGrid(CFS_Iberia_pr))
#' 
#' # The other way round, using nearest neighbour interpolation:
#' CFS_interpolated <- interpGrid(CFS_Iberia_pr, getGrid(NCEP_Iberia_hus850))
#' plotClimatology(climatology(CFS_interpolated), backdrop.theme = "countries")
#' # In this case, the mismatch in domain extent occurs in the longitudes and latitudes
 

getGrid <- function(gridData) {
      rot <- FALSE
      if (is.data.frame(gridData$xyCoords)) {
            out <- list(x = as.numeric(gridData$xyCoords[,1]), y = as.numeric(gridData$xyCoords[,2]))
            attr(out, "type") <- "irregular"
            if (!exists("resX", attributes(gridData$xyCoords))) {
                  attr(out, "resX") <- NULL
            } else {
                  attr(out, "resX") <- attr(gridData$xyCoords, "resX")
            }
            if (!exists("resY", attributes(gridData$xyCoords))) {
                  attr(out, "resY") <- NULL
            } else {
                  attr(out, "resY") <- attr(gridData$xyCoords, "resY")   
            }
      }else{
            if ("lon" %in% names(gridData$xyCoords)) rot <- TRUE
            grid.x <- c(gridData$xyCoords$x[1], tail(gridData$xyCoords$x, 1))
            grid.y <- c(gridData$xyCoords$y[1], tail(gridData$xyCoords$y, 1))
            out <- list(x = grid.x, y = grid.y)
            if (rot) {
                  out$lon <- gridData$xyCoords$lon 
                  out$lat <- gridData$xyCoords$lat
            }
            attributes(out) <- attributes(gridData$xyCoords)
            if (!exists("resX", attributes(gridData$xyCoords))) {
                  attr(out, "resX") <- (tail(gridData$xyCoords$x, 1) - gridData$xyCoords$x[1]) / (length(gridData$xyCoords$x) - 1)
            } else {
                  attr(out, "resX") <- attr(gridData$xyCoords, "resX")
            }
            if (!exists("resY", attributes(gridData$xyCoords))) {
                  attr(out, "resY") <- (tail(gridData$xyCoords$y, 1) - gridData$xyCoords$y[1]) / (length(gridData$xyCoords$y) - 1)
                  
            } else{ 
                  attr(out, "resY") <- attr(gridData$xyCoords, "resY")   
            }
            if (rot) {
                  attr(out, "resLON") <- NA 
                  attr(out, "resLAT") <- NA
            } 
      }
      return(out)
}
# End


#' @title Get geographical coordinates of a climate data object
#' @description Returns the coordinates of a climate data object, either stations or grid
#' @param obj Any object extending the station or grid classes
#' @return A list with x and y components
#' @family get.helpers
#' @author J. Bedia 
#' @export
#' @examples 
#' # Regular grid
#' data("NCEP_Iberia_hus850")
#' str(getCoordinates(NCEP_Iberia_hus850))
#' # Regular rotated grid with correpondence with a non regular grid in the 
#' # lat/lon domain.
#' data("CORDEX_Iberia_tas")
#' str(getCoordinates(CORDEX_Iberia_tas))
#' # Station data
#' data("VALUE_Iberia_tas")
#' str(getCoordinates(VALUE_Iberia_tas))


getCoordinates <- function(obj) {
    if (is.data.frame(obj$xyCoords)) {
        xy <- obj$xyCoords#[ , ,drop = FALSE]
        return(xy)
    } else {
        x <- obj$xyCoords$x
        y <- obj$xyCoords$y
        if (!is.data.frame(obj$xyCoords) && exists("lon", where = obj$xyCoords) && exists("lat", where = obj$xyCoords)) {
            lon <- obj$xyCoords$lon 
            lat <- obj$xyCoords$lat
            return(list("x" = x, "y" = y, "lon" = lon, "lat" = lat))
        } else {
            return(list("x" = x, "y" = y))
        }
    }
}
# End



#' @title Get season 
#' @description Retrieves the season encompassed by a station or grid object
#' @param obj Any object extending the station or grid classes
#' @return An integer vector with the season
#' @author J. Bedia 
#' @family get.helpers
#' @export
#' @examples 
#' data("NCEP_Iberia_hus850")
#' getSeason(NCEP_Iberia_hus850) # Boreal winter (DJF)

getSeason <- function(obj) {
      if ("season" %in% names(attributes(obj$Dates))) {
            attr(obj$Dates, "season")
      } else {
            aux <- if (is.null(names(obj$Dates))) {
                  as.integer(substr(obj$Dates[[1]]$start, 6,7))      
            } else {
                  as.integer(substr(obj$Dates$start, 6,7))      
            }
            unique(aux)
      }
}
# End


#' @title Get years as a factor
#' @description Extract the year as a factor (e.g. for computing annual statistics)
#' @param obj Any object extending the station or grid classes
#' @return A vector of years of the same length as the time dimension of the object, 
#' seasonally-adjusted in the case of year-crossing seasons (e.g. DJF). See details.
#' @details The function performs a very basic operation, extracting the year element from the 
#' dates previously converted to POSIXlt. The trick lies in the year-crossing seasons. For instance:
#'  by convention, winter 2001 encompasses December 2000 and January, February 2001. Therefore, in order to compute
#' annual statistics for a year-crossing season, it is necessary to modify first the vector of years, 
#' and assign year 2001 to the preceding December. Similarly, the next December 2001 belongs to winter 2002,
#'  and so on... The function is useful for computing and/or plotting annual statistics, seasonal climatologies ... 
#' @note Warning:
#' The function should no be used to extract the vector of actual date years
#' @family get.helpers
#' @author J. Bedia 
#' @export
#' @examples 
#' data("NCEP_Iberia_hus850")
#' getSeason(NCEP_Iberia_hus850)
#' # Winter 1983-2002
#' range(NCEP_Iberia_hus850$Dates$start)
#' ## Time series for the first point
#' # Dates vector
#' time <- as.POSIXlt(NCEP_Iberia_hus850$Dates$start, tz = "GMT")
#' hus850 <- NCEP_Iberia_hus850$Data[ ,1,1]
#' plot(time, hus850, ty = "l")
#' ## Computation of the annual series for winter specific humidity:
#' par(mfrow = c(2,1))
#' ## Wrong:
#' years <- as.POSIXlt(NCEP_Iberia_hus850$Dates$start)$year + 1900
#' x <- tapply(hus850, INDEX = list(years), FUN = mean)
#' plot(unique(years), x, ty = "b")
#' points(1990, x[1], col = "red", cex = 2, lwd = 2)
#' ## Correct:
#' years <- getYearsAsINDEX(NCEP_Iberia_hus850)
#' x <- tapply(hus850, INDEX = years, FUN = mean)
#' plot(unique(years), x, ty = "b")
#' par(mfrow = c(1,1))
#' 
#' #Station data:
#' data("VALUE_Iberia_tas")
#' getYearsAsINDEX(VALUE_Iberia_tas)


getYearsAsINDEX <- function(obj) {
      season <- getSeason(obj)
      aux.dates <- getRefDates(obj)
      yrs <- as.numeric(substr(aux.dates,1,4))
      mon <- as.numeric(substr(aux.dates,6,7))
      if (identical(yrs, unique(yrs))) {
            yrs
            if (!identical(season, sort(season))) {
                  yrs <- yrs + 1
            }
      } else {    
            if (!identical(season, sort(season))) {
                  yy <- unique(yrs)[-1]
                  aux <- match(mon, season)
                  brks <- c(1, which(diff(aux) < 0) + 1, length(aux) + 1)
                  l <- lapply(1:(length(brks) - 1), function(x) {
                        a <- yrs[brks[x]:(brks[x + 1] - 1)]
                        return(rep(yy[x], length(a)))
                  })
                  yrs  <- do.call("c", l)
            }
      }
      return(yrs)
}
# End

#' @title  Retrieve dimensions attribute
#' @description Retrieve dimensions attribute
#' @param obj A grid or station object
#' @return A character vector with the dimensions attribute of the object's \code{Data} component.
#' @keywords internal
#' @family get.helpers
#' @export
#' @author J. Bedia
#' @examples 
#' #Station data:
#' data("VALUE_Iberia_tas")
#' getDim(VALUE_Iberia_tas)
#' #Regular grid
#' data("NCEP_Iberia_hus850")
#' getDim(NCEP_Iberia_hus850)

getDim <- function(obj) {
      attr(obj[["Data"]], "dimensions")
}

#' @title  Retrieve array shape 
#' @description Retrieve array attributes 'dimensions' and 'dim'
#' @param obj A grid or station object
#' @return An integer vector with dim values, labelled with the \code{"dimension"} attribute names
#' @family get.helpers
#' @keywords internal
#' @export
#' @author J. Bedia
#' @examples 
#' #Station data:
#' data("VALUE_Iberia_tas")
#' getShape(VALUE_Iberia_tas)
#' #Regular grid
#' data("NCEP_Iberia_hus850")
#' getShape(NCEP_Iberia_hus850)

getShape <- function(obj, dimension = NULL) {
      dimNames <- getDim(obj)
      shape <- dim(obj[["Data"]])
      if (!is.null(dimension)) {
            ind <- match(dimension, dimNames)
            if (anyNA(ind)) message("NOTE: Some of the specified dimensions do not exist in the grid")
            shape <- shape[ind]
            dimNames <- dimNames[ind]
      }
      names(shape) <- dimNames
      return(shape)
}


#' @title Land borders
#' @description Add land borders to a map
#' @param ... Graphical parameters passed to \code{\link{lines}}.
#' @return Draws a simplied land border areas as lines onto the map
#' @details The function loads a built-in world segments dataset created ad hoc to avoid dependencies on other packages (i.e. 'maps').
#' Geographical lonlat coordinates in wgs84.
#' @source Postprocessed from the original shapefile from Natural Earth (http://www.naturalearthdata.com/downloads/110m-physical-vectors/)
#' @author J. Bedia
#' @keywords internal
#' @importFrom graphics lines
#' @export

draw.world.lines <- function(...) {
      load(system.file(package = "transformeR","wrl.Rda"), envir = environment())
      for (i in 1:length(node.list)) {
            lines(node.list[[i]][,1], node.list[[i]][,2], ...)            
      }
}




#' @title Leap years
#' @description Return the indices of leap years in a vector of years
#' @param years a integer vector of (gregorian) years
#' @return a vector of indices of the position of leap years
#' @references \url{https://en.wikipedia.org/wiki/Leap_year#Algorithm}
#' @keywords internal
#' @export
#' @family get.helpers
#' @author J. Bedia 
#' @examples
#' leap.years <- which.leap(1885:1937)
#' (1885:1937)[leap.years]

which.leap <- function(years) {
      which((years %% 4 == 0) & ((years %% 100 != 0) | years %% 400 == 0))
}


#' @title Retrieve reference dates
#' @description Retrieves the \code{$start} component of the \code{$Dates} element of a grid or station.
#'  In case of multigrids, the dates from the first grid are returned (i.e.: \code{grid$Dates[[1]]$start}).
#' @param obj A grid or station object
#' @param which Character string. The element of the \code{$Dates} list to be extracted.
#'  Default to \code{"start"}, otherwise \code{"end"}.
#' @return A character vector of dates
#' @details This utility function should be used always that reference dates are internally required by any function.
#' It takes into account the particular case when \code{\link{redim}} is applied to create a singleton \code{"var"}
#'  dimension: in this situation, the \code{"Dates"} component is not changed accordingly, and thus the criterion to
#'  distinguish multigrids from redim-ed grids is the shape of the array.
#' @keywords internal
#' @family get.helpers
#' @export
#' @author J. Bedia
#' @examples
#' #Station data:
#' data("VALUE_Iberia_tas")
#' str(getRefDates(VALUE_Iberia_tas))
#' #Regular grid
#' data("NCEP_Iberia_hus850")
#' str(getRefDates(NCEP_Iberia_hus850, which = "end"))

getRefDates <- function(obj, which = "start") {
    which <- match.arg(which, choices = c("start", "end"))
    ind <- switch(which, "start" = 1, "end" = 2)
    if (("var" %in% getDim(obj)) && (getShape(obj, "var") > 1)) {
        obj$Dates[[1]][[ind]]
    } else {
        obj$Dates[[ind]]
    }
}


#' @title Select an appropriate *pply function
#' @description Selects an appropriate *pply function depending on the parallel choice
#' @param parallel.pars The output from \code{\link{parallelCheck}}, passing the parallelization options.
#' @param .pplyFUN *pply function to be used. Current options are \code{"apply"}, \code{"lapply"}, \code{"sapply"} and \code{"mapply"}.
#' @details The output function will be either \code{parallel::parLapply} or \code{lapply} for \code{.pplyFUN = "lapply"},
#' depending of whether parallelization is enabled or not. Same for \code{apply} and \code{parallel::parApply} and so on.
#' @section Warning:
#' 
#' From the \code{\link[parallel]{makeCluster}} help: 
#' \dQuote{It is good practice to shut down the workers by calling \code{stopCluster}}. It is therefore recommended the
#' following line of code after using this function: \code{on.exit(parallel::stopCluster(parallel.pars$cl))}
#' 
#' @importFrom parallel parApply parLapply
#' @return A function
#' @keywords internal
#' @family parallel.helpers
#' @export
#' @author J. Bedia

selectPar.pplyFun <- function(parallel.pars, .pplyFUN = c("apply", "lapply", "sapply", "mapply")) {
    .pplyFUN <- match.arg(.pplyFUN, choices = c("apply", "lapply", "sapply", "mapply"))
    if (parallel.pars$hasparallel) {
        if (.pplyFUN == "apply") {
            fun <- function(...) {
                parallel::parApply(cl = parallel.pars$cl, ...)
            }
        } else if (.pplyFUN == "lapply") {
            fun <- function(...) {
                parallel::parLapply(cl = parallel.pars$cl, ...)
            }
        } else if (.pplyFUN == "sapply") {
            fun <- function(...) {
                parallel::parSapply(cl = parallel.pars$cl, ...)
            }
        } else if (.pplyFUN == "mapply") {
            fun <- function(...) {
                parallel::clusterMap(cl = parallel.pars$cl, ..., SIMPLIFY = TRUE)
            }
        }
    } else {
        fun <- switch(.pplyFUN,
                      "apply" = apply,
                      "lapply" = lapply,
                      "sapply" = sapply,
                      "mapply" = mapply)
    }
    return(fun)
}


#' @title Spatial check of input grids
#' @description Checks that the input grids have the same lon/lat shape
#' @param ... Input grids to be compared
#' @param dimension Character vector. Dimensions to check for consistency. Several dimensions can be tested at once.
#' Possible values are: \code{"var"}, \code{"member"}, \code{"time"}, \code{"lat"} and \code{"lon"}.  
#' @return In case of spatial inconsistency of any of the inputs grids, 
#' the function stops the execution of the current expression, with an error message.
#' @description The function simply compares the size of the dimension across input grids for 
#' structural consistency of their respective data arrays. It does not make any additional checks on the metadata,
#' so equal array shapes do not entail full temporal/spatial consistency.
#' @seealso \code{\link{getShape}}
#' @keywords internal 
#' @export
#' @family check.helpers
#' @author J Bedia
#' @examples 
#' data("VALUE_Iberia_tas")
#' data("VALUE_Iberia_pr")
#' checkDim(VALUE_Iberia_tas, VALUE_Iberia_pr) # ok, go on
#' data("EOBS_Iberia_tas")
#' data("EOBS_Iberia_pr")
#' checkDim(EOBS_Iberia_tas, EOBS_Iberia_pr) # ok, go on
#' data("NCEP_Iberia_psl")
#' try(checkDim(EOBS_Iberia_tas, EOBS_Iberia_pr, NCEP_Iberia_psl)) # inconsistent dimensions
#' data("CFS_Iberia_tas")
#' try(checkDim(NCEP_Iberia_psl, CFS_Iberia_tas, dimensions = "member")) 
#' checkDim(VALUE_Iberia_tas, VALUE_Iberia_pr) # ok, go on


checkDim <- function(..., dimensions = c("var", "member", "time", "lat", "lon")) {
    grid.list <- list(...)
    grid.list <- lapply(grid.list, "redim")
    dimensions <- match.arg(dimensions, choices = c("var", "member", "time", "lat", "lon"), several.ok = TRUE)
    dimlist <- lapply(dimensions, function(x) vapply(grid.list, "getShape", integer(1), x))
    oops <- dimensions[which(!sapply(dimlist, function(x) all(x == x[1])))]
    if (length(oops) > 0) {
        stop("Inconsistent sizes found for dimensions: ", paste(oops, collapse = ", "), call. = FALSE)
    }
}


#' @title Seasonal check
#' @description Check the consistency of seasons across the input grids
#' @param ... Input grids to be compared
#' @return In case of seasonal inconsistency of any of the inputs grids, the function stops the execution 
#' of the current expression, with an error message.
#' @seealso \code{\link{getSeason}}
#' @author J Bedia
#' @keywords internal
#' @family check.helpers
#' @export
#' @examples 
#' data("EOBS_Iberia_tas")
#' data("EOBS_Iberia_pr")
#' grid1 <- subsetGrid(EOBS_Iberia_tas, season = 1)
#' grid2 <- subsetGrid(EOBS_Iberia_pr, season = 1)
#' checkSeason(grid1, grid2) # ok, go on
#' \donttest{
#' try(checkSeason(grid1, grid2, EOBS_Iberia_tas)) # oops
#' }
#' # However note that it is insensitive to the level of temporal aggregation:
#' agg <- aggregateGrid(EOBS_Iberia_tas, aggr.m = list(FUN = "mean", na.rm = TRUE))
#' identical(getSeason(agg), getSeason(EOBS_Iberia_tas))
#' checkSeason(agg, EOBS_Iberia_tas)
#' 
#' #Station data
#' data("VALUE_Iberia_tas")
#' data("VALUE_Iberia_pr")
#' checkSeason(VALUE_Iberia_tas, VALUE_Iberia_pr)


checkSeason <- function(...) {
    grid.list <- list(...)
    sealist <- lapply(grid.list, "getSeason")
    oops <- sapply(sealist, function(x) all(x == sealist[[1]]))
    if (!all(oops)) stop("Inconsistent seasons among input grids")
}    


#' @title Variable name check
#' @description Check the consistency of variable (short)names across grids/multigrids
#' @param ... Input grids to be compared
#' @param check.order Check if the order is consistent across input grids. This is the default. When set to
#' \code{TRUE}, the function does not only check that the variable names match, but also that their order is the same
#' (in case of multigrids)
#' @return In case of inconsistency of any of the inputs grids, the function stops the execution 
#' of the current expression, with an error message.
#' @seealso \code{\link{getVarNames}}
#' @author J Bedia
#' @keywords internal
#' @family check.helpers
#' @export
#' @examples 
#' data("NCEP_Iberia_hus850", "NCEP_Iberia_psl", "NCEP_Iberia_ta850")
#' #' # Testing grids that contain different variables:
#' \donttest{
#' try(checkVarNames(NCEP_Iberia_hus850, NCEP_Iberia_psl))
#' }
#' # Testing multigrid contents:
#' mg <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' mg1 <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_ta850, NCEP_Iberia_psl)
#' mg2 <- makeMultiGrid(NCEP_Iberia_ta850, NCEP_Iberia_psl, NCEP_Iberia_hus850)
#' # The input grids contains the same variables, but their ordering is different:
#' checkVarNames(mg, mg1, mg2, check.order = FALSE) # OK
#' # If we do check.order = TRUE (the default), it gives an error:
#' \donttest{
#' try(checkVarNames(mg, mg1, mg2, check.order = TRUE)) # Error
#' }

checkVarNames <- function(..., check.order = TRUE) {
    grid.list <- list(...)
    if (length(grid.list) == 1) {
        message("NOTE: Only one input grid: 'checkVarNames' was skipped")   
    } else {
        out <- lapply(grid.list, FUN = "getVarNames")
        lengths <- sapply(out, "length")
        if (!all(lengths == lengths[[1]])) {
            stop("The number of variables stored in the input grids differ")
        }
        if (!all(sapply(2:length(out), function(x) all(out[[x]] %in% out[[1]])))) {
            stop("Variable names of the input grids differ")
        }
        if (check.order) {
            if (!all(sapply(2:length(out), function(x) identical(out[[x]], out[[1]])))) {
                stop("Variable ordering of the input grids differ")
            }
        }
    }
}

#' @title Temporal consistency check across input grids
#' @description Check the consistency of temporal aspects acroos input climate4R objects
#' @param ... Input grids to be compared. Either as a list, or comma sepparated
#' @return In case of inconsistency of any of the inputs grids, the function stops the execution 
#' of the current expression, with an error message.
#' @seealso \code{\link{checkSeason}}, for a more specific -less strict- type of check
#' @details The function takes into account the temporal resolution of the input grids via 
#' the \code{\link{getTimeResolution}} helper, and ensures its consistency. Then, it makes the 
#' comparison among inputs at that temporal level of resolution. For instance, if the input grids are all 
#' monthly, the function will intercompare only the consistency of years AND months, but will disregard
#' lower temporal units (days, hours). 
#' @author J Bedia
#' @keywords internal
#' @importFrom magrittr %>% %<>% 
#' @family check.helpers
#' @export

checkTemporalConsistency <- function(...) {
    grid.list <- list(...)
    if (length(grid.list) == 1) grid.list %<>% unlist(recursive = FALSE)
    if (length(grid.list) < 2) stop("Only one grid passed as input. Nothing was done", call. = FALSE)
    timeres <- sapply(grid.list, "getTimeResolution") %>% unique() 
    if (length(timeres) > 1) stop("Different time resolution grids can't be binded")
    refdates <- getRefDates(grid.list[[1]])
    refmon <- substr(refdates, 6, 7)
    refyr <- substr(refdates, 1, 4)
    refday <- substr(refdates, 9, 10)
    mssg <- "Input data are not temporally consistent"
    if (timeres == "MM") {
        for (i in 2:length(grid.list)) {
            testmon <- substr(getRefDates(grid.list[[i]]), 6, 7)
            testyr <- substr(getRefDates(grid.list[[i]]), 1, 4)
            if (!identical(refmon, testmon) | !identical(refyr, testyr)) stop(mssg)
        }
    } else if (timeres == "YY") {
        for (i in 2:length(grid.list)) {
            testyr <- substr(getRefDates(grid.list[[i]]), 1, 4)
            if (!identical(refyr, testyr)) stop(mssg)
        }
    } else if (timeres == "DD") {
        for (i in 2:length(grid.list)) {
            testmon <- substr(getRefDates(grid.list[[i]]), 6, 7)
            testyr <- substr(getRefDates(grid.list[[i]]), 1, 4)
            testday <- substr(getRefDates(grid.list[[i]]), 9, 10)
            if (!identical(refday, testday) | !identical(refyr, testyr) | !identical(refmon, testmon)) stop(mssg)
        }
    } else {
        if (!identical(as.POSIXlt(refdates), as.POSIXlt(getRefDates(grid.list[[i]])))) stop(mssg)
    }   
}





#' @title Check if the input grid is regular
#' @description Check if the coordinates in the data are regular or irregular
#' @param grid a grid or station data, or the object returned by function \code{\link{getGrid}}
#' @return Logical.
#' @seealso \code{\link{getGrid}}
#' @author M Iturbide
#' @keywords internal
#' @family is.helpers
#' @seealso typeofGrid, for a character representation of the type of grid
#' @export
#' @examples 
#' data("EOBS_Iberia_tas")
#' isRegular(EOBS_Iberia_tas)
#' isRegular(getGrid(EOBS_Iberia_tas))
#' data("VALUE_Iberia_tas")
#' isRegular(VALUE_Iberia_tas)

isRegular <- function(grid) {
    gr <- tryCatch({getGrid(grid)}, error = function(err) {grid})
    x <- sort(gr$x)
    y <- sort(gr$y)
    if (!is.null(attr(gr, "resX")) && !is.null(attr(gr, "resY"))) {
        if (attr(gr, "resX") == 0 && attr(gr, "resY") == 0) {
            FALSE 
        } else {
            TRUE
        }
    } else {
        if (length(x) == 1 && length(y) == 1) {
            FALSE
        } else {
            xdists <- lapply(1:(length(x) - 1), function(l) {
                x[l + 1] - x[l]
            })
            ydists <- lapply(1:(length(y) - 1), function(l) {
                y[l + 1] - y[l]
            })
            xa <- sum(unlist(xdists) - unlist(xdists)[1])
            ya <- sum(unlist(ydists) - unlist(ydists)[1])
            if (any(abs(c(xa, ya)) > 1e-05)) {
                FALSE
            } else {
                TRUE
            }
        }
    }
}

#End

#' @title Get time resolution
#' @description Get the time resolution of the input grid as a character
#' @param grid a grid or station data
#' @return A character representation of the time resolution. Current possible values are:
#' \code{"1h", "3h", "6h", "12h", "DD", "MM", "YY"}. If none of these matches, \code{"unknown"} is returned 
#' @author J Bedia
#' @keywords internal
#' @family get.helpers
#' @export
#' @examples 
#' data("EOBS_Iberia_tas")
#' getTimeResolution(EOBS_Iberia_tas)
#' monthly.grid <- aggregateGrid(EOBS_Iberia_tas, aggr.m = list(FUN = "mean", na.rm = TRUE))
#' stopifnot(identical(getTimeResolution(monthly.grid), "MM"))
#' annual.grid <- aggregateGrid(monthly.grid, aggr.y = list(FUN = "mean", na.rm = TRUE))
#' stopifnot(identical(getTimeResolution(annual.grid), "YY"))

getTimeResolution <- function(grid) {
    aux <- getRefDates(grid) 
    if (length(aux) == 1) {
        warning("The input grid is a climatology")
        out <- "unknown"
    } else {
        dft <- difftime(aux[2], aux[1], units = "hours") %>% as.numeric()
        out <- if (dft == 1) {
            "1h"
        } else if (dft == 3) {
            "3h"    
        } else if (dft == 6) {
            "6h"
        } else if (dft == 12) {
            "12h"
        } else if (dft == 24) {
            "DD"
        } else if (dft >= 672 & dft <= 744) {
            "MM"
        } else if (dft >= 8640 & dft <= 8784) {
            "YY"
        } else {
            "unknown"
        }
    }
    return(out)
}


#' @title Conversion 2D matrix into a 3D array for station data
#' @description Converts a 2D matrix of the form [time, lonlat] to a 3D array of the form
#' [time,lat,lon], in this order. Mainly for PCA analysis and grid reconstruction.
#' @param mat2D A 2D matrix with time in rows and lonlat in columns, as returned 
#' by \code{\link{array3Dto2Dmat.stations}} 
#' @param x unique X coordinates of the points, in ascending order
#' @param y As argument \code{x}, for the Y coordinates
#' @return A 3-dimensional array with the dimensions ordered: [time,lat,lon]
#' @importFrom abind abind
#' @family internal.helpers
#' @details The function is the inverse of \code{\link{array3Dto2Dmat.stations}} 
#' @author M. Iturbide
#' @seealso \code{\link{array3Dto2Dmat}}, which performs the inverse operation.

mat2Dto3Darray.stations <- function(mat2D, x, y) {
    mat <- matrix(NA, ncol = length(x), nrow = length(y))
    aux.list <- lapply(1:nrow(mat2D), function(i) {
        diag(mat) <- mat2D[i, ]
        mat
    })
    arr <- unname(do.call("abind", c(aux.list, along = -1)))
    aux.list <- NULL
    arr <- aperm(arr, perm = c(1,3,2))
    attr(arr, "dimensions") <- c("time", "lat", "lon")
    return(arr)
}
#End

#' @title Conversion of a 3D array to a 2D matrix for station data
#' @description Converts 3D arrays of the form [lon,lat,time] -not strictly in this order-,
#'  to 2D matrices of the form [time, grid-point], in this order. Mainly for PCA analysis.
#' @param array3D A 3-dimensional array with longitude, latitude and time dimensions
#' @return A 2-dimensional matrix with time in rows and grid-points in columns.
#' @author M. Iturbide
#' @keywords internal
#' @family internal.helpers
#' @seealso \code{\link{mat2Dto3Darray}}, which performs the inverse operation
#' 
array3Dto2Dmat.stations <- function(array3D) {
    dims <- dim(array3D)
    aux.list <- lapply(1:dims[1], function(i) {
        if (!is.matrix(array3D[i, ,])) {
            array3D[i, ,]
        } else{
            diag(array3D[i, ,])      
        }
    })
    mat <- unname(do.call("abind", c(aux.list, along = -1)))
    aux.list <- NULL
    attr(mat, "dimensions") <- c("time", "loc")
    return(mat)
}

#End


#' @title Get grid variable names
#' @description Get the names of the variables of a (multi)grid
#' @param grid A grid or station data
#' @param type Character. Should either the \code{"short"} (default) or the \code{"long"} variable name(s) be returned?.
#' See Details.
#' @param append.level Append the vertical level to the short name string?. Default to \code{TRUE}, ignored when \code{type = "long"}).
#'  See details.
#' @details 
#' 
#' \strong{About short names}
#' 
#' Note that for variables with vertical levels, if the option \code{append.level} is set to \code{TRUE} (the default),
#'  the function will "construct" the shortname following the climate4R convention,
#' that defines the code of the variable plus its vertical level, separated by a \dQuote{@@} symbol. 
#' This way, when querying grid variable names, the behaviour of \code{getVarNames} is different than directly accessing
#' the \code{varName} element of the grid structure as in, e.g.: \code{grid$Variable$varName}. Using \code{getVarNames} 
#' with \code{append.level = TRUE} is recommended in most applications when the variable string is needed.
#' 
#' \strong{About long names}
#' 
#' The long name is an optional attribute of a grid (not so the short name), and it is sometimes undefined (particularly in station datasets).
#' In this case, the function returns \code{NULL}.
#' @return A character vector with the variable name(s) in the indicated \code{type} format.
#' @author J Bedia
#' @keywords internal
#' @family get.helpers
#' @export
#' @examples 
#' data("CFS_Iberia_pr")
#' getVarNames(CFS_Iberia_pr)
#' grid <- CFS_Iberia_pr
#' getVarNames(CFS_Iberia_pr, "long")
#' ## Example with a multigrid
#' data(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' mg <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' getVarNames(mg)
#' # The level can be removed id needed:
#' getVarNames(mg, append.level = FALSE)
#' mg <- makeMultiGrid(CFS_Iberia_tas, CFS_Iberia_pr)
#' getVarNames(mg)
#' getVarNames(mg, "long")
#' ## Example with station data
#' data("VALUE_Iberia_tas")
#' ## The long name is an optional attribute, and might be undefined:
#' getVarNames(VALUE_Iberia_tas, "long")

getVarNames <- function(grid, type = c("short", "long"), append.level = TRUE) {
    stopifnot(is.logical(append.level))
    type <- match.arg(type, c("short", "long"))
    if (type == "short") {
        vnames <- grid$Variable$varName
        if (append.level) {
            if (!any(grepl(pattern = "@", vnames))) {
                lev <- grid$Variable$level
                ind <- suppressWarnings(which(!is.na(lev) & !is.null(lev)))
                if (length(ind) > 0) {
                    vnames[ind] <- vapply(ind, FUN.VALUE = character(1L), FUN = function(x) paste(vnames[x], lev[x], sep = "@"))    
                }
            }
        } else {
            vnames <- gsub("@.*$", "", vnames)    
        }
    } else {
        vnames <- gsub("\\\"","", attr(grid$Variable, "longname"))
    }
    return(vnames)
}

#' @title Get grid vertical levels
#' @description A helper function that returns a vector of the variable(s) vertical levels
#' @param grid Input grid
#' @param var.index A vector of indices indicating the positions of the variables whose level will be returned.
#' This can be either an integer vector of positions, or a character vector with the short names of the variables contained
#'   in the multigrid (see the \code{\link{getVarNames}} helper for more details). Default to \code{NULL} (levels of all variables)
#' @return A (short-)named vector with the vertical levels of the (multi)grid variable(s)
#' @author J Bedia
#' @keywords internal
#' @export
#' @family get.helpers
#' @examples 
#' data("CFS_Iberia_hus850")
#' getGridVerticalLevels(CFS_Iberia_hus850)
#' # Surface variables usually have an undefined vertical level <NA>
#' data("EOBS_Iberia_tas")
#' getGridVerticalLevels(EOBS_Iberia_tas)
#' 
#' data("NCEP_Iberia_hus850")
#' data("NCEP_Iberia_psl")
#' data("NCEP_Iberia_ta850")
#' mg <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' getGridVerticalLevels(mg)
#' # Use of var.index to select some variables:
#' # either by their shortname...
#' getGridVerticalLevels(mg, var.index = c("hus@@850","ta@@850"))
#' # ... or by index position in the multigrid
#' getGridVerticalLevels(mg, var.index = c(1,3))

getGridVerticalLevels <- function(grid, var.index = NULL) {
    var.names <- getVarNames(grid)
    var.index <- if (is.null(var.index)) {
        1:length(var.names)
    } else if (is.character(var.index)) {
        match(var.index, var.names)
    } else {
        var.index
    }
    if (!all(var.index %in% 1:length(var.names))) {
        stop("Invalid 'var.index' definition", call. = FALSE)
    }
    levs <- grid$Variable$level[var.index]
    # NULL values (surface vars) are converted to NA
    if (any(is.null(levs))) {
        levs[which(is.null(levs))] <- NA
    }
    names(levs) <- var.names[var.index]
    return(levs)
}


#' @title Type of grid
#' @description Describe the type of grid according to the spatial representation of the data
#' @param grid An input grid
#' @return A character vector decribing the type of spatial reference. Three disjoint values possible: \code{"station"}, \code{"rotated_grid"} and \code{"regular_grid"}
#' @keywords internal
#' @export
#' @author J Bedia
#' @family get.helpers

typeofGrid <- function(grid) {
    ref.grid <- getGrid(grid)
    if (attr(ref.grid, "resX") == 0) {
        "station"
    } else if (is.na(attr(ref.grid, "resX"))) {
        "rotated_grid"
    } else {
        "regular_grid"
    }
}    





#' @title Level depth in a list
#' @description Level depth in a list 
#' 
#' @param this list
#' @export
#' @return number of nesting lists
#' @keywords internal
#' @author M. Iturbide 

gridDepth <- function(this) {
    that <- this
    i <- 0
    while (is.list(that)) {
        i <- i + 1
        that <- that[[1]]
    }
    return(i)
}
#end



#' Reorder loc dimension in irregular grids
#' 
#' Retrieves a grid that is with the 'loc' dimension reordered.
#'  Multimember multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input irregular grid to be subset (possibly a multimember/multigrid).
#' @param lon Logical to order data according to longitudes.
#' @param lat Logical to order data according to latitudes.
#' @return An irregular grid (or multigrid).
#' @keywords internal
#' @importFrom magrittr %>% %<>%
#' @export
#' @author M. Iturbide

reorderStation <- function(grid, axis = c("x", "y")) {
    dimNames <- getDim(grid)
    axis <- match.arg(axis, choices = c("x", "y"))
    if (isRegular(grid)) stop("This function is applied only to irregular grids")
    indloc <- order(getCoordinates(grid)[[axis]])
    grid$Data <- asub(grid$Data, idx = indloc, dims = grep("loc", dimNames), drop = FALSE)
    attr(grid$Data, "dimensions") <- dimNames
    grid$xyCoords <- grid$xyCoords[indloc,]
    if ("Metadata" %in% names(grid)) {
        if ("station_id" %in% names(grid$Metadata)) grid$Metadata$station_id <- grid$Metadata$station_id[indloc]
        if ("name" %in% names(grid$Metadata)) grid$Metadata$name <- grid$Metadata$name[indloc]
        if ("altitude" %in% names(grid$Metadata)) grid$Metadata$altitude <- grid$Metadata$altitude[indloc]      
        if ("source" %in% names(grid$Metadata)) grid$Metadata$source <- grid$Metadata$source[indloc]
    }
    return(grid)
}
#end


#' @title Get grid coordinates as 2D matrix
#' @description Obtain grid coordinates as 2D matrix
#' @param grid An input grid
#' @return A 2D matrix of x-y coordinates (in this order)
#' @keywords internal
#' @author J. Bedia
#' @family get.helpers
#' @export

get2DmatCoordinates <- function(grid) {
    if (typeofGrid(grid) == "regular_grid") {
        coords <- getCoordinates(grid) 
        aux <- expand.grid(coords)
        aux[order(aux[,1]), ]
    } else if (typeofGrid(grid) == "station") {
        getCoordinates(grid)
    } else if (typeofGrid(grid) == "rotated_grid") {
        stop("2D coordinates matrix is not yet supported for rotated grids", call. = FALSE)
    }
}


#' Check if object is a grid
#' 
#'
#' @param grid Input object.
#' @return Logical.
#' @keywords internal
#' @export
#' @author M. Iturbide

isGrid <- function(grid) {
    if (is.list(grid)) {
        all(c("Variable", "Data", "xyCoords", "Dates") %in% names(grid))
    } else {
        FALSE
    }
}


#' Check if object is a multigrid
#' 
#'
#' @param grid Input object.
#' @return Logical.
#' @keywords internal
#' @export
#' @author M. Iturbide

isMultigrid <- function(grid) {
    if (is.list(grid)) {
        if (all(c("Variable", "Data", "xyCoords", "Dates") %in% names(grid))) {
            gridDepth(grid$Dates) > 1
        } else {
            FALSE
        }
    } else {
        FALSE
    }
}



#' @title Get grid units
#' @description Get the \code{"units"} attribute of a grid
#' @param grid An input grid
#' @param var Character vector of variable length. Variable short name(s) whose units are returned.
#'  Only makes sense in the case of multigrids storing several variables.
#'   Otherwise ignored.
#' @return Returns the \code{"units"} attribute 
#' @keywords internal
#' @export
#' @author J Bedia
#' @family get.helpers unit.helpers
#' @examples 
#' data(NCEP_Iberia_ta850)
#' getGridUnits(NCEP_Iberia_ta850)
#' data(NCEP_Iberia_hus850)
#' getGridUnits(NCEP_Iberia_hus850)
#' data(NCEP_Iberia_psl)
#' getGridUnits(NCEP_Iberia_psl)
#' mf <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' getGridUnits(mf)
#' getVarNames(mf)
#' getGridUnits(mf, "hus@850")
#' getGridUnits(mf, var = c("hus@850", "ta@850"))

getGridUnits <- function(grid, var = NULL) {
    uds <- attr(grid$Variable, "units") %>% gsub(pattern = "\\\"", replacement = "")
    if (isMultigrid(grid)) {
        if (is.null(var)) {
            message("NOTE: The input is a multigrid: Units of all variables are shown.\nUse argument 'var' for displaying the units of a particular variable")
        } else {
            vn <- getVarNames(grid)
            if (!all(var %in% vn)) stop("At least one variable in \'var\' was not found. Use \'getVarNames\' for help")
            uds <- uds[match(var, vn)]
        }
    } else {
        if (!is.null(var)) warning("The input \'grid\' is not a multigrid: argument \'var\' was ignored")
    }
    return(uds)
}


#' @title Set grid units
#' @description Set the \code{"units"} attribute of a grid
#' @param grid An input grid
#' @param unit.string Character string: a udunits-parseable character string vector.
#'  See details.
#' @param var In case of multigrids, the names of the variables whose units attribute is
#'   to be updated (see examples).
#' @return Retunrs (invisible) the same input grid with the new \code{"units"} 
#' attribute in \code{"$Variable"} list element.
#' @details 
#' The length of the \code{unit.string} vector should match the number of variables
#'  within the grid (in case of \code{multiGrids}), i.e., that of 
#'  getVarNames(grid) or the length of \code{var}, in case the latter is used.
#' @export
#' @author J Bedia
#' @family get.helpers unit.helpers
#' @examples 
#' data(NCEP_Iberia_hus850)
#' getGridUnits(NCEP_Iberia_hus850)
#' data(NCEP_Iberia_psl)
#' getGridUnits(NCEP_Iberia_psl)
#' mf <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' getGridUnits(mf)
#' mf2 <- setGridUnits(mf, unit.string = c("1", "Pa", "Kelvin"))
#' getGridUnits(mf2)
#' # Arbitrary subsets of variables within the multigrid can be updated:
#' getVarNames(mf)
#' mf3 <- setGridUnits(mf, unit.string = c("1", "Pa"), var = c("hus@850", "psl"))
#' getGridUnits(mf3)

setGridUnits <- function(grid, unit.string, var = NULL) {
    stopifnot(isGrid(grid))
    vn <- getVarNames(grid)
    if (isMultigrid(grid)) {
        if (is.null(var)) {
            if (length(vn) != length(unit.string)) {
                stop("The length of the \'unit.string\' vector does not match the number of variables in the grid")
            }
            ind <- 1:length(vn)
        } else {
            if (!all(var %in% vn)) stop("At least one variable in \'var\' was not found. Use \'getVarNames\' for help")
            if (length(unit.string) != length(var)) stop("Inconsistent \'unit.string\' and \'var\' vector lengths")
            ind <- match(var, vn)
        }
    } else {
        if (length(unit.string) > 1) stop("\'unit.string\' vector should have length 1")
        ind <- 1L
    }
    attr(grid$Variable, "units")[ind] <- unit.string
    invisible(grid)
}





