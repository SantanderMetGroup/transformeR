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
#' @examples \dontrun{
#' # Iberia domain
#' data(iberia_ncep_hus850)
#' getGrid(iberia_ncep_hus850)
#' # Europe domain
#' data(tasmin_forecast)
#' getGrid(tasmin_forecast)
#' plotMeanGrid(tasmin_forecast)
#' # Interpolate NCEP onto the System4 grid:
#' int <- interpGrid(iberia_ncep_hus850, getGrid(tasmin_forecast), "bilinear")
#' # Note the warnings because of the non-overlapping domain extents
#' plotMeanGrid(int)
#' # The other way round, using nearest neighbour interpolation:
#' int2 <- interpGrid(tasmin_forecast, getGrid(iberia_ncep_hus850))
#' plotMeanGrid(int2)
#' # In this case, the mismatch in domain extent occurs only in the longitudes (to the west)
#' }
#' 

getGrid <- function(gridData) {
    if (is.matrix(gridData$xyCoords)) {
          out <- list(x = as.numeric(gridData$xyCoords[,1]), y = as.numeric(gridData$xyCoords[,2]))
          attr(out, "type") <- "irregular"
          if (!exists("resX", attributes(gridData$xyCoords))) {
                attr(out, "resX") <- NULL
          }else{
                attr(out, "resX") <- attr(gridData$xyCoords, "resX")
          }
          if (!exists("resY", attributes(gridData$xyCoords))) {
                attr(out, "resY") <- NULL
          }else{
                attr(out, "resY") <- attr(gridData$xyCoords, "resY")   
          }
    }else{
        # if ("lon" %in% names(gridData$xyCoords)) stop("Not a regular grid")
        grid.x <- c(gridData$xyCoords$x[1], tail(gridData$xyCoords$x, 1))
        grid.y <- c(gridData$xyCoords$y[1], tail(gridData$xyCoords$y, 1))
        out <- list(x = grid.x, y = grid.y)
        attributes(out) <- attributes(gridData$xyCoords)
        if (!exists("resX", attributes(gridData$xyCoords))) {
            attr(out, "resX") <- (tail(gridData$xyCoords$x, 1) - gridData$xyCoords$x[1]) / (length(gridData$xyCoords$x) - 1)
        }else{
            attr(out, "resX") <- attr(gridData$xyCoords, "resX")
        }
        if (!exists("resY", attributes(gridData$xyCoords))) {
            attr(out, "resY") <- (tail(gridData$xyCoords$y, 1) - gridData$xyCoords$y[1]) / (length(gridData$xyCoords$y) - 1)
        }else{
            attr(out, "resY") <- attr(gridData$xyCoords, "resY")   
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

getCoordinates <- function(obj) {
    if (is.matrix(obj$xyCoords)){
        xy <- obj$xyCoords
        return(xy)
    }else{
        if(!is.matrix(obj$xyCoords) && exists("lon", obj$xyCoords) && exists("lat", obj$xyCoords)){
              x <- obj$xyCoords$lon
              y <- obj$xyCoords$lat 
        }else{
              x <- obj$xyCoords$x
              y <- obj$xyCoords$y
        }
        return(list("x" = x, "y" = y))
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
#' data(iberia_ncep_ta850)
#' getSeason(iberia_ncep_ta850) # Boreal winter (DJF)

getSeason <- function(obj) {
    if ("season" %in% names(attributes(obj$Dates))) {
        attr(obj$Dates, "season")
    } else {
        dimNames <- getDim(obj)
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
#' data(iberia_ncep_hus850)
#' getSeason(iberia_ncep_hus850)
#' # Winter 1991-2010
#' range(iberia_ncep_hus850$Dates$start)
#' ## Time series for the first point
#' # Dates vector
#' time <- as.POSIXlt(iberia_ncep_hus850$Dates$start, tz = "GMT")
#' hus850 <- iberia_ncep_hus850$Data[ ,1,1]
#' plot(time, hus850, ty = "l")
#' ## Computation of the annual series for winter specific humidity:
#' par(mfrow = c(2,1))
#' ## Wrong:
#' years <- as.POSIXlt(iberia_ncep_hus850$Dates$start)$year + 1900
#' x <- tapply(hus850, INDEX = list(years), FUN = mean)
#' plot(unique(years), x, ty = "b")
#' points(1990, x[1], col = "red", cex = 2, lwd = 2)
#' ## Correct:
#' years <- getYearsAsINDEX(iberia_ncep_hus850)
#' x <- tapply(hus850, INDEX = years, FUN = mean)
#' plot(unique(years), x, ty = "b")
#' par(mfrow = c(1,1))
#' 

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

getShape <- function(obj, dimension = NULL) {
    dimNames <- getDim(obj)
    shape <- dim(obj[["Data"]])
    if (!is.null(dimension)) {
        ind <- match(dimension, dimNames)
        if (anyNA(ind)) stop("Input 'dimension' value not found")
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
    load(system.file(package="transformeR","wrl.Rda"), envir = environment())
    for (i in 1:length(node.list)) {
        lines(node.list[[i]][,1], node.list[[i]][,2], ...)            
    }
}




#' @title Leap years
#' @description Return the indices of leap years in a vector of years
#' @param years a integer vector of (gregorian) years
#' @return a vector of indices of the position of leap years
#' @references \url{https://en.wikipedia.org/wiki/Leap_year}
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
#' @return A character vector of dates
#' @details This utility function should be used always that reference dates are internally required by any function.
#' It takes into account the particular case when \code{\link{redim}} is applied to create a singleton \code{"var"}
#'  dimension: in this situation, the \code{"Dates"} component is not changed accordingly, and thus the criterion to
#'  distinguish multigrids from redim-ed grids is the shape of the array.
#' @keywords internal
#' @family get.helpers
#' @export
#' @author J. Bedia

getRefDates <- function(obj) {
    if (("var" %in% getDim(obj)) && (getShape(obj, "var") > 1)) {
        obj$Dates[[1]]$start
    } else {
        obj$Dates$start 
    }
}


#' @title Select an appropriate *pply function
#' @description Selects an appropriate *pply function depending on the parallel choice
#' @param parallel.pars The output from \code{\link{parallelCheck}}, passing the parallelization options.
#' @param .pplyFUN *pply function to be used. Current options are \code{"apply"}, \code{"lapply"} and \code{"sapply"}.
#' @details The output function will be either \code{parallel::parLapply} or \code{lapply} for \code{.pplyFUN = "lapply"},
#' depending of whether parallelization is enabled or not. Same for \code{apply} and \code{parallel::parApply}.
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

selectPar.pplyFun <- function(parallel.pars, .pplyFUN = c("apply", "lapply", "sapply")) {
    .pplyFUN <- match.arg(.pplyFUN, choices = c("apply", "lapply", "sapply"))
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
        }
    } else {
        fun <- switch(.pplyFUN,
                      "apply" = apply,
                      "lapply" = lapply,
                      "sapply" = sapply)
    }
    return(fun)
}


#' @title Spatial check of input grids
#' @description Checks that the input grids have the same lon/lat shape
#' @param ... Input grids to be compared
#' @param dimension Character vector. Dimensions to check for consistency. Several dimensions can be tested at once.
#' Possible values are: \code{"member"}, \code{"time"}, \code{"lat"} and \code{"lon"}.  
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
#' data("EOBS_Iberia_tas")
#' data("EOBS_Iberia_tp")
#' checkDim(EOBS_Iberia_tas, EOBS_Iberia_tp) # ok, go on
#' data("iberia_ncep_psl")
#' try(checkDim(EOBS_Iberia_tas, EOBS_Iberia_tp, iberia_ncep_psl)) # inconsistent dimensions
#' data("tasmax_forecast")
#' try(checkDim(iberia_ncep_psl, tasmax_forecast, dimensions = "member")) 

checkDim <- function(..., dimensions = c("member", "time", "lat", "lon")) {
    grid.list <- list(...)
    grid.list <- lapply(grid.list, "redim")
    dimensions <- match.arg(dimensions, choices = c("member", "time", "lat", "lon"), several.ok = TRUE)
    dimlist <- lapply(dimensions, function(x) vapply(grid.list, "getShape", integer(1), x))
    oops <- dimensions[which(!sapply(dimlist, function(x) all(x == x[1])))]
    if (length(oops) > 0) {
        stop("Inconsistent sizes found for dimensions: ", paste(oops, collapse = ", "))
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
#' data("EOBS_Iberia_tp")
#' grid1 <- subsetGrid(EOBS_Iberia_tas, season = 1)
#' grid2 <- subsetGrid(EOBS_Iberia_tp, season = 1)
#' checkSeason(grid1, grid2) # ok, go on
#' try(checkSeason(grid1, grid2, EOBS_Iberia_tas)) # oops
#' 
#' # However note that it is insensitive to the level of temporal aggregation:
#' agg <- aggregateGrid(EOBS_Iberia_tas, aggr.m = list(FUN = "mean", na.rm = TRUE))
#' identical(getSeason(agg), getSeason(EOBS_Iberia_tas))
#' checkSeason(agg, EOBS_Iberia_tas)

checkSeason <- function(...) {
    grid.list <- list(...)
    sealist <- lapply(grid.list, "getSeason")
    oops <- sapply(sealist, function(x) all(x == sealist[[1]]))
    if (!all(oops)) stop("Inconsistent seasons among input grids")
}    
    

#' @title Check if the imput grid is regular
#' @description Check if the coordinates in the data are regular or irregular
#' @param grid a grid or station data, or the objectreturned by function \code{\link{getGrid}}
#' @return Logical.
#' @seealso \code{\link{getGrid}}
#' @author M Iturbide
#' @keywords internal
#' @family is.helpers
#' @export
#' @examples 
#' data("EOBS_Iberia_tas")
#' isRegular(EOBS_Iberia_tas)
#' isRegular(getGrid(EOBS_Iberia_tas))

isRegular <- function(grid){
      gr <- tryCatch({getGrid(grid)}, error = function(err){grid})
      x <- sort(gr$x)
      y <- sort(gr$y)
      xdists <- lapply(1:(length(x)-1), function(l){
            x[l+1] - x[l]
      })
      ydists <- lapply(1:(length(y)-1), function(l){
            y[l+1] - y[l]
      })
      xa <- sum(unlist(xdists) - unlist(xdists)[1])
      ya <- sum(unlist(ydists) - unlist(ydists)[1])
      if(any(c(xa, ya) > 0.00001)){
            FALSE
      }else{
            TRUE
      }
}

#End


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

array3Dto2Dmat.stations <- function(array3D) {
      dims <- dim(array3D)
      aux.list <- lapply(1:dims[1], function(i) {
            diag(array3D[i, ,])
      })
      mat <- unname(do.call("abind", c(aux.list, along = -1)))
      aux.list <- NULL
      attr(mat, "dimensions") <- c("time", "loc")
      return(mat)
}

#End
