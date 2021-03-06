##     subsetGrid.R Arbitrary subsetting of grids along one or more of its dimensions
##
##     Copyright (C) 2020 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Select an arbitrary subset from a grid or multigrid along one or more of its dimensions
#' @description Create a new grid/multigrid that is a subset of the input grid along the selected dimensions
#' @param grid The input grid or station data to be subset. This is either a grid (or station data), as 
#' returned e.g. by \code{loadeR::loadGridData} (or \code{loadeR::loadStationData}), a
#' multigrid, as returned by \code{makeMultiGrid}, or other types of multimember grids
#' (possibly multimember grids) as returned e.g. by \code{loadeR.ECOMS::loadECOMS}.
#' @param var Character vector indicating the variables(s) to be extracted. (Used for multigrid subsetting). See details.
#' @inheritParams subsetCluster
#' @param members An integer vector indicating \strong{the position} of the members to be subset.
#' @param runtime An integer vector indicating \strong{the position} of the runtimes to be subset.
#' @param years The years to be selected. Note that this can be either a continuous or discontinuous
#' series of years, the latter option often used in a cross-validation framework.
#'  See details for year-crossing seasons. Default to \code{NULL} (no subsetting is performed on the time dimension).
#' @param season An integer vector indicating the months to be subset. 
#' @param latLim Same as \code{lonLim} argument, but for latitude.
#' @param lonLim Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees,
#'  of the bounding box defining the subset. For single-point subsets, a numeric value with the
#'  longitude coordinate. If \code{NULL} (default), no subsetting is performed on the longitude dimension
#' @param outside if TRUE subset coordinates outside the grid extent are allowed. Default is FALSE.
#' @param station.id Station ID (check \code{$Metadata$station_id}).
#' @param drop Logical (default is TRUE). Drop or keep dimensions of length 1.
#' @return A new grid object that is a logical subset of the input grid along the specified dimensions.
#' @details
#' 
#' The attribute \code{subset} will be added to the different slots corresponding to the subset dimensions, taking
#' the value of the subroutine called in each case (e.g.: attribute subset will have the value \code{subsetSpatial}
#' in the xyCoords slot after spatial subsetting...).
#' 
#' \strong{Time slicing}
#' 
#' In case of year-crossing seasons (e.g. boreal winter (DJF), \code{season = c(12,1,2)}),
#' the season is assigned to the years of January and February 
#' (i.e., winter of year 2000 corresponds to Dec 1999, Jan 2000 and Feb 2000). Thus, 
#' the \code{years} argument must be introduced accordingly (See e.g. \code{\link{getYearsAsINDEX}}
#' function for details). Hence, when subsetting along \code{season}, some data might be lost when using year-crossing
#' seasons. For example, assume a dataset encompassing a full-year season (i.e., \code{season=1:12}) for the period 1981-2010
#'  (i.e., \code{years=1981:2010}). When performing a subset on boreal winter (DJF, \code{season = c(12,1,2)}),
#'  the first available winter will be \dQuote{winter 1982}, encompassing Dec 1981 and Jan and Feb 1982. Thus, all data corresponding to
#'  Jan and Feb 1981 are discarded from the subset (i.e., only complete \dQuote{winters} will be returned). Similarly,
#'  December 2010 will be lost (because it belongs to winter 2011, beyond the temporal extent of the dataset),
#'  and the last data provided will correspond to winter 2009. To override this default behaviour and retaining all
#'  January, February and December records strictly within the period 1981-2010, 
#'  the non-standard \code{season=c(1,2,12)} can be specified (although this is rarely needed).
#' 
#'  
#' \strong{Spatial slicing}
#'  
#'  Spatial subset definition is done via the \code{lonLim} and \code{latLim} arguments, in the same way as
#'   for instance the \code{loadGridData} function, from package \pkg{loadeR}, with the exception that several checks are undertaken
#'   to ensure that the subset is actually within the current extent of the input grid. It is also possible to
#'   make single-point selections from a grid, just by specifying a single coordinate instead of a range
#'    as the argument value. For instance \code{lonLim = c(-10,10)} and \code{latLim = c(35,45)} indicate a
#'  rectangular window centered in the Iberian Peninsula), and single grid-cell values
#'  (for instance \code{lonLim = -3.21} and \code{latLim = 41.087} for retrieving the data in the closest grid
#'  point to the point coordinate -3.21E, 41.087N. In the last two cases, the function
#'  operates by finding the nearest (euclidean distance) grid-points to the coordinates introduced.
#'  
#'  \strong{Extracting grids from multigrids}
#'  
#'  One or several variables from a multigrid object can be extracted. Note that argument \code{var} is 
#'  insensitive to the order of the variables, i.e.: variables will be always returned in the same order
#'   they are in the original multigrid.
#'  
#' @importFrom abind asub
#' @author J. Bedia, M. Iturbide, J. A. Fernandez
#' @export
#' @family subsetting
#' @examples \dontrun{
#' require(climate4R.datasets)
#' # Example 1 - Spatial / member subset
#' data("CFS_Iberia_tas")
#' # Selection of a smaller domain over the Iberian Peninsula and members 3 and 7
#' sub <- subsetGrid(CFS_Iberia_tas,
#'                   members = c(3,7),
#'                   lonLim = c(-10,-5),
#'                   latLim = c(36,43))
#' require(visualizeR)                  
#' spatialPlot(climatology(sub), tol = 0.005, contour = TRUE,
#'                 backdrop.theme = "coastline", rev.colors = TRUE)
#'                 
#' ## Example 2 - Subsetting a multimember multigrid by variables
#' # Multimember multigrid creation
#' data("CFS_Iberia_pr", "CFS_Iberia_hus850")
#' mm.mf <- makeMultiGrid(CFS_Iberia_tas, CFS_Iberia_pr, CFS_Iberia_hus850)
#' # Extracting just minimum temperature
#' sub1 <- subsetGrid(mm.mf, var = "tas", members = 1:4)
#' spatialPlot(climatology(sub1, by.member = TRUE), backdrop.theme = "coastline",
#'      rev.colors = TRUE)
#' # Extracting precipitation and maximum temperature
#' # (Note that the grid variables are NOT re-ordered)
#' sub2 <- subsetGrid(mm.mf, var = c("pr", "tas"))
#' getShape(sub2)
#' getVarNames(sub2)
#' 
#' ## Example 3 - Subsetting stations by their codes
#' data("VALUE_Iberia_pr")
#' getStationID(VALUE_Iberia_pr)
#' central_ib <- subsetGrid(VALUE_Iberia_pr, station.id = c("000229", "000231", "000232"))
#' getStationID(central_iberia)
#' central_iberia$Metadata
#' VALUE_Iberia_pr$Metadata
#' }

subsetGrid <- function(grid,
                       var = NULL,
                       cluster = NULL,
                       runtime = NULL,
                       members = NULL,
                       years = NULL,
                       season = NULL,
                       lonLim = NULL,
                       latLim = NULL,
                       outside = FALSE,
                       station.id = NULL,
                       drop = TRUE) {
    if (!is.null(var)) {
        grid <- subsetVar(grid, var)
    }
    if (!is.null(cluster)) {
      grid <- subsetCluster(grid, cluster)
    }
    if (!is.null(runtime)) {
        grid <- subsetRuntime(grid, runtime)
    }
    if (!is.null(members)) {
        grid <- subsetMembers(grid, members)
    }
    if (!is.null(years)) {
        grid <- subsetYears(grid, years)
    }
    if (!is.null(season)) {
        grid <- subsetSeason(grid, season)
    }
    if (!is.null(lonLim) | !is.null(latLim)) {
        grid <- subsetSpatial(grid, lonLim, latLim, outside)
    }
    if (!is.null(station.id)) {
        grid <- subsetStation(grid, station.id)
    }
    if (isTRUE(drop)) grid <- redim(grid, drop = TRUE)
    return(grid)
}
# End


#' Extract a grid from a multigrid object
#' 
#' Extracts a grid from a multigrid object. Multimember multigrids are supported. Subroutine of subsetGrid.
#'
#' @param multiGrid Input multigrid to be subset 
#' @param var Character vector indicating the variables(s) to be extracted
#' @param drop Logical (default is TRUE). Drop or keep dimensions of length 1.
#' @return Either a (multimember)grid or (multimember)multigrid if one ore more variables
#' are selected respectively.
#' @details Argument \code{var} is insensitive to the order of the variables, i.e.: variables
#' will be always returned in the same order they are in the original multigrid.
#' 
#' An attribute 'subset' with value 'subsetVar' is added to the Variable slot of the output subset.
#' 
#' @importFrom abind asub
#' @keywords internal
#' @export
#' @author J. Bedia 
#' @family subsetting

subsetVar <- function(grid, var) {
    varnames <- getVarNames(grid)
    levelnames <- grid[["Variable"]][["level"]]
    if (length(varnames) == 1) {
        message("NOTE: Variable subsetting was ignored: Input grid is not a multigrid object")
        return(grid)
    } 
    var.idx <- grep(paste0("^", var, "$", collapse = "|"), varnames)
    if (length(var.idx) == 0) {
        stop("Variables indicated for subsetting not found", call. = FALSE)
    }
    if (length(var.idx) < length(var)) {
        stop("Some variables indicated for subsetting not found", call. = FALSE)
    }
    # Recovering attributes
    dimNames <- getDim(grid)
    var.dim <- grep("var", dimNames)
    grid$Data <- asub(grid$Data, idx = var.idx, dims = var.dim, drop = FALSE)   
    attrs <- attributes(grid$Variable)
    attrs.aux <- lapply(attrs, "[", var.idx)
    grid$Variable <- list(varName = varnames[var.idx], level = levelnames[var.idx])
    attrs.aux$names <- NULL
    for(x in 1:length(attrs.aux)) attr(grid[["Variable"]], names(attrs.aux)[x]) <- attrs.aux[[x]]
    names(grid$Variable) <- c("varName", "level")
    grid$Dates <- if (length(var.idx) > 1L) {
        grid$Dates[var.idx]
    } else {
        grid$Dates[[var.idx]]
    }
    attr(grid$Variable, "subset") <- "subsetVar"
    attr(grid$Data, "dimensions") <- dimNames
    return(grid)
}
# End

#' Cluster subsets from a multimember grid
#' 
#' Retrieves a grid that is a logical subset of a multimember grid along its 'time' dimension based on the cluster index.
#' Multimember multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input multimember grid to be subset (possibly a multimember multigrid). A grid resulting from \code{\link{clusterGrid}} 
#' must be used here, otherwise the function will return an error message
#' @param cluster An integer indicating \strong{the cluster} to be subset. For Lamb WTs subsetting, see \code{\link{lambWT}}.
#' @return A grid (or multigrid) that is a logical subset of the input grid along its 'time' dimension based on the cluster index.
#' @keywords internal
#' @export
#' @author J. A. Fernandez 
#' @family subsetting

subsetCluster <- function(grid, cluster) {
  #Check 'grid' is a clustering analyzed grid
  if (is.null(attr(grid, "cluster.type"))) {
    warning("Argument 'cluster' was ignored: There is not any clustering information in 'grid'",
            call. = FALSE)
    return(grid)
  }
  if (!all(cluster %in% attr(grid, "wt.index"))) {
    stop("'cluster' index out of bounds", call. = FALSE)
  }
  indices = which(!is.na(match(attr(grid, "wt.index"), cluster))) 
  grid <- subsetDimension(grid, dimension = "time", indices = indices)
  attr(grid$Variable, "subset") <- "subsetCluster"
  return(grid)
}
# End


#' Member subsets from a multimember grid
#' 
#' Retrieves a grid that is a logical subset of a multimember grid along its 'member' dimension.
#'  Multimember multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input multimember grid to be subset (possibly a multimember multigrid).
#' @param members An integer vector indicating \strong{the position} of the members to be subset.
#' @param drop Logical (default is TRUE). Drop or keep dimensions of length 1.
#' @return A grid (or multigrid) that is a logical subset of the input grid along its 'member' dimension.
#' @details An attribute 'subset' with value 'subsetMembers' is added to the Members slot of the output subset.
#' @importFrom abind asub
#' @keywords internal
#' @export
#' @author J. Bedia 
#' @family subsetting

subsetMembers <- function(grid, members) {
    dimNames <- getDim(grid)
    if (length(grep("member", dimNames)) == 0) {
        warning("Argument 'members' was ignored: Input grid is not a multimember grid object",
                call. = FALSE)
        return(grid)
    }      
    mem.dim <- grep("member", dimNames)
    if (!all(members %in% (1:getShape(grid, "member")))) {
        stop("'members' dimension subscript out of bounds", call. = FALSE)
    }
    grid$Data <- asub(grid$Data, idx = members, dims = mem.dim, drop = FALSE)  
    attr(grid$Data, "dimensions") <- dimNames
    grid$Members <- grid$Members[members]
    if (is.list(grid$InitializationDates)) { # e.g. CFSv2 (members defined through lagged runtimes)
            grid$InitializationDates <- grid$InitializationDates[members]
    } 
    if (!is.null(grid$Runtime)) attr(grid$Members, "subset") <- "subsetMembers"
    return(grid)
}
# End

#' Runtime subsets from a grid
#' 
#' Retrieves a grid that is a logical subset of a multiruntime grid along its 'runtime' dimension.
#'  Multiruntime multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input multiruntime grid to be subset.
#' @param runtime An integer vector indicating \strong{the position} of the runtimes to be subset.
#' @return A grid (or multigrid) that is a logical subset of the input grid along its 'runtime' dimension.
#' @details An attribute 'subset' with value 'subsetRuntime' is added to the Runtime slot of the output subset.
#' @importFrom abind asub
#' @keywords internal
#' @export
#' @author M. Iturbide
#' @family subsetting

subsetRuntime <- function(grid, runtime) {
    dimNames <- attr(grid$Data, "dimensions")
    if (length(grep("runtime", dimNames)) == 0) {
        warning("Argument 'runtime' was ignored: Input grid is not a multiruntime grid object")
        return(grid)
    }      
    run.dim <- grep("runtime", attr(grid$Data, "dimensions"))
    if (!all(runtime %in% (1:dim(grid$Data)[run.dim]))) {
        stop("'runtime' dimension subscript out of bounds")
    }
    grid$Data <- asub(grid$Data, idx = runtime, dims = run.dim, drop = FALSE)                  
    mf <- FALSE
    attr(grid$Data, "dimensions") <- if (length(dim(grid$Data)) == length(dimNames)) {
        mf <- TRUE
        dimNames
    } else {
        dimNames[-run.dim]
    }
    grid$Runtime <- grid$Members[runtime]
    if (is.list(grid$InitializationDates)) { # e.g. CFSv2 (members defined through lagged runtimes)
        grid$InitializationDates <- grid$InitializationDates[runtime]
    } 
    if (!is.null(grid$Runtime)) attr(grid$Runtime, "subset") <- "subsetRuntime"
    return(grid)
}
# End




#' Year subsets from a multimember grid
#' 
#' Retrieves a grid that is a logical subset of a multimember grid along its 'time' dimension,
#'  on a yearly basis. Multimember multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input grid to be subset (possibly a multimember/multigrid).
#' @param years An integer vector indicating the years to be subset.
#' @details An attribute 'subset' with value 'subsetYears' is added to the Dates slot of the output subset.
#' @return A grid (or multigrid) that is a logical subset of the input grid along its 'time' dimension.
#' @importFrom abind asub
#' @keywords internal
#' @export
#' @author J. Bedia 
#' @family subsetting

subsetYears <- function(grid, years) {
    dimNames <- getDim(grid)
    season <- getSeason(grid)
    all.years <- getYearsAsINDEX(grid)
    aux.year.ind <- match(years, unique(all.years))
    if (length(intersect(years, all.years)) == 0) {
        stop("No valid years for subsetting. The argument \'years\' was ignored")
    }
    if (any(years < min(all.years) | years > max(all.years))) {
        stop("Some subset time boundaries outside the current grid extent")
    }
    time.ind <- which(all.years %in% years)
    dims <- grep("^time", dimNames)
    if ((isTRUE(drop)) & ((getShape(grid, "time") == 1L) | length(time.ind) == 1L)) {
        dimNames <- dimNames[-grep("^time", dimNames)]    
    } 
    grid$Data <- asub(grid$Data, time.ind, dims, drop = FALSE)
    attr(grid$Data, "dimensions") <- dimNames
    attr(grid, "wt.index") <- attr(grid, "wt.index")[time.ind]
    # Verification Date adjustment
    grid$Dates <- if (getShape(redim(grid, var = TRUE), dimension = "var") != 1) {
        lapply(1:length(grid$Dates), function(i) {
            lapply(grid$Dates[[i]], function(x) x[time.ind])})
    } else {
        lapply(grid$Dates, FUN = "[", time.ind)
    }
    # Initialization time adjustment
    if ("member" %in% dimNames) {
        grid$InitializationDates <- if (is.list(grid$InitializationDates)) { # Lagged runtime config
            lapply(grid$InitializationDates, "[", aux.year.ind)      
        } else {
            grid$InitializationDates[aux.year.ind]
        }
    }
    attr(grid$Dates, "subset") <- "subsetYears"
    attr(grid$Dates, "season") <- season
    return(grid)
}
# End


#' Spatial subset from a grid
#' 
#' Retrieves a grid that is a logical subset of the input grid along its 'lat' and 'lon' dimensions.
#'  Multimember multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input grid to be subset (possibly a multimember multigrid).
#' @param lonLim Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees,
#'  of the bounding box defining the subset. For single-point subsets, a numeric value with the
#'  longitude coordinate. If \code{NULL} (default), no subsetting is performed on the longitude dimension
#' @param latLim Same as \code{lonLim} argument, but for latitude.
#' @param outside Logical. Default to \code{FALSE}. If \code{TRUE}, subset coordinates outside the grid extent 
#' are allowed. 
#' @details An attribute \code{subset} with value \code{subsetSpatial} is added to the \code{xyCoords}
#' component of the output grid.
#' @return A grid (or multigrid) that is a logical spatial subset of the input grid.
#' @importFrom abind asub
#' @importFrom utils tail
#' @keywords internal
#' @export
#' @author J. Bedia 
#' @family subsetting
#' 
subsetSpatial <- function(grid, lonLim, latLim, outside) {
    dimNames <- getDim(grid)
    londim <- c("loc", "loc")
    if (isRegular(grid)) londim <- c("lon", "lat")
    if (!is.null(lonLim)) {
        if (!is.vector(lonLim) | length(lonLim) > 2) {
            stop("Invalid longitudinal boundary definition")
        }
        if (!isRegular(grid)) {
              grid <- reorderStation(grid, axis = "x")
        }
        lons <- getCoordinates(grid)$x
        if (lonLim[1] < lons[1] | lonLim[1] > tail(lons, 1)) {
            if (outside == FALSE) {
                stop("Subset longitude boundaries outside the current grid extent: \n(",
                     paste(getGrid(grid)$x, collapse = ","), ")")
            } else {
                warning("Subset longitude boundaries outside the current grid extent: \n(",
                        paste(getGrid(grid)$x, collapse = ","), ")")
            }
        }
        lon.ind <- which.min(abs(lons - lonLim[1]))
        if (!isRegular(grid)) lon.ind <- min(which(lons >= lonLim[1]))
        if (length(lonLim) > 1) {
            if (lonLim[2] < lons[1] | lonLim[2] > tail(lons, 1)) {
                if (outside == FALSE) {
                    stop("Subset longitude boundaries outside the current grid extent: \n(",
                         paste(getGrid(grid)$x, collapse = ","), ")")
                } else {
                    warning("Subset longitude boundaries outside the current grid extent: \n(",
                            paste(getGrid(grid)$x, collapse = ","), ")")
                }
            }
            lon2 <- which.min(abs(lons - lonLim[2]))
            if (!isRegular(grid)) lon2 <- max(which(lons <= lonLim[2]))            
            lon.ind <- lon.ind:lon2
        }
        grid$Data <- asub(grid$Data, idx = lon.ind, dims = grep(londim[1], dimNames), drop = FALSE)
        attr(grid$Data, "dimensions") <- dimNames
        if (isRegular(grid)) {
              grid$xyCoords$x <- grid$xyCoords$x[lon.ind] 
        } else {
              grid$xyCoords <- grid$xyCoords[lon.ind,]
              if ("Metadata" %in% names(grid)) {
                grid$Metadata %<>% lapply(FUN = "[", lon.ind)
              }
        }
    }
    if (!is.null(latLim)) {
        if (!is.vector(latLim) | length(latLim) > 2) {
            stop("Invalid latitudinal boundary definition")
        }
          if (!isRegular(grid)) {
               grid <- reorderStation(grid, axis = "y")
          }
        lats <- getCoordinates(grid)$y
        if (latLim[1] < lats[1] | latLim[1] > tail(lats, 1)) {
            if (outside == FALSE) {
                stop("Subset latitude boundaries outside the current grid extent: \n(",
                     paste(getGrid(grid)$y, collapse = ","), ")")
            } else {
                warning("Subset longitude boundaries outside the current grid extent: \n(",
                        paste(getGrid(grid)$x, collapse = ","), ")")
            }
        }
        lat.ind <- which.min(abs(lats - latLim[1]))
        if (!isRegular(grid)) lat.ind <- min(which(lats >= latLim[1]))            
        if (length(latLim) > 1) {
            if (latLim[2] < lats[1] | latLim[2] > tail(lats, 1)) {
                if (outside == FALSE) {
                    stop("Subset latitude boundaries outside the current grid extent: \n(",
                         paste(getGrid(grid)$y, collapse = ","), ")")
                } else {
                    warning("Subset longitude boundaries outside the current grid extent: \n(",
                            paste(getGrid(grid)$x, collapse = ","), ")")
                }
            }
            lat2 <- which.min(abs(lats - latLim[2]))
            if (!isRegular(grid)) lat2 <- max(which(lats <= latLim[2]))            
            lat.ind <- lat.ind:lat2
        }
        grid$Data <- asub(grid$Data, lat.ind, grep(londim[2], dimNames), drop = FALSE)
        attr(grid$Data, "dimensions") <- dimNames
        if (isRegular(grid)) {
              grid$xyCoords$y <- grid$xyCoords$y[lat.ind] 
        } else {
              grid$xyCoords <- grid$xyCoords[lat.ind,]
              if ("Metadata" %in% names(grid)) {
                grid$Metadata %<>% lapply(FUN = "[", lat.ind)
              }
        }
    }
    attr(grid$xyCoords, "subset") <- "subsetSpatial"
    return(grid)
}
# End


#' Monthly subset of a grid
#' 
#' Retrieves a grid that is a logical subset of the input grid along its 'time' dimension,
#'  on a monthly basis. Multimember multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input grid to be subset (possibly a multimember/multigrid).
#' @param season An integer vector indicating the months to be subset.
#' @details An attribute 'subset' with value 'time' is added to the Variable slot of the output grid.
#' @return A grid (or multigrid) that is a logical subset of the input grid along its 'time' dimension.
#' @importFrom magrittr %>% %<>% 
#' @keywords internal
#' @export
#' @author J. Bedia 
#' @family subsetting

subsetSeason <- function(grid, season) {
  season0 <- getSeason(grid)
  if ((min(season) < 1 | max(season) > 12)) stop("Invalid season definition", call. = FALSE)
  if (!all(season %in% season0)) stop("Month selection outside original season values") 
  if (!identical(season0, season)) {
    if (getTimeResolution(grid) != "YY") {
      mon <- getRefDates(grid) %>% substr(6,7) %>% as.integer()
      time.ind <- which(mon %in% season)
      grid %<>% subsetDimension(dimension = "time", indices = time.ind)
      if (!identical(season, sort(season))) {
        mon <- getRefDates(grid) %>% substr(6,7) %>% as.integer()
        yr <- getRefDates(grid) %>% substr(1,4) %>% as.integer()
        # Lost months from first year
        rm.ind.head <- (which(diff(season) != 1L) + 1):length(season)
        rm1 <- which(yr == head(yr, 1) & (mon %in% season[rm.ind.head]))
        if (length(rm1) == 0L) rm1 <- NA
        # Lost months from last year
        rm.ind.tail <- 1:which(diff(season) != 1L)
        rm2 <- which(yr == tail(yr, 1) & (mon %in% season[rm.ind.tail]))
        if (length(rm2) == 0L) rm1 <- NA
        rm.ind <- na.omit(c(rm1, rm2))
        if (length(rm.ind) > 0L) {
          message("NOTE: Some data will be lost on year-crossing season subset (see the \'Time slicing\' section of subsetGrid documentation for more details)")
          time.ind <- (1:getShape(grid, "time"))[-rm.ind]
          grid %<>% subsetDimension(dimension = "time", indices = time.ind)
        }
      }
    } else {
      message("NOTE: Can't perform monthly subsetting on annual data. 'season' argument was ignored.")
    }
  }
  return(grid)
}


#' Monthly subset of a grid
#' 
#' Retrieves a grid that is a logical subset of the input grid along its 'time' dimension,
#'  on a monthly basis. Multimember multigrids are supported. Subroutine of \code{\link{subsetGrid}}.
#'
#' @param grid Input grid to be subset (possibly a multimember/multigrid).
#' @param season An integer vector indicating the months to be subset.
#' @details An attribute 'subset' with value 'time' is added to the Variable slot of the output grid.
#' @return A grid (or multigrid) that is a logical subset of the input grid along its 'time' dimension.
#' @importFrom magrittr %>% %<>% 
#' @keywords internal
#' @export
#' @author J. Bedia 
#' @family subsetting

subsetStation <- function(grid, station.id = NULL) {
      station0 <- grid$Metadata$station_id
      if (!all(station.id %in% station0)) stop("Station ID selection does not exist in the data")      
      id.ind <- sapply(1:length(station.id),FUN = function(z) {which(station0 == station.id[z])})
      grid %<>% subsetDimension(dimension = "loc", indices = id.ind)
      # if ("Metadata" %in% names(grid)) {
      #   grid$Metadata %<>% lapply(FUN = "[", id.ind)
      # }      
      return(grid)
}

#end



#' @title Arbitrary grid subsetting along one of its dimensions
#' @description Create a new grid/multigrid that is a subset of the input grid 
#' along the selected dimension.
#' @param grid The input grid to be subset. This is either a grid, as returned e.g. by \code{loadGridData} from package \pkg{loadeR} or a
#' multigrid, as returned by \code{makeMultiGrid}, or other types of multimember grids
#' (possibly multimember multigrids) as returned e.g. by \code{loadECOMS}, from package \pkg{loadeR.ECOMS}.
#' @param dimension Character vector indicating the dimension along which the positions indicated by the \code{indices} paraneter.
#' Unlike subsetGrid, only one dimension at a time is accepted here
#' @param indices An integer vector indicating \strong{the positions} of the dimension to be extracted.
#' In case of subsetting several dimensions at at time, this corresponds to a list of indices.
#' @return A new grid object that is a logical subset of the input grid along the specified dimension.
#' @details
#' The attribute \code{subset} will be added taking the value of the \code{dimension} parameter.
#' @importFrom abind asub
#' @importFrom magrittr %<>% 
#' @author J. Bedia and S. Herrera
#' @keywords internal
#' @export
#' @family subsetting
#' @examples \donttest{
#' require(climate4R.datasets)
#' # Example - Member subset
#' data("CFS_Iberia_tas")
#' # Selection of members 3 and 7
#' sub <- subsetDimension(CFS_Iberia_tas,
#'                        dimension = "member",
#'                        indices = 1:2)
#' require(visualizeR)
#' spatialPlot(climatology(sub), backdrop.theme = "coastline")
#' }

subsetDimension <- function(grid, dimension = NULL, indices = NULL) {
    if (is.null(indices)) {
        message("NOTE: Argument 'indices' is NULL. Nothing was done.")
    } else {
        dimension <- match.arg(dimension,
                               choices = c("runtime","var","member","time","lat","lon","loc"),
                               several.ok = TRUE)
        dimNames <- getDim(grid)
        dims <- match(dimension, dimNames)
        if (length(dims) == 0) stop("Dimension names passed to 'dimension' not found")
        grid$Data <- asub(grid$Data, idx = indices,
                          dims = dims,
                          drop = FALSE)
        attr(grid$Data, "dimensions") <- dimNames
        if ("time" %in% dimension) {
            attr(grid$Dates, "season") <- NULL
            var.shape <- suppressMessages(getShape(grid, "var"))
            attrs <- attributes(grid$Dates)
            if (is.na(var.shape) || var.shape == 1) {
                grid$Dates %<>% lapply(FUN = "[", indices)
            } else {
                grid$Dates <- lapply(1:var.shape, function(i) {
                    grid$Dates[[i]] %<>% lapply(FUN = "[", indices)
                })
            }
            mostattributes(grid$Dates) <- attrs
            attr(grid$Dates, "season") <- getSeason(grid)
            attr(grid, "wt.index") <- attr(grid, "wt.index")[indices]
        }
        if ("lon" %in% dimension) {
            grid$xyCoords$x <- grid$xyCoords$x[indices]
        }
        if ("lat" %in% dimension) {
            grid$xyCoords$y <- grid$xyCoords$y[indices]
        }
        if ("loc" %in% dimension) {
              grid$xyCoords <- grid$xyCoords[indices,]  
              grid$Metadata %<>% lapply(FUN = "[", indices)
        }
        if ("member" %in% dimension) {
            grid$Members <- grid$Members[indices]
            if (is.list(grid$InitializationDates)) { # e.g. CFSv2 (members defined through lagged runtimes)
                grid$InitializationDates <- grid$InitializationDates[indices]
            } 
        }
        attr(grid$Variable, "subset") <- dimension
    }
    return(grid)
}
# End


#' @title Temporal intersection
#' @description Takes two input grids and crops the overlapping part along time dimension
#' @param obs First grid (typically observations, but not necessarily)
#' @param prd Second grid (typically predictors in downscaling applications, but not necessarily)
#' @param which.return Which subset grid should be returned, \code{obs} or \code{prd}?
#' @return The grid indicated in \code{which.return}, encompassing the overlapping time period with the other one.
#' @importFrom magrittr %<>% %>% 
#' @author J Bedia
#' @family subsetting
#' @seealso \code{\link{checkDim}}, \code{\link{checkSeason}}, \code{\link{getYearsAsINDEX}}, \code{\link{getSeason}}, for other time dimension helpers
#' @export
#' @examples \donttest{
#' require(climate4R.datasets)
#' data("NCEP_Iberia_psl")
#' range(getRefDates(NCEP_Iberia_psl))
#' data("EOBS_Iberia_tas")
#' range(getRefDates(EOBS_Iberia_tas))
#' # Assume NCEP's sea-level pressure is the predictor, 
#' # and EOBS observations are the predictand,
#' # encompassing both datasets different temporal periods:
#' predictor <- subsetGrid(NCEP_Iberia_psl, years = 1987:2001, season = 1)
#' getSeason(predictor) # January
#' range(getYearsAsINDEX(predictor)) # period 1987-2001 
#' predictand <- EOBS_Iberia_tas
#' getSeason(predictand) # December-January-February (winter)
#' range(getYearsAsINDEX(predictand)) # period 1983-2002
#' # We often want to ensure that their time dimension matches perfectly before downscaling:
#' try(checkDim(predictor, predictand, dimensions = "time"))
#' # getTemporalIntersection is the solution:
#' predictand.adj <- getTemporalIntersection(obs = predictand, prd = predictor, which.return = "obs")
#' getSeason(predictand.adj) # January 
#' range(getYearsAsINDEX(predictand.adj)) # 1987-2001 
#' # In the same vein, it is often required to do the same on the predictor 
#' predictor.adj <- getTemporalIntersection(obs = predictand, prd = predictor, which.return = "prd")
#' checkDim(predictor.adj, predictand.adj, dimensions = "time") # perfect
#' }

getTemporalIntersection <- function(obs, prd, which.return = c("obs", "prd")) {
    which.return <- match.arg(which.return, choices = c("obs", "prd"))
    obs.dates <- getRefDates(obs) %>% as.Date(tz = "GMT", format = "%Y-%m-%d")
    prd.dates <- getRefDates(prd) %>% as.Date(tz = "GMT", format = "%Y-%m-%d")
    auxDates <- intersect(obs.dates, prd.dates) %>% as.Date(origin = "1970-01-01", tz = "GMT", format = "%Y-%m-%d")
    if (which.return == "obs") {
        out <- obs
        ind <- which(is.element(obs.dates, auxDates))
        seas <- getSeason(prd)
        attr(out$Dates, "season") <- NULL
        prd <- auxDates <- NULL
    } else {
        out <- prd
        ind <- which(is.element(prd.dates, auxDates))
        seas <- getSeason(obs)
        attr(out$Dates, "season") <- NULL
        obs <- auxDates <- NULL
    }
    out %<>% subsetDimension(dimension = "time", indices = ind)
    attr(out$Variable, "time_subset") <- "getTemporalIntersection"
    return(out)
}





