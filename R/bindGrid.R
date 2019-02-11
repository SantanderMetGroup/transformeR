#     bindGrid.R Grid binding along user-defined dimension
#
#     Copyright (C) 2018 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Grid binding by the specified dimension
#' @description Flexible binding of grids by the specified dimension. The grids must be consistent 
#' regarding the dimensions that are no subject of binding. 
#' Useful to handle sets of predictors as a single block.
#' @param ... Input grids to bind by their member dimension. These must be compatible in time and space (see details).
#' For flexibility, they can be introduced as a list or directly as consecutive arguments.
#' @param dimension Dimension along which the binding is performed. Choices are \code{"member"} (the default),
#' \code{"time"}, \code{"lat"}, \code{"lon"} or \code{"loc"} (for point data) .
#' @param spatial.tolerance Numeric of length one. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @param dataset.attr Character string used to update the \code{"dataset"} attribute of the output grid. This
#' is used only when binding grids along the \code{"member"} dimension, in case multi-model combination is being undertaken,
#' (i.e., different models are being joined as members of a single multimodel dataset)
#' so the new dataset attribute reflects this. Note that the default behaviour is retaining the \code{"dataset"} 
#' attribute of the first grid given, resulting in a misleading metadata information if this is the case. Default to \code{NULL},
#' and ignored.
#' @details
#' 
#' \strong{Application}
#' 
#' One usual application of this function is the construction of multimodel ensembles, by binding different grids from
#'  different models along their \code{"member"} dimension. The function can be also useful for handling loading large domains and multimembers, that are difficult or impossible to load
#'  at once with the loading functions. The task of loading can be efficiently parallelized by splitting the request by
#'  ensemble member subsets, and then binded with this function. 
#' 
#' \strong{Input grids consistency checks}
#' 
#' The function makes a number of checks in order to test the spatiotemporal compatibility of the input multi-member grids.
#'  The spatial consistency of the input grids is also checked. In order to avoid possible errors from the user, the spatial
#'   consistency (i.e., equal XY coordinates) of the input grids must be ensured before attempting the creation of the multigrid,
#'   otherwise giving an error. This can be achieved either through the specification of the same 'lonLim' and 'latLim' argument
#'   values when loading the grids, or using the \code{\link{interpGrid}} interpolator in conjuntion with the \code{\link{getGrid}}
#'   method.
#'   
#' \strong{Binding along the time dimension}  
#' 
#' The \code{"time"} dimension will be always returned in ascending order, regardless of the ordering of the input grids.
#' The internal helper \code{\link{sortDim.time}} is applied to this aim.
#'
#' @examples
#' ## Binding along the member dimension:
#' data("CFS_Iberia_tas")
#' # We first diaggregate in various grids with different members
#' members1_2 <- subsetGrid(CFS_Iberia_tas, members = 1:2)
#' members3_4 <- subsetGrid(CFS_Iberia_tas, members = 3:4)
#' member7 <- subsetGrid(CFS_Iberia_tas, members = 7)
#' member8 <- subsetGrid(CFS_Iberia_tas, members = 8)
#' # The function is insensitive to the number of members per input grid
#' bindedGrid <- bindGrid(members1_2, members3_4, member7, member8, dimension = "member")
#' getShape(bindedGrid, "member")
#' ## Also works for point data
#' data("VALUE_Iberia_tas")
#' CFS_points <- interpGrid(CFS_Iberia_tas, getGrid(VALUE_Iberia_tas))
#' getShape(CFS_points)
#' mem1 <- subsetGrid(CFS_points, members = 1)
#' mem5 <- subsetGrid(CFS_points, members = 5)
#' CFS_points_2mem <- bindGrid(mem1, mem5, dimension = "member")
#' getShape(CFS_points_2mem)
#' \dontrun{
#' require(visualizeR)
#' spatialPlot(climatology(bindedGrid), backdrop.theme = "coastline", rev.colors = TRUE)
#' }
#' ## Binding along time:
#' data("EOBS_Iberia_tas")
#' eobs.1998 <- subsetGrid(EOBS_Iberia_tas, years = 1998)
#' eobs.1999_2000 <- subsetGrid(EOBS_Iberia_tas, years = 1999:2000)
#' eobs1 <- bindGrid(eobs.1998, eobs.1999_2000, dimension = "time")
#' eobs2 <- bindGrid(eobs.1999_2000, eobs.1998, dimension = "time")
#' # Note that the output is always adequately ordered along time:
#' identical(eobs1, eobs2)
#' 
#' # For convenience while programming, it also accepts a list as input:
#' year.list <- list(1998, 1999:2000)
#' grid.list <- lapply(year.list, function(x) {
#'     subsetGrid(EOBS_Iberia_tas, years = x)
#'     # perform any other tasks with the subsets...
#' }) 
#' eobs3 <- do.call("bindGrid", c(grid.list, dimension = "time"))
#' identical(eobs1, eobs3)
#' @importFrom abind abind
#' @seealso \code{\link{subsetGrid}}, for the reverse operation. \code{\link{makeMultiGrid}} is somehow related, although 
#' envisaged to bind grids of different variables along a new dimension \code{"var"} (variable), mainly for the construction of predictor sets in
#' downscaling applications. \code{\link{intersectGrid}}, for temporal or spatial intersection of two or more grids.
#' @author J Bedia and M Iturbide
#' @export

bindGrid <- function(..., dimension = c("member", "time", "lat", "lon", "loc"), 
                     spatial.tolerance = 1e-3,
                     dataset.attr = NULL) {
      dimension <- match.arg(dimension, choices = c("member", "time", "lat", "lon", "loc"))
      if (dimension == "member") {
            bindGrid.member(..., tol = spatial.tolerance, attr. = dataset.attr)
      } else if (dimension == "time") {
            bindGrid.time(..., tol = spatial.tolerance)
      } else if (dimension == "lat" | dimension == "lon" | dimension == "loc") {
            bindGrid.spatial(..., dimn = dimension, tol = spatial.tolerance)
      } 
}
#end

#' @title Grid binding by member dimension
#' @description Flexible binding of (spatiotemporally consistent) grids by their member dimension
#'  useful to handle sets of predictors as a single block.
#' @param ... Input grids to bind by their member dimension. These must be compatible in time and space (see details).
#' For flexibility, they can be introduced as a list or directly as consecutive arguments.
#' @param tol numeric. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @param attr. Character string used to update the \code{"dataset"} attribute of the output grid. This
#' is used only when binding grids along the \code{"member"} dimension, in case multi-model combination is being undertaken,
#' (i.e., different models are being joined as members of a single multimodel dataset)
#' so the new dataset attribute reflects this. Note that the default behaviour is retaining the \code{"dataset"} 
#' attribute of the first grid given, resulting in a misleading metadata information if this is the case. Default to \code{NULL},
#' and ignored.
#' @importFrom abind abind
#' @family internal.helpers
#' @author J Bedia

bindGrid.member <- function(..., tol, attr.) {
      grid.list <- list(...)
      if (!is.null(attr.)) {
            stopifnot(is.character(attr.))
            if (length(attr.) != 1L) stop("Invalid \'dataset.attr\' value: The dataset attribute must have length 1")    
      }
      if (length(grid.list) == 1) {
            grid.list <- unlist(grid.list, recursive = FALSE)
            if (isGrid(grid.list)) {
                  message("NOTE: One single grid passed to the function: nothing to bind, so the original grid was returned")
                  return(grid.list)
            }
      }
      if (length(grid.list) < 2) {
            ref <- grid.list[[1]]
            warning("Only one grid passed as input. Nothing was done", call. = FALSE)
      } else {
            loc <- unique(unlist(lapply(grid.list, function(x) "loc" %in% getDim(x))))
            if (length(loc) > 1) stop("grids and stations cannot be combined")
            grid.list <- lapply(grid.list, "redim", loc = loc)
            # Disaggregation in single members
            mem.index <- sapply(grid.list, "getShape", "member")
            aux.list <- list()
            for (h in 1:length(grid.list)) {
                  subgrid <- grid.list[[h]]
                  n.mem <- mem.index[h]
                  for (i in 1:n.mem) {
                        aux.list[[length(aux.list) + 1]] <- redim(subsetGrid(subgrid, members = i, drop = FALSE), loc = loc)
                  }
            }
            grid.list <- aux.list
            aux.list <- NULL
            checkTemporalConsistency(grid.list)
            for (i in 2:length(grid.list)) {
                  # Spatial test
                  if (!isTRUE(all.equal(grid.list[[1]]$xyCoords, grid.list[[i]]$xyCoords, check.attributes = FALSE, tolerance = tol))) {
                        stop("Input data is not spatially consistent")
                  }
                  # # temporal test
                  # if (!identical(as.POSIXlt(grid.list[[1]]$Dates$start)$yday, as.POSIXlt(grid.list[[i]]$Dates$start)$yday) 
                  #     | !identical(as.POSIXlt(grid.list[[1]]$Dates$start)$year, as.POSIXlt(grid.list[[i]]$Dates$start)$year)) {
                  #     stop("Input data is not temporally consistent")
                  # }
                  # data dimensionality test
                  if (!identical(getShape(grid.list[[1]]), getShape(grid.list[[i]]))) {
                        stop("Incompatible data array dimensions")
                  }
                  checkDim(grid.list[[1]], grid.list[[i]], dimensions = c("time", "lat", "lon"))
                  # if (!identical(getDim(grid.list[[1]]), getDim(grid.list[[i]]))) {
                  #     stop("Inconsistent 'dimensions' attribute")
                  # }
            }
            mem.metadata <- !is.null(grid.list[[1]][["Members"]])
            if (mem.metadata) {
                  #Backwards compatibility fix
                  if (!is.list(grid.list[[1]][["InitializationDates"]])) {
                        for (i in 1:length(grid.list)) {
                              grid.list[[i]][["InitializationDates"]] <- list(grid.list[[i]][["InitializationDates"]])
                        }
                  }
            }
            ref <- grid.list[[1]]
            dimNames <- getDim(ref) 
            dim.bind <- grep("member", dimNames)
            data.list <- lapply(grid.list, FUN = "[[", "Data")
            if (mem.metadata) {
                  inits.list <- vapply(grid.list, FUN.VALUE = list(1L), FUN = "[[", "InitializationDates")
                  member.list <- vapply(grid.list, FUN.VALUE = character(1L), FUN = "[[", "Members")
            } else {
                  inits.list <- rep(NA, length(grid.list))
                  member.list <- paste0("Member_", 1:length(grid.list))
            }
            grid.list <- NULL
            ref[["Members"]] <- member.list
            ref[["InitializationDates"]] <- inits.list
            names(ref[["InitializationDates"]]) <- member.list
            ref[["Data"]] <- unname(do.call("abind", c(data.list, along = dim.bind)))
            inits.list <- member.list <- data.list <- NULL
            attr(ref[["Data"]], "dimensions") <- dimNames
            if (!is.null(attr.)) {
                  attr(ref, "dataset") <- attr.
            }
      }
      return(ref)
}
#end


#' @title Grid binding by spatial dimension
#' @description Flexible binding of (spatially/member consistent) grids by their longitude or latitude dimension
#' @param ... Input grids to bind by their time dimension. These must be compatible in members and space (see details).
#' For flexibility, they can be introduced as a list or directly as consecutive arguments.
#' @param dimn latitude ("lat") or longitude ("lon")
#' @param tol numeric. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @importFrom abind abind
#' @family internal.helpers
#' @author M Iturbide


bindGrid.spatial <- function(..., dimn, tol) {
      # dimension <- match.arg(dimension, choices = c("lat", "lon"))
      grid.list <- list(...)
      if (length(grid.list) == 1) {
            grid.list <- unlist(grid.list, recursive = FALSE)
      }
      if (length(grid.list) < 2) {
            stop("The input must be a list of at least two grids")
      }
      dimsort <- "y"
      loc <- FALSE
      coordfun <- c
      if (dimn == "lon") {
            dimsort <- "x"
      } else if (dimn == "loc") {
            dimsort <- c("x", "y")
            loc <- TRUE
            coordfun <- rbind
            station_id <- unlist(unname(lapply(grid.list, function(x) x$Metadata$station_id)))
            station_name <- unlist(unname(lapply(grid.list, function(x) x$Metadata$name)))
      }
      grid.list <- lapply(grid.list, "redim", var = TRUE, loc = loc)
      for (i in 2:length(grid.list)) {
            # Temporal test
            if (!isTRUE(all.equal(grid.list[[1]]$Dates, grid.list[[i]]$Dates, check.attributes = FALSE, tolerance = tol))) {
                  stop("Input data is not temporally consistent")
            }
            # Member
            if (getShape(grid.list[[1]])[match('member', getDim(grid.list[[1]]))] != getShape(grid.list[[i]])[match('member', getDim(grid.list[[i]]))]) {
                  stop("Member dimension is not consistent")
            }
      }
      ref <- grid.list[[1]]
      dimNames <- getDim(ref) 
      dim.bind <- grep(dimn, dimNames)
      data.list <- lapply(grid.list, FUN = "[[", "Data")
      ref[["Data"]] <- unname(do.call("abind", c(data.list, along = dim.bind)))
      data.list <- NULL
      lat <- lapply(grid.list, FUN = function(x) {
            getCoordinates(x)[dimsort]
      })
      lat <- unname(lat)
      grid.list <- NULL
      lats <- do.call(coordfun, lat)
      if (class(lats) == "list") lats <- unlist(lats)
      attr(ref[["Data"]], "dimensions") <- dimNames
      # n.vars <- getShape(ref, "var")
      #if (n.vars > 1) lats <- rep(list(lats), n.vars)
      if (dimn == "loc") {
            ref[["xyCoords"]] <- lats  
            ref[["Metadata"]][["station_id"]] <- station_id
            ref[["Metadata"]][["name"]] <- station_name
      } else {
            ref[["xyCoords"]][[dimsort]] <- lats
      }
      # ref %<>% sortDim.spatial()
      redim(ref, drop = TRUE)
      return(ref)
}
#end

#' @title Grid sorting by latitude or longitude
#' @description Returns a grid with time sorted in ascending order. For internal use mainly, 
#' the function is useful for instance when subsetting along time and binding with \code{\link{bindGrid.time}}, 
#' so the user does not need to worry about the ordering of the input.
#' @param grid Input grid
#' @param dimension longitude ("x") or latitude ("y")
#' @return A grid fo similar characteristics, but with time dimension re-arranged in ascending ordered if needed.
#' @keywords internal
#' @family internal.helpers
#' @importFrom magrittr %<>%
#' @importFrom abind asub 
#' @author M Iturbide

sortDim.spatial <- function(grid, dimension = c("y", "x")) {
      dimension <- match.arg(dimension, choices = c("y", "x"))
      lats <- grid %>% getCoordinates() #%>% as.Date() %>% as.integer()
      lats <- lats[[dimension]]
      ind <- sort.int(lats, index.return = TRUE)$ix
      attrs <- attributes(grid$xyCoords)
      reflats <- getCoordinates(grid)$y
      grid$xyCoords$y <- reflats
      attributes(grid$xyCoords) <- attrs
      dimNames <- getDim(grid)
      lat.ind <- grep("^lat", dimNames)
      grid$Data %<>% asub(idx = ind, dims = lat.ind, drop = FALSE)
      attr(grid$Data, "dimensions") <- dimNames
      return(grid)
}
#end


#' @title Grid binding by time dimension
#' @description Flexible binding of (spatially/member consistent) grids by their time dimension
#' @param ... Input grids to bind by their time dimension. These must be compatible in members and space (see details).
#' For flexibility, they can be introduced as a list or directly as consecutive arguments.
#' @param tol numeric. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @importFrom abind abind
#' @family internal.helpers
#' @author M De Felice, J Bedia

bindGrid.time <- function(..., tol) {
      grid.list <- list(...)
      if (length(grid.list) == 1) {
            grid.list <- unlist(grid.list, recursive = FALSE)
      }
      if (length(grid.list) < 2) {
            stop("The input must be a list of at least two grids")
      }
      grid.list <- lapply(grid.list, "redim", var = TRUE)
      for (i in 2:length(grid.list)) {
            # Spatial test
            if (!isTRUE(all.equal(grid.list[[1]]$xyCoords, grid.list[[i]]$xyCoords, check.attributes = FALSE, tolerance = tol))) {
                  stop("Input data is not spatially consistent")
            }
            # Member
            if (getShape(grid.list[[1]])[match('member', getDim(grid.list[[1]]))] != getShape(grid.list[[i]])[match('member', getDim(grid.list[[i]]))]) {
                  stop("Member dimension is not spatially consistent")
            }
      }
      ref <- grid.list[[1]]
      dimNames <- getDim(ref) 
      dim.bind <- grep("^time", dimNames)
      data.list <- lapply(grid.list, FUN = "[[", "Data")
      ref[["Data"]] <- unname(do.call("abind", c(data.list, along = dim.bind)))
      data.list <- NULL
      start.list <- lapply(grid.list, FUN = function(x) {
            getRefDates(x)
      })
      end.list <- lapply(grid.list, FUN = function(x) {
            getRefDates(x, "end")
      })
      grid.list <- NULL
      refdates <- list(start = do.call(c, start.list),
                       end = do.call(c, end.list))
      attr(ref[["Data"]], "dimensions") <- dimNames
      n.vars <- getShape(ref, "var")
      if (n.vars > 1) refdates <- rep(list(refdates), n.vars)
      ref[["Dates"]] <- refdates
      ref %<>% sortDim.time()
      return(ref)
}
#end

#' @title Grid sorting by time
#' @description Returns a grid with time sorted in ascending order. For internal use mainly, 
#' the function is useful for instance when subsetting along time and binding with \code{\link{bindGrid.time}}, 
#' so the user does not need to worry about the ordering of the input.
#' @param grid Input grid
#' @return A grid of similar characteristics, but with time dimension re-arranged in ascending ordered if needed.
#' @keywords internal
#' @family internal.helpers
#' @importFrom magrittr %<>%
#' @importFrom abind asub 
#' @author J Bedia

sortDim.time <- function(grid) {
      dates <- grid %>% getRefDates() %>% as.Date() %>% as.integer()
      ind <- sort.int(dates, index.return = TRUE)$ix
      attrs <- attributes(grid$Dates)
      refdates <- list("start" = getRefDates(grid, "start"), "end" = getRefDates(grid, "end"))
      refdates %<>% lapply("[", ind)
      n.vars <- getShape(grid, "var")
      if (n.vars > 1) refdates <- rep(list(refdates), n.vars)
      grid$Dates <- refdates
      attributes(grid$Dates) <- attrs
      dimNames <- getDim(grid)
      time.ind <- grep("^time", dimNames)
      grid$Data %<>% asub(idx = ind, dims = time.ind, drop = FALSE)
      attr(grid$Data, "dimensions") <- dimNames
      return(grid)
}



