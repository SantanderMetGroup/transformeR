##     interpGrid.R Grid interpolation
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

#' @title Grid interpolation
#' @description Interpolation of gridded datasets into a user-defined grid using nearest-neighbour or bilinear weights. 
#' @importFrom akima interp
#' @importFrom abind abind
#' @importFrom fields interp.surface.grid interp.surface
#' @importFrom stats na.exclude setNames
#' @param grid An input grid to be interpolated/regridded.
#' @param new.coordinates Definition of the new grid coordinates, in the form of a list with the x and y components, in thir order.
#' If new coordinates correspond to an irregular grid, lengths for x and y must be the same: Each position in x and y correspond to
#' a new location (a pair of coordinates).
#' Each component consists of a vector of length three with components \emph{from}, \emph{to} and \emph{by},
#'  in this order, similar as the arguments passed to the \code{\link[base]{seq}} function, giving the 
#'  westernmost, easternmost and grid cell width in the X axis and, in the same way,
#'  the southernmost, northernmost and grid cell resolution in the Y axis. See details.
#' @param method Method for interpolation. Currently implemented methods are either \code{"bilinear"},
#' for bilinear interpolation, and \code{"nearest"}, for nearest-neighbor interpolation (default).
#' @param bilin.method Algorithm chosen for bilinear interpolation. Two options available: \code{"akima"} uses \code{\link[akima]{interp}} and
#' \code{"fields"} the \code{\link[fields]{interp.surface.grid}} algorithm. In case any missing values exist in the input data matrix, 
#' the \code{"fields"} option, able to handle missing values, need to be used. Otherwise, the \code{"akima"} option performs much faster.
#' @template templateParallelParams 
#' @return An interpolated object preserving the structure of the input
#' @details  In case of default definition of either x, y or both grid coordinates, the default grid
#' is calculated taking the corners of the current grid and assuming x and y resolutions equal to 
#' the default \code{by} argument value in function \code{\link[base]{seq}}: \emph{by = ((to - from)/(length.out - 1))}.
#' The output has special attributes in the \code{xyCoords} element that indicate that the object
#'  has been interpolated. These attributes are \code{interpolation}, which indicates the method used and
#'  \code{resX} and \code{resY}, for the grid-cell resolutions in the X and Y axes respectively.
#'  It is also possible to pass the interpolator the grid of a previously existing grid dataset using the
#'  \code{\link{getGrid}} method.
#' @template templateParallel
#' @param ... Further arguments for bilinear interpolation that are passed to function \code{\link[akima]{interp}} 
#' from package \pkg{\link[akima]{akima}}.
#' @note To avoid unnecessary NA values, the function will not extrapolate using a new grid outside the
#' current extent of the dataset, returning an error message.
#' @author J. Bedia, S. Herrera, M. de Felice, M. Iturbide
#' @export
#' @examples 
#' # boreal winter (DJF) precipitation data for the Iberian Peninsula and the period 1983-2002
#' data(EOBS_Iberia_tp)
#' plotClimatology(climatology(EOBS_Iberia_tp))
#' # Bilinear interpolation to domain centered in Spain using a 0.5 degree resolution 
#' # in both X and Y axes
#' t1 <- interpGrid(EOBS_Iberia_tp, new.coordinates = list(x = seq(-10,5,.5),
#'                                                         y = seq(36,44,.5)),
#'                  method = "bilinear",
#'                  bilin.method = "akima")
#' plotClimatology(climatology(t1), backdrop.theme = "countries")
#' # New attributes indicate that the data have been interpolated:
#' attributes(t1$xyCoords)
#' 
#' # Using the coordinate information of another grid
#' data(NCEP_Iberia_tp)
#' t2 <- interpGrid(EOBS_Iberia_tp, new.coordinates = getGrid(NCEP_Iberia_tp),
#'                  method = "nearest")
#' plotClimatology(climatology(t2), backdrop.theme = "countries")
#' 
#' #From station data to grid
#' data(VALUE_Iberia_tp)
#' plotClimatology(climatology(VALUE_Iberia_tp), backdrop.theme = "countries")
#' t3 <- interpGrid(VALUE_Iberia_tp, new.coordinates = getGrid(EOBS_Iberia_tp),
#'                  method = "bilinear")
#' plotClimatology(climatology(t3), backdrop.theme = "countries")
#' 
#' #From grid to station data
#' t4 <- interpGrid(EOBS_Iberia_tp, new.coordinates = getGrid(VALUE_Iberia_tp),
#'                  method = "nearest")
#' plotClimatology(climatology(t4), backdrop.theme = "countries")
#' t5 <- interpGrid(EOBS_Iberia_tp, 
#'                new.coordinates = list(x = c(-6.7, -4.5, 2.5), 
#'                                       y = c(41.8, 40, 39)))
#' plotClimatology(climatology(t5), backdrop.theme = "countries")
#' 
#' #From grid to a single point or station
#' t6 <- interpGrid(grid = EOBS_Iberia_tp, 
#'                  new.coordinates = list(x = -6.7, y = 41.8))
#' str(t6$Data)


interpGrid <- function(grid,
                       new.coordinates = list(x = NULL, y = NULL),
                       method = c("nearest", "bilinear"),
                       bilin.method = NULL,
                       parallel = FALSE,
                       max.ncores = 16,
                       ncores = NULL,
                       ...) {
      arg.list <- list(...)
      tab <- c("member", "time", "level", "lat", "lon")
      dimNames.ref <- c("member", "time", "lat", "lon")
      mess <- FALSE
      method <- match.arg(method, choices = c("nearest", "bilinear"))
      if (method == "nearest" & !is.null(bilin.method)) message("NOTE: argument 'bilin.method' ignored for nearest neighbour interpolation")
      if (method == "bilinear") bilin.method <- match.arg(bilin.method, choices = c("akima", "fields"))
      parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
      # redim object
      grid <- redim(grid, runtime = FALSE)
      mem.ind <- grep("member", getDim(grid))
      n.members <- getShape(grid, "member")
      time.ind <- grep("^time", getDim(grid))
      n.times <- getShape(grid, "time")
      lon.ind <- grep("^lon", getDim(grid))
      lat.ind <- grep("^lat", getDim(grid))
      coords <- getCoordinates(grid)
      #Old coordinates
      if(is.data.frame(coords)){
            x <- coords[,1]
            y <- coords[,2]
            bilin.method <- "akima"
            message("NOTE: Input data corresponds to an irregular grid, bilin.method = 'akima'")
            mess <- TRUE
      }else if("lon" %in% names(coords)){
            x <- coords$lon
            y <- coords$lat
      }else if(!is.data.frame(coords)){
            x <- list(x = outer(coords$y*0, coords$x, FUN = "+"),
                      y = outer(coords$y, coords$x*0, FUN = "+"))$x
            y <- list(x = outer(coords$y*0, coords$x, FUN = "+"),
                      y = outer(coords$y, coords$x*0, FUN = "+"))$y
      }
      #New coordinates
      if (is.null(new.coordinates)) {
            new.coordinates <- getGrid(grid)
      } else if (!isRegular(new.coordinates)){
            if(mess && method == "bilinear") stop("Both original and new coordinates are irregular: bilinear method is not implemented for this case")
            dimNames.ref <- c("member", "time", "loc")
            tab <- c("member", "time", "level", "loc")
            output.coords <- setNames(data.frame(cbind("x" = new.coordinates$x, "y" = new.coordinates$y)), nm = c("x", "y"))
            new.resX <- 0
            new.resY <- 0
            bilin.method <- "fields"
            if (method == "bilinear") message("NOTE: New coordinates are irregular, bilin.method = 'fields'")
            #
      } else {
            if (is.null(new.coordinates$x)) {
                  new.coordinates$x <- x
            } else if (exists("resX", where = attributes(new.coordinates))) {
                  if (length(new.coordinates$x) != 2 | new.coordinates$x[2] < new.coordinates$x[1]) {
                        stop("Invalid grid definition in X")
                  }
                  if ((max(c(new.coordinates$x[1],new.coordinates$x[2])) < min(x)) | (min(c(new.coordinates$x[1],new.coordinates$x[2])) > max(x))) {
                        stop("The input and output grids do not overlap\nCheck the input and output grid definitions")
                  }
                  if (new.coordinates$x[1] < floor(min(x)) | new.coordinates$x[2] > ceiling(max(x))) {
                        warning("The new longitudes are outside the data extent")
                  }
                  new.coordinates$x <- do.call("seq", as.list(c(new.coordinates$x, attr(new.coordinates, 'resX'))))
            } else {
                  new.coordinates$x <- new.coordinates$x
            }
            # else if (length(new.coordinates$x) == 3) {
            #       if (new.coordinates$x[2] < new.coordinates$x[1]) {
            #             stop("Invalid grid definition in X")
            #       }
            #       if ((max(c(new.coordinates$x[1],new.coordinates$x[2])) < min(x)) | (min(c(new.coordinates$x[1],new.coordinates$x[2])) > max(x))) {
            #             stop("The input and output grids do not overlap\nCheck the input and output grid definitions")
            #       }
            #       if (new.coordinates$x[1] < floor(min(x)) | new.coordinates$x[2] > ceiling(max(x))) {
            #             warning("The new longitudes are outside the data extent")
            #       }
            #       if ((new.coordinates$x[2] > new.coordinates$x[1]) & (abs(new.coordinates$x[3]) < abs(new.coordinates$x[2] - new.coordinates$x[1]))) {
            #             new.coordinates$x <- seq(from = new.coordinates$x[1], to = new.coordinates$x[2], by = new.coordinates$x[3])
            #       }
            # }
            if (is.null(new.coordinates$y)) {
                  new.coordinates$y <- y
            } else if (exists("resY", where = attributes(new.coordinates))) {
                  if (length(new.coordinates$y) != 2 | new.coordinates$y[2] < new.coordinates$y[1]) {
                        stop("Invalid grid definition in Y")
                  }
                  if ((max(c(new.coordinates$y[1],new.coordinates$y[2])) < min(y)) | (min(c(new.coordinates$y[1],new.coordinates$y[2])) > max(y))) {
                        stop("The input and output grids do not overlap\nCheck the input and output grid definitions")
                  }
                  if (new.coordinates$y[1] < floor(min(y)) | new.coordinates$y[2] > ceiling(max(y))) {
                        warning("The new latitudes are outside the data extent")
                  }
                  new.coordinates$y <- do.call("seq", as.list(c(new.coordinates$y, attr(new.coordinates, 'resY'))))
            } else {
                  new.coordinates$y <- new.coordinates$y
            } 
            # else if (length(new.coordinates$y) == 3) {
            #       if (new.coordinates$y[2] < new.coordinates$y[1]) {
            #             stop("Invalid grid definition in Y")
            #       }
            #       if ((max(c(new.coordinates$y[1],new.coordinates$y[2])) < min(y)) | (min(c(new.coordinates$y[1],new.coordinates$y[2])) > max(y))) {
            #             stop("The input and output grids do not overlap\nCheck the input and output grid definitions")
            #       }
            #       if (new.coordinates$y[1] < floor(min(y)) | new.coordinates$y[2] > ceiling(max(y))) {
            #             warning("The new latitudes are outside the data extent")
            #       }
            #       if ((new.coordinates$y[2] > new.coordinates$y[1]) & (abs(new.coordinates$y[3]) < abs(new.coordinates$y[2] - new.coordinates$y[1]))) {
            #             new.coordinates$y <- seq(from = new.coordinates$y[1], to = new.coordinates$y[2], by = new.coordinates$y[3])
            #       }
            # }
            new.resX <- abs(new.coordinates$x[2] - new.coordinates$x[1]) 
            new.resY <- abs(new.coordinates$y[2] - new.coordinates$y[1]) 
            output.coords <- list("x" = new.coordinates$x, "y" = new.coordinates$y)
      }
      # function for lapply 
      apply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "lapply")
      if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
      # nearest indices
      if (method == "nearest") {
            if (parallel.pars$hasparallel) {
                  message("NOTE: parallel option skipped for nearest method")
            }
            message("[", Sys.time(), "] Calculating nearest neighbors...")
            ind.NN.x <- matrix(nrow = length(new.coordinates$x), ncol = length(new.coordinates$y))
            ind.NN.y <- ind.NN.x
            for (k in 1:length(new.coordinates$x)) {
                  if(isRegular(new.coordinates)){
                        for (l in 1:length(new.coordinates$y)) {
                              distK <- sqrt((x - new.coordinates$x[k]) ^ 2 + (y - new.coordinates$y[l]) ^ 2)
                              aux.ind <- which(distK == min(distK), arr.ind = TRUE)
                              if(!is.matrix(coords)){
                                    aux.ind <- matrix(aux.ind, ncol = 2)
                                    ind.NN.x[k,l] <- aux.ind[1,2]
                                    ind.NN.y[k,l] <- aux.ind[1,1] 
                              }else{
                                    ind.NN.x[k,l] <- 1
                                    ind.NN.y[k,l] <- aux.ind
                              }
                              
                        }
                  }else{
                        distK <- sqrt((x - new.coordinates$x[k]) ^ 2 + (y - new.coordinates$y[k]) ^ 2)
                        aux.ind <- which(distK == min(distK), arr.ind = TRUE)
                        if(!is.matrix(coords)){
                              ind.NN.x[k,k] <- aux.ind[2]
                              ind.NN.y[k,k] <- aux.ind[1] 
                        }else{
                              ind.NN.x[k,k] <- 1
                              ind.NN.y[k,k] <- aux.ind
                        }
                  }
            }
      }
      message("[", Sys.time(), "] Performing ", method, " interpolation... may take a while")
      aux.list <- list()
      any_is_NA_or_NAN <- any(!is.finite(grid$Data))
      # if (any_is_NA_or_NAN && bilin.method == "akima") message("The input grid contains missing values\nConsider using 'bilin.method=\"fields\"' instead")
      for (i in 1:n.members) {
            if (n.members > 1) message("[", Sys.time(), "] Interpolating member ", i, " out of ", n.members)
            if (method == "nearest") {
                  int <- array(dim = c(n.times, length(new.coordinates$y), length(new.coordinates$x)))
                  for (k in 1:length(new.coordinates$x)) {
                        for (l in 1:length(new.coordinates$y)) {
                              int[,l,k] <- grid$Data[i,,ind.NN.y[k,l],ind.NN.x[k,l]]
                        }
                  }
                  if(!isRegular(new.coordinates)) int <- array3Dto2Dmat.stations(int)
                  aux.list[[i]] <- int
                  int <- NULL
            }
            if (method == "bilinear") {
                  if(isRegular(new.coordinates)) dimNames.ref <- c("member", "time", "lon", "lat")
                  if(!isRegular(grid) && bilin.method == "fields"){
                        irr.data <- adrop(asub(grid$Data, idx = i, dims = mem.ind, drop = FALSE), drop = lon.ind)
                        z.mem <- mat2Dto3Darray.stations(asub(irr.data, idx = i, dims = mem.ind), x, y)
                        z.mem <- unname(abind(z.mem, along = 0))
                  }else{
                        z.mem <- asub(grid$Data, idx = i, dims = mem.ind, drop = FALSE)
                  }
                  interp.list <- apply_fun(1:n.times, function(j) { # iterates in time (inefficient!, to be changed)
                        z <- asub(z.mem, idx = j, dims = time.ind)
                        if (bilin.method == "akima") {
                              indNoNA <- which(is.finite(z))
                              arg.list$x <- x[indNoNA] ; arg.list$y <- y[indNoNA] ; arg.list$z <- z[indNoNA]
                              arg.list$xo <- new.coordinates$x ; arg.list$yo <- new.coordinates$y
                              arg.list$nx <- length(new.coordinates$x) ; arg.list$ny <- length(new.coordinates$y)
                              int <- do.call("interp", arg.list)$z
                        } else if (bilin.method == "fields") {
                              # if (!any_is_NA_or_NAN & i == 1 & j == 1) message("NOTE: No missing values present in the input grid\nConsider using the option bilin.method=\"akima\" for improved speed")
                              arg.list.fields <- list()
                              arg.list.fields$obj <- list("x" = coords$x, "y" = coords$y, "z" = t(z))
                              if(!isRegular(new.coordinates)){
                                    arg.list.fields$loc <- cbind(new.coordinates$x, new.coordinates$y)
                                    int <- do.call("interp.surface", arg.list.fields)
                              }else{
                                    arg.list.fields$grid.list <- list(x = new.coordinates$x, y = new.coordinates$y)
                                    int <- do.call("interp.surface.grid", arg.list.fields)$z  
                              }
                              
                        }
                        z <- NULL
                        return(int)
                  })
                  aux.list[[i]] <- unname(do.call("abind", c(interp.list, along = -1L)))
                  interp.list <- NULL
            }
      }
      grid$Data <- unname(do.call("abind", c(aux.list, along = -1L)))
      attr(grid$Data, "dimensions") <- dimNames.ref
      aux.list <- NULL
      # Dimension ordering & Coordinate system
      grid$xyCoords <- output.coords
      attr(grid$xyCoords, "resX") <- new.resX
      attr(grid$xyCoords, "resY") <- new.resY
      if (is.null(attr(grid$xyCoords, "projection")) & !is.null(attr(new.coordinates, "projection"))) {
            attr(grid$xyCoords, "projection") <- attr(new.coordinates, "projection")
      }
      attr(grid$xyCoords, "interpolation") <-  method
      gdims <- getDim(grid)
      b <- na.exclude(match(tab, gdims))
      gdims <- gdims[b]
      grid$Data <- aperm(grid$Data, perm = b)
      attr(grid$Data, "dimensions")  <- gdims
      if (is.null(attr(grid$xyCoords, "projection"))) {
            attr(grid$xyCoords, "projection") <- "undefined"
      }
      grid <- redim(grid, drop = TRUE)
      message("[", Sys.time(), "] Done")
      return(grid)
}
# End





