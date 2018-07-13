#     projectGrid.R Grid datum definition and transformation
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


#' @title grid datum definition and transformation
#' @description Defines and/or transforms the projection of a grid (or station data) by means of a \code{\link[sp]{CRS}} object. 
#' @param grid a grid or multigrid or station data.
#' @param original.CRS character as passed to function \code{\link{CRS}}. It defines the original projection. Ignored if already exists in the data,
#' in which case, a warning is returned with the original datum.
#' @param new.CRS character as passed to function \code{\link{CRS}} for project.
#' @details This function uses \code{\link{spTransform}},  \code{\link{CRS}} and \code{\link{proj4string}} from package \pkg{sp}
#' @seealso \code{\link{spTransform}}, \code{\link{proj4string}}.
#' 
#' @author M. Iturbide
#' @export
#' @importFrom sp proj4string spTransform CRS
#' @importFrom abind abind
#' @examples
#' data("VALUE_Iberia_pr")
#' plot(getCoordinates(VALUE_Iberia_pr))
#' grid <- projectGrid(VALUE_Iberia_pr,
#'                     original.CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
#'                     new.CRS = "+init=epsg:28992")
#' plot(getCoordinates(grid))
#' 
#' data("EOBS_Iberia_pr")
#' plot(get2DmatCoordinates(EOBS_Iberia_pr))
#' grid <- projectGrid(EOBS_Iberia_pr,
#'                     original.CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
#'                     new.CRS = "+init=epsg:28992")
#' plot(get2DmatCoordinates(grid))
#' require(visualizeR)
#' spatialPlot(climatology(grid))




projectGrid <- function(grid,
                        original.CRS = "",
                        new.CRS = "") {
      original.CRS <- tryCatch({CRS(original.CRS)}, error = function(err) {stop("Non-valid original.CRS argument")})
      new.CRS <- tryCatch({CRS(new.CRS)}, error = function(err) {stop("Non-valid new.CRS argument")})
      orig.datum <- attr(grid$xyCoords, "projection")
      # if (orig.datum == "RotatedPole") stop("This function is not applicable to this projection. See Details")
      if (!is.null(orig.datum) & !is.na(original.CRS)) {
            warning("CAUTION! Grid with previusly defined projection: ", orig.datum)
      } else if (is.null(orig.datum) & is.na(original.CRS)) {
            stop("Please define original.CRS")
      } else if (!is.null(orig.datum) & is.na(original.CRS)) {
            original.CRS <- orig.datum
            if (class(original.CRS) == "character") 
                  original.CRS <- tryCatch({CRS(original.CRS)}, error = function(err) {stop("Grid with non-valid defined projection. Please, use argument original.CRS to redefine it correctly")})
      } 
      data <- get2DmatCoordinates(grid)
      sppoints <- SpatialPoints(data)
      sp::proj4string(sppoints) <- original.CRS
      message("[",Sys.time(), "] ", "Arguments of the original projection defined as ", original.CRS)
      if (!is.na(new.CRS)) {
            message("[",Sys.time(), "] ", "Projecting..")
            sppoints.new <- spTransform(sppoints, new.CRS)
            new.coords <- coordinates(sppoints.new)
            x <- unique(new.coords[,1])
            y <- unique(new.coords[,2])
            if (length(x) > 1 & length(y) > 1) {
                  xdists <- lapply(1:(length(x) - 1), function(l) {
                        x[l + 1] - x[l]
                  })
                  ydists <- lapply(1:(length(y) - 1), function(l) {
                        y[l + 1] - y[l]
                  })
                  xa <- sum(unlist(xdists) - unlist(xdists)[1])
                  ya <- sum(unlist(ydists) - unlist(ydists)[1])
                  cond <- any(abs(c(xa, ya)) > 1e-05)
            } else {
                  cond <- TRUE
            }
            if (cond) {
                  if (isRegular(grid)) {
                        grid <- redim(grid, member = TRUE, runtime = TRUE)
                        data.aux1 <- lapply(1:getShape(grid)["runtime"], function(r) {
                              data.aux0 <- lapply(1:getShape(grid)["member"], function(m) {
                                    array3Dto2Dmat(subsetGrid(grid, runtime = r, members = 1)$Data)
                              })
                              do.call("abind", list(data.aux0, along = 0))
                        })
                        grid$Data <-  do.call("abind", list(data.aux1, along = 0))
                        attr(grid$Data, "dimensions") <- c("runtime", "member", "time", "loc")
                        grid <- redim(redim(grid, drop = T), member = FALSE, loc = TRUE)
                  }
                  grid$xyCoords <- as.data.frame(new.coords)
                  attr(grid$xyCoords, "projection") <- new.CRS
                  attr(grid$xyCoords, "resX") <- 0
                  attr(grid$xyCoords, "resY") <- 0
            } else {
                  grid$xyCoords <- list("x" = unique(new.coords[,1]), "y" = unique(new.coords[,2]))
                  attr(grid$xyCoords, "projection") <- new.CRS
                  attr(grid$xyCoords, "resX") <- xdists[[1]]
                  attr(grid$xyCoords, "resY") <- ydists[[1]]
            }
            message("[",Sys.time(), "] ", "Done.")
      }
      return(grid)
}
                        
#end
