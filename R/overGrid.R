#' @title Spatial overlay for grids and Spatial* objects
#' @description Application of function \code{over} from \pkg{sp} to grids or
#' station data.
#' @param grid Input grid or station data. 
#' @param layer SpatialPolygons object from which the geometries or attributes are 
#' queried (type \code{help(package = "sp")}.
#' @param subset Logical (default is FALSE). If TRUE, spatial subsetting is performed over
#' the imput grid using the bounding coordinates of argument 'layer'.
#' 
#' @details All grid locations outside layer are filled with NAs. 
#' 
#' @return A grid or station data.
#' 
#' @importFrom sp over bbox
#' 
#' @return A grid
#' @author M. Iturbide
#' @family subsetting
#' @export


overGrid <- function(grid, layer, subset = FALSE) {
      # commented lines are for posible runtime dimension consideration,
      # in which case function bindGrid.runtime should be created
      # grid <- redim(grid, runtime = TRUE)
      loc <- "loc" %in% getDim(grid)
      grid <- redim(grid, loc = loc)
      # n.run <- getShape(grid)["runtime"]
      n.mem <- getShape(grid)["member"]
      if (loc) {
            coords <- grid$xyCoords[, 2:1]
      } else {
            coords <- expand.grid(getCoordinates(grid)$y, getCoordinates(grid)$x)
      }
      # grr <- lapply(1:n.run, function(k){
            # grid.r <- subsetGrid(grid, runtime = k)
            grm <- lapply(1:n.mem, function(x) {
                  grl <- subsetGrid(grid, members = x)
                  dimNames.sub <- getDim(grl)
                  if (loc) {
                        dat <- grl$Data
                  } else {
                        dat <- array3Dto2Dmat(grl$Data)
                  }
                  a <- sp::SpatialPointsDataFrame(cbind(coords[,2], coords[,1]), data.frame(t(dat)))
                  a[which(is.na(over(a, layer))),] <- NA 
                  if (loc) {
                        grl$Data <- unname(as.matrix(t(a@data)))
                  } else {
                        grl$Data <- mat2Dto3Darray(t(a@data), getCoordinates(grl)$x, getCoordinates(grl)$y)
                  }
                  attr(grl$Data, "dimensions") <- dimNames.sub
                  if (subset) {
                        grl <- subsetGrid(grl, lonLim = bbox(layer)[1,], 
                                    latLim = bbox(layer)[2,], outside = TRUE)
                  }
                  grl
            })
            if (n.mem > 1) {
                  newgrid <- do.call("bindGrid.member", grm)
            } else {
                  newgrid <- grm[[1]]
            }
      # })
        # newgrid <-  do.call("bindGrid.runtime", grr)
      return(newgrid)
}




