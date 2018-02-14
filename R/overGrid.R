#' @title Spatial overlay for grids and Spatial* objects
#' @description Application of function \code{over} from \pkg{sp} over grids.
#' @param grid Input grid. The first element must be a grid, the rest could
#' be either grids or numbers.
#' @param layer Spatial* object from which the geometries or attributes are 
#' queried (type \code{help(package = "sp")}.
#' @param subset Logical (default is FALSE). If TRUE, spatial subsetting is performed over
#' the imput grid using the bounding coordinates of argument 'layer'.
#' 
#' @importFrom sp over
#' 
#' @return A grid
#' @author M. Iturbide
#' @export
#' @examples
#' library(visualizeR)
#' data("PRUDENCEregions")
#' library(loadeR)
#' grid <- loadGridData(
#'       "http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.25regular/tx_0.25deg_reg_v16.0.nc",
#'       var = "tx", years = 1983, season = 12)
#' 
#' iberia <- overGrid(grid, PRUDENCEregions[2, ])
#' iberia2 <- overGrid(grid, PRUDENCEregions[2,], subset = TRUE)
#' allregions <- overGrid(grid, PRUDENCEregions, subset = TRUE)
#' 
#' spatialPlot(climatology(grid),
#'             sp.layout = list(list(PRUDENCEregions, first = F)))
#' spatialPlot(climatology(iberia),
#'             sp.layout = list(list(PRUDENCEregions, first = F)))
#' spatialPlot(climatology(iberia2),
#'             sp.layout = list(list(PRUDENCEregions, first = F)))
#' spatialPlot(climatology(allregions),
#'             sp.layout = list(list(PRUDENCEregions, first = F)))

overGrid <- function(grid, layer, subset = FALSE) {
      redim(grid)
      dimNames <- getDim(grid)
      dat <- array3Dto2Dmat(grid$Data)
      coords <- expand.grid(getCoordinates(grid)$y, getCoordinates(grid)$x)
      a <- sp::SpatialPointsDataFrame(cbind(coords[,2], coords[,1]), data.frame(t(dat)))
      a[which(is.na(over(a, layer))),] <- NA 
      grid$Data <- mat2Dto3Darray(t(a@data), getCoordinates(grid)$x, getCoordinates(grid)$y)
      attr(grid$Data, "dimensions") <- dimNames
      if (subset) {
            grid <- subsetGrid(grid, lonLim = bbox(layer)[1,], latLim = bbox(layer)[2,], outside = T)
      }
      grid <- redim(grid, drop = TRUE)
      grid <- redim(grid, member = FALSE)
      return(grid)
}




