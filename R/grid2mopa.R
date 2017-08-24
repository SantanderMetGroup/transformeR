##     climatology.R Compute a grid climatology
##
##     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
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



#' @title Transform a grid or multigrid to a set of predictors to be used for modeling
#' @description Transform a grid or multigrid to collections of raster files for modeling
#' with the \code{mopa} package.
#' @param grid Input grid or multigrid without member or runtime dimensions. Grids returned by function 
#' \code{\link[transformeR]{climatology}} are also supported.
#' @param clim.fun Function to compute the climatology (i.e. function for temporal aggregation). 
#' This is specified as a list, indicating the name 
#' of the aggregation function in first place (as character), and other optional arguments to be passed 
#' to the aggregation function. Default to mean (i.e., clim.fun = list(FUN="mean",na.rm = TRUE)) and
#' ignored if \code{\link[transformeR]{climatology}} has been previously applied.
#' @param crs Optional (Default NA). Character or object of class CRS. PROJ.4 type description of a Coordinate Reference System (map projection). 
#' If this argument is missing, and the x coordinates are withing -360 .. 360 and the y coordinates are 
#' within -90 .. 90, "+proj=longlat +datum=WGS84" is used. Also see under Details if x is a character (filename).
#' @return A raster for grid objects and a RasterStack for multigrids.
#' @importFrom raster raster flip stack
#' @author M. Iturbide
#' @seealso \code{\link[transformeR]{climatology}}; \code{\link[loadeR]{loadGridData}}
#' @export
#' @examples
#' # A raster stack from a multigrid
#' data("CFS_Iberia_tas")
#' data("CFS_Iberia_tp")
#' multigrid <- makeMultiGrid(CFS_Iberia_tas, CFS_Iberia_tp)
#' multigridaggr <- aggregateGrid(multigrid, aggr.mem = list(FUN = "mean"))
#' ras <- grid2mopa(multigridaggr)
#' require(sp)
#' spplot(ras)



grid2mopa <- function(grid, clim.fun = list(FUN = "mean", na.rm = TRUE), crs = NA){
  if("var" %in% getDim(grid)){
    r <- list()
    for(i in 1:getShape(grid)["var"]){
      grid1 <- subsetGrid(grid, var = grid$Variable$varName[i])
      r[[i]] <- grid2mopa0(grid = grid1, clim.fun = clim.fun, 
                           varname = grid$Variable$varName[i], crs = crs)
    }
    rr <- stack(r)
  }else{
    rr <- grid2mopa0(grid = grid, clim.fun = clim.fun, 
                     varname = grid$Variable$varName, crs = crs)
  }
  return(rr)
}


#end

#' @title Transform a grid or multigrid to a set of predictors to be used for modeling
#' @description Transform a grid or multigrid to collections of raster files for modeling
#' with the \code{mopa} package.
#' @param grid Input grid or multigrid without member or runtime dimensions. Grid returned by function 
#' \code{\link[transformeR]{climatology}} are also supported.
#' @param clim.fun Function to compute the climatology (i.e. function for temporal aggregation). 
#' This is specified as a list, indicating the name 
#' of the aggregation function in first place (as character), and other optional arguments to be passed 
#' to the aggregation function. Default to mean (i.e., clim.fun = list(FUN="mean",na.rm = TRUE)).
#' @param varname Variable name of the grid
#' @param crs Optional (Default NA). Character or object of class CRS. PROJ.4 type description of a Coordinate Reference System (map projection). 
#' If this argument is missing, and the x coordinates are withing -360 .. 360 and the y coordinates are 
#' within -90 .. 90, "+proj=longlat +datum=WGS84" is used. Also see under Details if x is a character (filename).
#' @return A raster for grid objects and a RasterStack for multigrids.
#' @importFrom raster raster flip stack
#' @author M. Iturbide
#' @seealso \code{\link[transformeR]{climatology}}; \code{\link[loadeR]{loadGridData}}
#' @export
#' @examples 
#' #' # Maximum July surface temp forecast climatology
#' data("CFS_Iberia_tas")
#' # Aggregate all members and compute climatology
#' t.clim <- climatology(CFS_Iberia_tas, by.member = FALSE)
#' plotClimatology(t.clim)
#' t.ras <- grid2mopa0(t.clim)
#' require(sp)
#' spplot(t.ras)

grid2mopa0 <- function(grid, clim.fun = list(FUN = "mean", na.rm = TRUE), 
                       varname = "variable", crs = NA){
  if(!any(names(attributes(grid$Data)) == "climatology:fun")){
    grid <- climatology(grid, clim.fun = clim.fun)
  }
  grid <- redim(grid, drop = T)
  bbox <- getGrid(grid)
  xmn <- bbox$x[1]
  xmx <- bbox$x[2]
  ymn <- bbox$y[1]
  ymx <- bbox$y[2]
  if (length(dim(grid$Data)) != 2) stop("Grid with extra dimensions (member or runtime).
                                         Apply function aggregateGrid to reduce dimensionality.")
  r <- raster(grid$Data, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = crs)
  r@data@unit <- attr(grid$Variable, "units")
  r@data@names <- varname
  rr <- flip(r, direction = "y")
  return(rr)
}


#end


