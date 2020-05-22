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
#' @param crs Optional (Default NA). Character or object of class CRS. PROJ.4 type description of a Coordinate Reference System (map projection). 
#' If this argument is missing, and the x coordinates are withing -360 .. 360 and the y coordinates are 
#' within -90 .. 90, "+proj=longlat +datum=WGS84" is used. Also see under Details if x is a character (filename).
#' @return A raster/RasterStack for grid objects and a list of rasters/RasterStacks for multigrids.
#' @details Function grid2mopa generates a raster object for each time unit in the "time" dimension, for instance, if 
#' time dimension length is 12 (e.g. monthly data), a RasterStack of 12 rasters is obtained. Functions in \pkg{transformeR}
#' allow to operate over the grids in order to obtain the desired temporal means (etc.) at the desired temporal resolution. 
#' @importFrom raster raster flip stack
#' @author M. Iturbide
#' @seealso \code{\link[transformeR]{climatology}}; \code{\link[loadeR]{loadGridData}}
#' @export
#' @examples \donttest{
#' require(climate4R.datasets)
#' # A raster stack from a multigrid
#' data("EOBS_Iberia_tas")
#' data("EOBS_Iberia_pr")
#' multigrid <- makeMultiGrid(EOBS_Iberia_tas, EOBS_Iberia_pr)
#' multigridaggr <- aggregateGrid(multigrid, aggr.y = list(FUN = "mean"))
#' ras <- grid2mopa(multigridaggr)
#' require(sp)
#' spplot(ras$rr)
#' }


grid2mopa <- function(grid, crs = NA){
  grid <- redim(grid, drop = TRUE)
  grid <- redim(grid, member = FALSE)
  if("runtime" %in% getDim(grid) | "member" %in% getDim(grid)){
        stop("runtime and member dimensions are not allowed at the moment. 
             Use function subsetGrid in advance to apply the function")
  }
  if("var" %in% getDim(grid)){
    rr <- list()
    for(i in 1:getShape(grid)["var"]){
      grid1 <- subsetGrid(grid, var = grid$Variable$varName[i])
      rr[[i]] <- grid2mopa0(grid = grid1, 
                           varname = grid$Variable$varName[i], crs = crs)
    }
     names(rr) <- grid$Variable$varName
  }else{
    rr <- grid2mopa0(grid = grid, 
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
#' require(visualizeR)
#' spatialPlot(t.clim)
#' t.ras <- grid2mopa0(t.clim)
#' # require(sp)
#' # spplot(t.ras)

grid2mopa0 <- function(grid, 
                       varname = "variable", crs = NA){
  # if(!any(names(attributes(grid$Data)) == "climatology:fun")){
  #   grid <- climatology(grid, clim.fun = clim.fun)
  # }
  grid <- redim(grid, drop = T)
  grid <- redim(grid, runtime = FALSE, member = FALSE)
  bbox <- getGrid(grid)
  xmn <- bbox$x[1]
  xmx <- bbox$x[2]
  ymn <- bbox$y[1]
  ymx <- bbox$y[2]
  if (length(dim(grid$Data)) > 3) stop("Grid with extra dimensions (member or runtime).
                                         Apply function aggregateGrid to reduce dimensionality.")
  rr <- list()
  for(i in 1:getShape(grid)["time"]){
    r <- raster(grid$Data[i,,], xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = crs)
    r@data@unit <- attr(grid$Variable, "units")
    r@data@names <- varname
    rr[[i]] <- flip(r, direction = "y")
  }
  if(length(rr) > 1){
    rr <- stack(rr)
  } else{
    rr <- rr[[1]]
  }
  return(rr)
}


#end


