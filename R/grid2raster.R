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


#' @title Compute a grid climatology
#' @description Calculates the climatology (i.e., complete temporal aggregation, typically the mean)
#' of the input grid.
#' @param grid Input grid or multigrid without time, member or runtime dimensions, e.g. a grid returned by function 
#' \code{\link{climatology}}.
#' @param crs character or object of class CRS. PROJ.4 type description of a Coordinate Reference System (map projection). 
#' If this argument is missing, and the x coordinates are withing -360 .. 360 and the y coordinates are 
#' within -90 .. 90, "+proj=longlat +datum=WGS84" is used. Also see under Details if x is a character (filename).
#' @return A raster for grid objects and a RasterStack for multigrids.
#' @importFrom raster raster flip stack
#' @author M. Iturbide
#' @seealso \code{\link{climatology}}; \code{\link{raster}}
#' @export
#' @examples \dontrun{
#' #' # Maximum July surface temp forecast climatology
#' data("tasmax_forecast")
#' # Aggregate all members before computing the climatology
#' tx_mean.clim <- climatology(tasmax_forecast, by.member = FALSE)
#' plotClimatology(tx_mean.clim)
#' tx_ras <- grid2raster(tx_mean.clim)
#' library(raster)
#' spplot(tx_ras)
#' }


grid2raster <- function(grid, crs = NA){
      grid <- redim(grid, drop = T)
      bbox <- getGrid(grid)
      xmn <- bbox$x[1]
      xmx <- bbox$x[2]
      ymn <- bbox$y[1]
      ymx <- bbox$y[2]
      if ("var" %in% getDim(grid)) {
            vars <- grid$Variable$varName
            rr <- list()
            for (i in 1:length(vars)) {
                  s.grid <- subsetGrid(grid, var = vars[i]) 
                  if (length(dim(s.grid$Data)) != 2) stop("The multigrid does not have three dimensions. Apply function climatology to the input grid previously.")
                  r <- raster(s.grid$Data, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = crs)
                  r@data@unit <- attr(s.grid$Variable, "units")
                  r@data@names <- vars[i]
                  rr[[i]] <- flip(r, direction = "y")
            }
            names(rr) <- vars
            rr <- stack(rr)
      } else {
            if (length(dim(grid$Data)) != 2) stop("Grid does not have two dimensions. Apply function climatology to the input grid previously.")
            r <- raster(grid$Data, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx, crs = crs)
            r@data@unit <- attr(grid$Variable, "units")
            r@data@names <- grid$Variable$varName
            rr <- flip(r, direction = "y")
      }
      return(rr)
}


#end
