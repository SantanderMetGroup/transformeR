#     pattern2grid.R Transform a climate4R pattern into a climate4R grid.
#
#     Copyright (C) 2019 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Transform a climate4R pattern into a climate4R grid.
#' @description Transform a climate4R pattern into a climate4R grid.
#' @param pattern A climate4R pattern, obtained from \code{circIndexGrid}.
#' @param index.code Circulation index to be extracted. See \code{circIndexShow()} for details.
#' @param members In a model ensemble, position of the desired member. Default: 1 (no member dimension).
#' @param season Season to be extracted. Default: 1.
#' @param coords List of xyCoords, as in C4R grids.
#' @param dates List of (start and end) dates, as in C4R grids.

#' @return A typical C4R grid.
#' 
#' @export
#' @examples \dontrun{ 
#' data(NCEP_hgt500_2001_2010)
#' cpc <- indicesCPC(grid=NCEP_hgt500_2001_2010, index.code = c("NAO", "EA","PNA"), season=1)
#' cpc2plot <- pattern2grid(cpc, index.code="NAO", coords =NCEP_hgt500_2001_2010$xyCoords,
#'                         dates=NCEP_hgt500_2001_2010$Dates)
#' spatialPlot(cpc2plot, backdrop.theme="coastline")
#' }


pattern2grid <- function(pattern, index.code, members=1, season=1,
                       coords=NULL, dates=NULL){
  
  cpc.index <- c("NAO", "EA", "WP", "EP/NP", "PNA", "EA/WR", "SCA", "TNH", "POL", "PT")
  enso.index <- c("NINO3.4", "ONI")
  wt.index <- c("kmeans", "som", "hierarchical", "lamb")
  
  choices <- c(cpc.index, enso.index, wt.index)
  
  ind.index <- which(names(pattern) %in% index.code)
  ind.seas <- which(names(pattern[[ind.index]][[members]]) %in% paste0("Month_",season)) #get season form list 'pattern'
  
  if ((length(ind.index)==1) && (choices > 12)){
    
    res <- list()
    res$Variable <- list(varName=index.code, level=NULL)
    attr(res$Variable, "description") <- "Teleconnection index obtained with circIndexGrid"
    attr(res$Variable, "longname") <- index.code  #should this be change to the name of the variable?
    attr(res, "cluster.type") <- index.code
    res$Data <- pattern[[ind.index]][[members]][[1]]$pattern 
    attr(res$Data, "dimensions") <- c("time","lat","lon")
    attr(res, "centers") <- attr(pattern[[ind.index]][[members]][[1]], "centers")
    res$xyCoords <- coords
    res$Dates <- dates
    grid <- cluster2plot(res)
    
  }else {
    
    grid <- list()
    grid$Variable <- list(varName=index.code, level=NULL)
    attr(grid$Variable, "description") <- "Teleconnection index obtained with circIndexGrid"
    attr(grid$Variable, "longname") <- index.code
    
    grid$Data <- pattern[[ind.index]][[members]][[ind.seas]]$pattern 
    attr(grid$Data, "dimensions") <- c("lat","lon")
    
    grid$xyCoords <- coords
    grid$Dates <- dates
    
  }
  
  return(grid)
}

