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
#' @param index.code Circulation index to be extracted. Only ONE circulation index can be extracted at a time. See \code{circIndexShow()} for details.
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
  
  ind.index <- which(names(pattern) %in% index.code)
  if(length(ind.index)!=1){
    stop("Only ONE circulation index can be extracted at a time")
  }

  cpc.index <- c("NAO", "EA", "WP", "EP/NP", "PNA", "EA/WR", "SCA", "TNH", "POL", "PT")
  enso.index <- c("NINO3.4", "ONI")
  wt.index <- c("kmeans", "som", "hierarchical", "lamb")
  
  #choices <- c(cpc.index, enso.index, wt.index)
  
  if (index.code %in% wt.index){
    res <- list()
    res$Variable <- pattern[[ind.index]]$Variable
    res$Data <- pattern[[ind.index]][[members]][[1]]$pattern 
    attr(res$Data, "dimensions") <- c("time","lat","lon")
    res$xyCoords <- coords
    res$Dates <- dates
    attr(res, "cluster.type") <- index.code
    attr(res, "centers") <- attr(pattern[[ind.index]][[members]][[1]], "centers")
    if (index.code == "kmeans") {
      attr(res, "withinss") <- attr(pattern[[ind.index]][[members]][[1]], "withinss")
      attr(res, "betweenss") <-  attr(pattern[[ind.index]][[members]][[1]], "betweenss")
    } else if (index.code == "hierarchical") {
      attr(res, "height") <- attr(pattern[[ind.index]][[members]][[1]], "height")
      attr(res, "cutree.at.height") <- attr(pattern[[ind.index]][[members]][[1]], "cutree.at.height")
      attr(res, "diff.height.threshold") <- attr(pattern[[ind.index]][[members]][[1]], "diff.height.threshold")
    } 
    attr(res, "description") <- "Weather types index obtained with circIndexGrid"
    grid <- suppressMessages(cluster2plot(res))
    
  }else {
    ind.seas <- which(names(pattern[[ind.index]][[members]]) %in% paste0("Month_",season)) #get season form list 'pattern'
    grid <- list()
    grid$Variable <- list(varName=index.code, level=NULL)
    attr(grid$Variable, "description") <- "Teleconnection index obtained with circIndexGrid"
    attr(grid$Variable, "longname") <- index.code
    grid$Data <- pattern[[ind.index]][[members]][[ind.seas]]$pattern 
    attr(grid$Data, "dimensions") <- c("lat","lon")
    grid$xyCoords <- coords
    grid$Dates <- dates
    attr(grid, "description") <- "Teleconnection index obtained with circIndexGrid"
  }
  
  return(grid)
}

