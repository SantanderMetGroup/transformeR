#     clusterGrid.R Cluster analysis of grid data
#
#     Copyright (C) 2020 Santander Meteorology Group (http://www.meteo.unican.es)
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

#'@title Cluster analysis
#'@description Cluster analysis of climate data. Several clustering algorithms are available.  
#'@inheritParams clusterGrid_2D 
#'@param type Clustering algorithm. Possible values are \code{"kmeans"} (default), \code{"hierarchical"}, \code{"som"} and \code{"lamb"}. See Details.
#'@param newdata Optional grid containing the data for prediction. It must contain the same variables as the input \code{grid} taken as reference.
#'@param y Optional predictand grid data. Clustering Analysis of this grid will be performed as a day-by-day correspondence with 
#' the reference grid (\code{grid} or \code{newdata}). Thus time dimension from \code{y} and the reference grid must intersect. For weather typing.
#'@param ... Further specific arguments passed to the different clustering functions. 
#'@seealso \code{\link[stats]{kmeans}}, \code{\link[stats]{hclust}}, \code{\link[kohonen]{som}}.
#'@importFrom fields rdist
#'@return A C4R (multimember/multi) grid object that will contain the data from: 
#'\itemize{
#'\item The input \code{grid}, if neither \code{newdata} nor \code{y} are indicated.  
#'\item The \code{newdata} grid if this is specified, and \code{y = NULL}. 
#'\item The \code{y} grid, if this is specified.
#'}
#'The clustering type (cluster.type), number of clusters (centers), centroids of clusters from 'grid' input (centroids) and the days corresponding to each 
#'cluster (index) are returned as attributes in all clustering algorithms. Then, other algorithm-specific parameters are provided as attributes.
#'@details 
#'\strong{kmeans}
#'
#'While using the K-means algorithm, the number of clusters (argument \code{centers}) needs to be provided (with no default). 
#'See the \code{\link[stats]{kmeans}} documentation for further details and optional arguments of the method.
#'
#'\strong{hierarchical}
#'
#'While using the hierarchical algorithm (check \code{\link[stats]{hclust}} for further information) 
#'\code{clusterGrid} allows the user either to especify a predefined number of clusters (\code{centers}) or not. 
#'If the argument \code{centers} is not provided, they are automatically set and the tree is cut when the height 
#'difference between two consecutive divisions (sorted in ascending order) is larger than the interquartile 
#'range of the heights vector, as determined by \code{\link[stats]{cutree}}. 
#'
#'\strong{som}
#'
#'While using the SOM (self-organizing maps) algorithm (check \code{\link[kohonen]{som}} for further information), the argument \code{centers} is provided as
#' a two-element vector, indicating the dimensions \code{xdim,ydim} of the grid (see \code{\link[kohonen]{somgrid}}).
#'By default, a rectangular topology (8x6) of 48 clusters is obtained. 
#'
#'\strong{Lamb}
#'
#'The Lamb Weather Typing algorithm (Lamb 1972) is implemented in the function \code{\link{lambWT}}. The argument \code{centers} is not used.
#'A default of 26 types is calculated, following Trigo and daCamara (2000) 
#'@author J. A. Fernández
#'@export
#'@references 
#'
#'Lamb, H., 1972. British Isles weather types and a register of the daily sequence of circulation patterns.
#'
#'Trigo, R.M., DaCamara, C.C., 2000. Circulation weather types and their influence on the precipitation regime in Portugal. 
#'Int. J. Climatol. 23. https://doi.org/10.1002/1097-0088(20001115)20:13%3C1559::AID-JOC555%3E3.0.CO;2-5

#'@examples \donttest{
#'require(climate4R.datasets)
#'#Example of K-means clustering: 
#'data(NCEP_Iberia_psl, package = "transformeR")
#'clusters<- clusterGrid(NCEP_Iberia_psl, type="kmeans", centers=10, iter.max=1000)
#'
#'#Example of hierarchical clustering: 
#'clusters<- clusterGrid(NCEP_Iberia_psl, type="hierarchical")
#'
#'#Example of som clustering: 
#'clusters<- clusterGrid(NCEP_Iberia_psl, type="som", centers = c(10,1))
#'
#'#Example of lamb clustering:
#'data(NCEP_slp_2001_2010)
#'clusters <- clusterGrid(grid = NCEP_slp_2001_2010, type = "lamb")
#'}


clusterGrid <- function(grid, 
                        type = "kmeans",
                        centers = NULL,
                        newdata = NULL, 
                        y = NULL,
                        ...) {
 
  type <- match.arg(type, choices = c("kmeans", "hierarchical", "som", "lamb"))
  
  if (is.null(newdata)) {
    #Checking grid dimensions
    if (!is.na(suppressMessages(getShape(grid, "member"))) && getShape(grid, "member") > 1) {
      message("Clustering analysis will be done after Ensemble mean...")
      grid <- suppressMessages(aggregateGrid(grid = grid, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
    }
    n.var <- suppressMessages(getShape(grid, "var")) 
    var.names <- getVarNames(grid)
    
    ### Circulation types
    #Special case: Lamb WT
    if (type == "lamb") {
      grid <- redim(grid, var = TRUE)
      n.var <- getShape(grid, "var")
      if ((n.var) != 1) {
        stop("For lamb, only 'psl' variable is required Use subsetGrid to extract it")
      }
      if (!is.null(centers)) {
        message("Lamb WT was choosen, so the number of clusters will be forced to 26. Arg. 'centers' will be ignored.")
      }
      centers <- 26
      arg.list <- list(...)
      arg.list[["grid"]] <- grid
      lamb.wt <- do.call("lambWT", arg.list)
      #clusters.list <- vector("list", n.mem)
      #for (m in 1:n.mem){
        clusters.list <- array3Dto2Dmat(lamb.wt[[1]][[1]][[1]]$pattern)
      #} 
    } else { #Rest of clustering algorithms
      #Scaling and combining data from all variables:
      if (!(is.na(n.var))) {
        data.combined <- comb.vars(grid = grid, base = NULL, ref = NULL, var.names = var.names)
      }else {
        n.var <- 1
        var <- var.names
        data.combined <- comb.vars(grid = grid, base = NULL, ref = grid, var.names = var.names)
      }
      #Clustering Analysis of 'grid':
      clusters.list <- clusterGrid_2D(grid.2D = data.combined, type, centers, ...)
      centers <- nrow(clusters.list)
    }
    if (type == "lamb") {
      wt.index <- lamb.wt[[1]][[1]][[1]]$index
    } else {
      wt.index <- attr(clusters.list, "index")
    }
    
    ### Weather types
    if (is.null(y)) { 
      out.grid <- grid
    } else {
      checkTemporalConsistency(grid, y)
      out.grid <- y
    }
    attr(out.grid, "cluster.type") <- type
    attr(out.grid, "centers") <- centers
    attr(out.grid, "wt.index") <- wt.index
    attr(out.grid, "centroids") <- clusters.list[ , ]
    if (type == "kmeans") {
        attr(out.grid, "withinss") <- attr(clusters.list, "withinss")
        attr(out.grid, "betweenss") <- attr(clusters.list, "betweenss")
    }else if (type == "hierarchical") {
        attr(out.grid, "height") <- attr(clusters.list, "height")
        attr(out.grid, "cutree.at.height") <- attr(clusters.list, "cutree.at.height")
        attr(out.grid, "diff.height.threshold") <- attr(clusters.list, "diff.height.threshold")
    }
  } else {
    #Clustering Analysis of 'newdata':
    if (is.null(attr(grid, "wt.index"))){
      stop("'grid' is not a clustering object.")
    }
    if (!is.na(suppressMessages(getShape(newdata, "member"))) && getShape(newdata, "member") > 1) {
      message("Clustering analysis will be done after Ensemble mean...")
      newdata <- suppressMessages(aggregateGrid(grid = newdata, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
    }
    #Checking consistency among input grids
    checkVarNames(newdata, grid)
    if (getGridUnits(grid) != getGridUnits(newdata)) {
      stop("Inconsistent variable units among 'grid' and 'newdata'")
    }
    checkDim(newdata, grid, dimensions = c("var", "lat", "lon"))
    checkSeason(grid, newdata)
    if (getTimeResolution(grid) != getTimeResolution(newdata)) {
      stop("Inconsistent time resolution among 'grid' and 'newdata'")
    }
    n.var <- suppressMessages(getShape(grid, "var")) 
    arg.list <- list(...)
    base <- arg.list[["base"]]
    if (!is.null(base)) { 
      if (!is.na(suppressMessages(getShape(base, "member"))) && getShape(base, "member") > 1) {
        base <- suppressMessages(aggregateGrid(grid = base, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
      }
      checkVarNames(newdata, base)
      if (getGridUnits(base) != getGridUnits(newdata)) {
        stop("Inconsistent variable units among 'base' and 'newdata'")
      }
      checkDim(newdata, base, dimensions = c("var", "lat", "lon"))
      checkSeason(base, newdata)
      if (getTimeResolution(base) != getTimeResolution(newdata)) {
        stop("Inconsistent time resolution among 'base' and 'newdata'")
      }
    }
    #Pre-processing in order to do clustering to ref CT's: 
    if (n.var != 1) {
      mat.newdata <- comb.vars(grid = newdata, base = base, ref = NULL, var.names = getVarNames(newdata))
    } else {
      mat.newdata <- comb.vars(grid = newdata, base = base, ref = newdata, var.names = getVarNames(newdata))
    }
    dist.mat <- rdist(mat.newdata, attr(grid, "centroids"))
    wt.index <- apply(dist.mat, MARGIN = 1, FUN = "which.min")
    if (is.null(y)) {
      out.grid <- newdata
    } else {
      checkTemporalConsistency(newdata, y)
      out.grid <- y
    }
    attr(out.grid, "cluster.type") <- attr(grid, "cluster.type")
    attr(out.grid, "centers") <- attr(grid, "centers")
    attr(out.grid, "wt.index") <- wt.index
    attr(out.grid, "centroids") <- attr(grid, "centroids")
    if (type == "kmeans") {
      attr(out.grid, "withinss") <- attr(grid, "withinss")
      attr(out.grid, "betweenss") <- attr(grid, "betweenss")
    }else if (type == "hierarchical") {
      attr(out.grid, "height") <- attr(grid, "height")
      attr(out.grid, "cutree.at.height") <- attr(grid, "cutree.at.height")
      attr(out.grid, "diff.height.threshold") <- attr(grid, "diff.height.threshold")
    }
  }
  
  return(redim(out.grid, drop = TRUE))
}


#' @title Variables combination
#' @description Scaling and combining data from all variables
#' @param grid A grid containing the variables to be scaled and combined 
#' @param base Reference baseline grid to be passed to \code{'scaleGrid'}. See \link{scaleGrid} for further information.
#' @param ref Reference grid to be passed to \code{'scaleGrid'}. See \link{scaleGrid} for further information.
#' @param var.names Variables names
#' @importFrom magrittr %>%  
#' @return A list
#' @keywords internal
#' @author J. A. Fernandez

comb.vars <- function(grid, base, ref, var.names){
  grid.scaled <- suppressMessages(scaleGrid(grid = grid, base = base, ref = ref, type = "standardize"))
  l <- redim(grid.scaled, var = TRUE)
  vardata.list <- lapply(1:length(var.names), function(x) {
    suppressMessages(subsetGrid(l, var = var.names[x])[["Data"]]) %>% array3Dto2Dmat()
  }) 
  data.combined <- do.call("cbind", vardata.list)
  return(data.combined)
}


#'@title Cluster analysis of 2D matrix
#'@description Performs cluster analysis of 3D grids. Several clustering algorithms are available.
#'@param grid A grid (gridded or station dataset), multigrid, multimember grid or multimember multigrid object
#'@param type Clustering algorithm to be used for the cluster analysis. 
#'Possible values are "\strong{kmeans}" (default), "\strong{hierarchical}", "\strong{som}". 
#'The core functions are \link[stats]{kmeans}, \link[stats]{hclust}, \link[kohonen]{som}, respectively. See Details.
#'@param centers Integer value indicating the number of clusters, \strong{k}, or center points. See Details.
#'@param ... Further specific arguments passed to the clustering functions
#'@keywords internal
#'@author J. A. Fernandez
#'@importFrom stats kmeans hclust cutree dist quantile
#'@importFrom kohonen som somgrid
#'@return A matrix object that contains the clusters centroids' created using the specified algorithm.
#'The clustering type, number of clusters, cluster index of each day and other algorithm-specific parameters are 
#'provided as attributes.


clusterGrid_2D <- function(grid.2D, type, centers, ...){
  arg.list <- list(...)
  type = tolower(type)
  if (is.null(type) | type == "kmeans") {
    if (is.null(centers)) {
      stop("in 'centers'.\n In K-means the number of clusters ('centers') needs to be provided")
    }
    arg.list[["centers"]] <- centers
    arg.list[["x"]] <- grid.2D
    kmModel <- do.call("kmeans", arg.list)
    Y <- kmModel$centers
    attr(Y, "dimnames") <- NULL
    attr(Y, "index") <- kmModel$cluster
    attr(Y, "withinss") <- kmModel$withinss
    attr(Y, "betweenss") <- kmModel$betweenss
  } else if (type == "hierarchical") {
    arg.list[["d"]] <- dist(grid.2D)
    hc <- do.call("hclust", arg.list)
    token <- "FALSE"
    if (is.null(centers)) {
      token <- "TRUE"
      # Auto-calculation of the number of clusters: Quartile method applied
      quantile.range <- quantile(hc$height, c(0.25, 0.75), na.rm= TRUE) 
      hc.height.diff <- numeric(length(hc$height) - 1)
      for (i in 1:length(hc$height) - 1) {
        hc.height.diff[i] <- hc$height[i + 1] - hc$height[i]
      }
      index <- which(hc.height.diff > (quantile.range[[2]] - quantile.range[[1]]))
      centers <- length(hc$order) - index[1]
    }
    if(is.na(centers)){
      stop("All the height differences are smaller than the interquartile range, probably due to the fact that the 'time' dimension is too small. We recommend to set the number of clusters manually...")
    }
    memb <- cutree(hc, k = centers) #Found the corresponding cluster for every element
    cent <- NULL
    for (k in 1:centers) {   #Set up the centers of the clusters
      cent <- rbind(cent, colMeans(grid.2D[memb == k, ,drop = FALSE]))
    }
    Y <- cent
    attr(Y, "index") <- memb
    attr(Y, "height") <- hc$height
    attr(Y, "cutree.at.height") <- hc$height[(length(hc$order) - centers) + 1] #the previous heigth divides in "centers" numb. of clusters 
    if (token){
      attr(Y, "diff.height.threshold") <- (quantile.range[[2]] - quantile.range[[1]])
    }
  } else if (type == "som") {
    arg.list[["X"]] <- grid.2D
    if (is.null(centers)) {
      centers <- 6*8
      som.grid <- do.call("som", arg.list)
    } else {
      if (length(centers) != 2) {
        stop("in 'centers'.\n Unexpected lenght for this argument while using SOM. It must be a vector of 2 elements")
      }
      arg.list[["grid"]] <- somgrid(xdim = centers[1], ydim=centers[2], topo = "rectangular")
      som.grid <- do.call("som", arg.list)
      centers <- centers[1] * centers[2]
    }
    Y <- som.grid$codes[[1]]
    attr(Y, "dimnames")<- NULL
    attr(Y, "index") <- som.grid$unit.classif
  } 
  return(Y)
}


