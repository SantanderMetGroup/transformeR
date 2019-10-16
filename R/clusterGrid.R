#     clusterGrid.R Cluster analysis of grid data
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

#'@title Cluster analysis of grids
#'@description Performs cluster analysis of grids, multigrids or multimember multigrids. Several clustering algorithms are available.  
#'@inheritParams clusterGrid_3D 
#'@seealso \link[stats]{kmeans}, \link[stats]{hclust}, \link[kohonen]{som}.
#'@return A new C4R grid object that contains the clusters created using the specified algorithm. Clusters are included in the dimension 'time'.
#'The clustering type, number of clusters and other algorithm-specific parameters are provided as attributes.
#'@details 
#'\strong{kmeans}
#'
#'While using the  K-means algorithm, the number of clusters (argument 'centers') needs to be provided (no default). 
#'See  \link[stats]{kmeans} for more details in the implementation.
#'
#'\strong{hierarchical}
#'
#'While using the hierarchical algorithm (check \link[stats]{hclust} for further information) 
#'\code{clusterGrid} allows the user either to especify the number of clusters ('centers') or not. 
#'If the argument 'centers' is not provided, they are automatically set and the tree is cut when the height 
#'difference between two consecutive divisions (sorted in ascending order) is larger than the interquartile 
#'range of the heights vector (see \link[stats]{cutree}) . 
#'
#'\strong{som}
#'
#'While using the SOM (self-organizing maps) algorithm (check \link[kohonen]{som} for further information), the argument 'centers' is provided as
#' a two-element vector, indicating the dimensions \code{xdim,ydim} of the grid (see \link[kohonen]{somgrid}).
#'Otherwise, by default 48 clusters (8x6) with rectangular topology are obtained. 
#'@author J. A. Fernandez
#'@export
#'@importFrom magrittr %>% 
#'@examples 
#'#Example of K-means clustering: 
#' data(NCEP_Iberia_psl, package = "transformeR")
#' clusters<- clusterGrid(NCEP_Iberia_psl, type="kmeans", centers=10, iter.max=1000)
#'
#'#Example of hierarchical clustering: 
#'clusters<- clusterGrid(NCEP_Iberia_psl, type="hierarchical")
#'
#'#Example of som clustering: 
#'clusters<- clusterGrid(NCEP_Iberia_psl, type="som", centers = c(10,1))
#'
#'#Example of K-means clustering of several variables:
#' data(NCEP_Iberia_ta850, package = "transformeR")
#' clusters<- clusterGrid(makeMultiGrid(NCEP_Iberia_psl, NCEP_Iberia_ta850),
#'                        type="kmeans", centers=10, iter.max=1000)



clusterGrid <- function(grid, type = "kmeans", centers = NULL, iter.max = 10, nstart = 1, method = "complete") {
 
  type <- match.arg(type, choices = c("kmeans", "hierarchical", "som"))
  #Flags
  num.clusters <- NULL
  token <- NULL
  
  #Check of grid dimensions
  var.names <- getVarNames(grid)
  n.var <- length(var.names)
  if (suppressMessages(is.na(getShape(grid, dimension = "member")))) {
    grid<-redim(grid)
  }
  n.mem <- getShape(grid, "member")
  
  #Initialization of some attributes as matrix:
  cluster.mat <- array(dim = c(n.var, n.mem, getShape(grid, dimension = "time")))
  if (type == "kmeans") {
    withinss <- array(dim = c(n.var, n.mem, ncol = centers))
    betweenss <- matrix(nrow = n.var, ncol = n.mem)
  }else if (type == "hierarchical") {
    height <- array(dim = c(n.var, n.mem, getShape(grid, dimension = "time")-1))
    cutree.at.height <- matrix(nrow = n.var, ncol = n.mem)
    diff.height.threshold <- NULL
  }
  
  var.list <- vector("list", n.var)
  mem.list <- vector("list", n.mem)
  
  for (x in 1:n.var){
    l <- suppressMessages(subsetGrid(grid, var = var.names[x])) %>% redim(member = TRUE)
    for (m in 1:n.mem){
      # calculate clusters of 3D data
      sub.grid <- suppressMessages(subsetGrid(l, members = m, drop = TRUE))
      clusters <- suppressWarnings(clusterGrid_3D(sub.grid, type, centers, iter.max, nstart, method, num.clusters, token))
      #Extract attributes as matrix:
      cluster.mat[x, m, ] <- attr(clusters, "cluster")
      if (type == "kmeans") {
        withinss[x, m, ] <- attr(clusters, "withinss")
        betweenss[x, m] <- attr(clusters, "betweenss")
      }else if (type == "hierarchical") {
        height[x, m, ] <- attr(clusters, "height")
        cutree.at.height[x, m] <- attr(clusters, "cutree.at.height")
        if (m == 1 & is.null(centers)){
          diff.height.threshold <- attr(clusters, "diff.height.threshold")
        }
      }
      mem.list[[m]] <- clusters
    }
    var.list[[x]] <- suppressWarnings(bindGrid(mem.list, dimension = "member"))
  }
  
  out <- suppressWarnings(makeMultiGrid(var.list))
  
  #Set attributes as matrix:
  attr(out, "cluster") <- drop(cluster.mat)
  if (type == "kmeans") {
    attr(out, "withinss") <- drop(withinss)
    attr(out, "betweenss") <- drop(betweenss)
  }else if (type == "hierarchical") {
    attr(out, "height") <- drop(height)
    attr(out, "cutree.at.height") <- drop(cutree.at.height)
    attr(out, "diff.height.threshold") <- drop(diff.height.threshold)
  }
  return(out)
}



#'@title Cluster analysis of 3D grids
#'@description Performs cluster analysis of 3D grids. Several clustering algorithms are available.
#'@param grid A grid (gridded or station dataset), multigrid, multimember grid or multimember multigrid object, as 
#' returned e.g. by \code{loadeR::loadGridData} (or \code{loadeR::loadStationData}), a
#' multigrid, as returned by \code{makeMultiGrid}, or other types of multimember grids
#' (possibly multimember grids) as returned e.g. by \code{loadeR.ECOMS::loadECOMS}.
#'@param type Clustering algorithm to be used for the cluster analysis. 
#'Possible values are "\strong{kmeans}" (default), "\strong{hierarchical}", "\strong{som}". 
#'The core functions are \link[stats]{kmeans}, \link[stats]{hclust}, \link[kohonen]{som}, respectively. See Details.
#'@param centers Integer value indicating the number of clusters, \strong{k}, or center points. See Details.
#'@param iter.max (for the K-means algorithm) Integer value indicating the maximum number of iterations allowed. Default: 10.
#'@param nstart (for the K-means algorithm) If centers is a number, how many random sets should be chosen? Default: 1.
#'@param method (for the hierarchical algorithm) Agglomeration method to be used, one of "complete" (default), "ward.D", "ward.D2", "single",
#'"average", "mcquitty", "median" or "centroid".  
#'@keywords internal
#'@importFrom stats kmeans hclust cutree dist quantile
#'@importFrom kohonen som somgrid
#'@return A new 3D grid object that contains the clusters created using the specified algorithm.
#'The clustering type, number of clusters and other algorithm-specific parameters are provided as attributes.


clusterGrid_3D <- function(grid, type, centers, iter.max, nstart, method, num.clusters, token){
  type = tolower(type)
  grid.2D <- array3Dto2Dmat(grid$Data) #From 3D to 2D
  if (is.null(type) | type == "kmeans") {
    if (is.null(centers)) {
      stop("in 'centers'.\n In K-means the number of clusters ('centers') needs to be provided")
    }
    kmModel <- kmeans(grid.2D, centers, iter.max = iter.max, nstart = nstart) #Datos de entrenamiento en KNN     
    #Going back from 2D to 3D:
    Y <- mat2Dto3Darray(kmModel$centers, grid$xyCoords$x, grid$xyCoords$y)
  } else if (type == "hierarchical") {
    hc <- hclust(dist(grid.2D), method)
    if (!is.null(centers)) {
      num.clusters <- centers
    }
    if (is.null(centers) & is.null(num.clusters)) {
      # Auto-calculation of the number of clusters: Quartile method applied
      quantile.range <- quantile(hc$height, c(0.25, 0.75), na.rm= TRUE) 
      hc.height.diff <- numeric(length(hc$height) - 1)
      for (i in 1:length(hc$height) - 1) {
        hc.height.diff[i] <- hc$height[i + 1] - hc$height[i]
      }
      index <- which(hc.height.diff > (quantile.range[[2]] - quantile.range[[1]]))
      num.clusters <- length(hc$order) - index[1]
    }
    if(is.na(num.clusters)){
     stop("All the height differences are smaller than the interquartile range, probably due to the fact that the 'time' dimension is too small. We recommend to set the number of clusters manually...")
    }
    memb <- cutree(hc, k = num.clusters) #Found the corresponding cluster for every element
    cent <- NULL
    for (k in 1:num.clusters) {   #Set up the centers of the clusters
      cent <- rbind(cent, colMeans(grid.2D[memb == k, ,drop = FALSE]))
    }
    Y <- mat2Dto3Darray(cent, grid$xyCoords$x, grid$xyCoords$y)
  } else if (type == "som") {
    if (is.null(centers)) {
      som.grid <- som(grid.2D)
    } else {
      if (length(centers) != 2) {
        stop("in 'centers'.\n Unexpected lenght for this argument while using SOM. It must be a vector of 2 elements")
      }
      som.grid <- som(grid.2D, somgrid(xdim = centers[1], ydim=centers[2], topo = "rectangular"))
    }
    Y <- mat2Dto3Darray(som.grid$codes[[1]], grid$xyCoords$x, grid$xyCoords$y)
  } else {
    stop("Input data is not valid.\n'", paste(type), "' is not a valid algorithm")
  }
  #Setting up metadata for Y
  aux <- grid
  aux$Data <- Y
  attr(aux, "cluster.type") <- type
  # Add attributes depending on the cluster algorithm
  if (type == "kmeans") {
    attr(aux, "cluster") <- kmModel$cluster
    attr(aux, "centers") <- centers
    attr(aux, "withinss") <- kmModel$withinss
    attr(aux, "betweenss") <- kmModel$betweenss
  } else if (type == "hierarchical") {
    attr(aux, "centers") <- num.clusters
    attr(aux, "cluster") <- memb
    attr(aux, "height") <- hc$height
    attr(aux, "cutree.at.height") <- hc$height[(length(hc$order) - num.clusters) + 1] #the previous heigth divides in "centers" numb. of clusters 
    if (is.null(centers) & is.null(token)){
      attr(aux, "diff.height.threshold") <- (quantile.range[[2]] - quantile.range[[1]])
      token <- "used"
    }
  } else if (type == "som") {
    attr(aux, "cluster") <- som.grid$unit.classif
    if (is.null(centers)) {
      attr(aux, "centers") <- 8*6
    } else {
      attr(aux, "centers") <- centers[1] * centers[2]
    }
  }
  return(aux)
}

