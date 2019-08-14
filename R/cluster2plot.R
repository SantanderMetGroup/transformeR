#     cluster2plot.R Convert a grid of clusters into a C4R grid for plotting
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

#'@title Convert a grid of clusters into a C4R grid for plotting
#'@description This function allows to prepare a grid of clusters (included in 'time' dimension) for plotting. A subset of a variable and/or member is also performed in multimember or multigrid objects.   
#'@param cluster A grid (gridded or station dataset), multigrid, multimember grid or multimember multigrid object, as 
#' returned by \code{clusterGrid}, with clusters in the 'time' dimension.
#'@param members (optional) Integer value indicating \strong{the position} of the member to be subset. Default: 1.
#'If input grid has no member dimension, this argument is ignored. 
#'@param var (optional) Character vector indicating the variable name to be extracted. Default: first variable.
#'See \link[transformeR]{subsetGrid}. 
#'@seealso \link[transformeR]{makeMultiGrid}, \link[transformeR]{subsetGrid}, \link[transformeR]{subsetDimension}.
#'@return A new C4R grid object that contains the clusters from the specified variable and/or member. 
#'This subset grid of clusters is ready to be plotted with C4R plotting tools, e.g. \code{spatialPlot}, since clusters are intrepreted as variables (see \link[transformeR]{makeMultiGrid}).
#'@details  
#'@author J. A. Fernandez
#'@export
#'@examples 
#'#Example 1: 'cluster' is a 3D grid of clusters.
#'data(NCEP_Iberia_psl, package = "transformeR")
#'clusters<- clusterGrid(NCEP_Iberia_psl, type="kmeans", centers=10)
#'mg <- cluster2plot(clusters)
#'visualizeR::spatialPlot(mg, backdrop.theme = "coastline", rev.colors = TRUE, layout = c(2,ceiling(attr(clusters, "centers")/2)), as.table=TRUE)
#'
#'#Example 2: 'cluster' is a grid of clusters with two variables (same for members). 
#'data(NCEP_Iberia_ta850, package = "transformeR")
#'clusters<- clusterGrid(makeMultiGrid(NCEP_Iberia_psl, NCEP_Iberia_ta850), type="kmeans", centers=10, iter.max=1000)
#'mg <- cluster2plot(clusters, members=1, var="psl") #To obtain first member and variable called "psl"
#'visualizeR::spatialPlot(mg, backdrop.theme = "coastline", rev.colors = TRUE, layout = c(2,ceiling(attr(clusters, "centers")/2)), as.table=TRUE)



cluster2plot <- function(cluster, members=1, var=getVarNames(cluster)[1]){
  
  #check if input grid has cluster_type attrib. 
  if(is.null(attr(cluster, "cluster.type"))){
    stop("Input grid is not a grid of clusters.")
  }
  s<- subsetGrid(cluster, var=var, members = members, drop=TRUE) #dimension members was = 2, after this is = 1
  cluster.grids <- lapply(1:attr(s, "centers"), function(x) {
    subsetDimension(s, dimension="time", indices=x)})
  mg <- do.call("makeMultiGrid", c(cluster.grids, skip.temporal.check = TRUE))
  attr(mg$Variable,"longname")<-paste("Cluster_", 1:getShape(mg, "var"),sep="") #Prints Cluster_1, Cluster_2, etc...

    return(mg)
}

