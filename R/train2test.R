##     convert2bin.R Splits a dataset into a train and a test dataset
##
##     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Splits a dataset into a train and a test dataset
#' @description A dataset is divided in order to generate two datasets: train and test. The amount of data that goes for each dataset is also controlled. 
#' @param grid The dataset to be splitted. 
#' @param splitting.size A numeric vector of two coordinates indicating the fraction that goes to the train dataset (first coordinate) and to the test dataset (second coordinate). Default is c(3/4,1/4). 
#' @param only.index If TRUE, returns a list containing both the index of the train data and the test data. If FALSE, returns the both indexes and the new train and test datasets generated. Default is FALSE.
#' @return Depends on the parameter "only.index". A list containing: two grid objects (train and test datasets), and two index vectors (train and test index)
#' @details It uses the "base" function \code{\link{floor}} to round the fraction of the observations going to a certain dataset. The function also takes care of adjusting dates and other relevant metadata (via the internal \code{\link{subsetDimension}}). 
#' 
#' Appends the attribute \code{subset = "train2test"} in the \code{Variable} element.
#' 
#' @author J. Bano-Medina
#' @export
#' @family subsetting
#' @examples
#' data("VALUE_Iberia_tp")
#' # Take a look at the dimensions:
#' getShape(VALUE_Iberia_tp)
#' new.datasets <- train2test(VALUE_Iberia_tp)
#' # Take a look at the dimensions of the train and test datasets:
#' getShape(new.datasets$grid.train)
#' getShape(new.datasets$grid.test)
#' # Take a look at the dimensions of the train and test datasets with a different splitting size:
#' new.datasets2 <- train2test(VALUE_Iberia_tp, spliting.size = c(1/2,1/2))
#' getShape(new.datasets2$grid.train)
#' getShape(new.datasets2$grid.test)

train2test <- function(grid, spliting.size = c(3/4,1/4), only.index = FALSE) {
  ind.train <- sample(1:getShape(grid,dimension = "time"),size = floor(spliting.size[1]*getShape(grid,dimension = "time")),replace = FALSE)
  ind.test  <- setdiff(1:getShape(grid,dimension = "time"),ind.train)
  if (only.index) {
    return(list("ind.train" = ind.train , "ind.test" = ind.test))} 
  else {
    grid.train <- subsetDimension(grid,dimension = "time",indices = ind.train)
    grid.test  <- subsetDimension(grid,dimension = "time",indices = ind.test)
    return(list("grid.train" = grid.train, "grid.test" = grid.test, "ind.train" = ind.train, "ind.test" = ind.test))}
}