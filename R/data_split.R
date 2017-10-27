##     data_split.R Split data into two different sets
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

#' @title Split data into two different sets.
#' @description Split data into two different sets by a specific fraction. Splitting data is normally used to obtain a train and a validation set.
#' @param x The input grid object.
#' @param y The observations object.
#' @param fraction A value between (0,1) that indicates the fraction of the data that will define the train set. The unselected data will be the test set. Default is 3/4.
#' @return A list containing the grids x and y splitted as well as the indices in the original grids.
#' @details The function internally intersect datasets x and y in such a manner that only the observations present in both sets are then splitted, by \code{\link[transformeR]{getTemporalIntersection}}. It also removes those days were there is no value in at least 1 station by using \code{\link[transformeR]{filterNA}}).
#' 
#' 
#' @author J. Bano-Medina
#' @importFrom magrittr %>%
#' @export
#' @family subsetting
#' @examples
#' x <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' y <- VALUE_Iberia_pr
#' # Take a look at the dimensions of the data:
#' dim(VALUE_Iberia_pr$Data)
#' # Split the data 
#' data.splitted <- data_split(x,y,fraction = 0.5)
#' str(data.splitted)
#' # Take a look at the dimensions of the pbservations of the train set:
#' dim(data.splitted$yT$Data)
data_split <- function(x,y, fraction = 3/4) {
  y <- filterNA(y)
  x <- getTemporalIntersection(y,x,which.return = 'prd')
  y <- getTemporalIntersection(y,x,which.return = 'obs')
  
  indT <- sample(1:getShape(y,dimension = "time"),size = floor(fraction*getShape(y,dimension = "time")),replace = FALSE) %>% sort()
  indt  <- setdiff(1:getShape(y,dimension = "time"),indT)
  xT <- subsetDimension(x,dimension = "time",indices = indT)
  xt <- subsetDimension(x,dimension = "time",indices = indt)
  yT <- subsetDimension(y,dimension = "time",indices = indT)
  yt <- subsetDimension(y,dimension = "time",indices = indt)
  return(list("xT" = xT, "xt" = xt, "yT" = yT, "yt" = yt, "indT" = indT, "indt" = indt))}