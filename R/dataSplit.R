##     dataSplit.R Split data into two different sets
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
#' @description Split data into two different sets by a specific fraction. Splitting data is 
#' normally used to obtain a train and a validation set.
#' @param x The input grid object.
#' @param y The observations object.
#' @param f Could be a fraction, value between (0,1) indicating the fraction of the data that will define the train set, 
#' or an integer indicating the number of folds. 
#' @param type A string, c("random","chronological"), indicating if the splittins should be random or in a choronological order. 
#' Default is "random".
#' @param test.pos An integer indicating the fold that would be the test posittion. Default is NULL.
#' Example: if you divide the data in 4 folds and define test.pos = 2. then the second fold would be the test set and the first, 
#' third and fourth would be the train set.
#' @param scale A logical value. Indicates if the data should be estandardized (i.e., 0 mean and 1 standard deviation).
#' @return A list containing the grids x and y splitted.
#' @family downscaling.helpers
#' @author J. Bano-Medina
#' @export
#' @examples
#' x <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' y <- VALUE_Iberia_pr
#' # Split the data in 3 folds in chronological order and let the second fold be the test set:
#' data.splitted <- dataSplit(x,y,f = 3, test.pos = 2, type = "chronological")
#' dim(x$Data)
#' dim(y$Data)
#' dim(data.splitted$yT$Data)
#' dim(data.splitted$yt$Data)
#' dim(data.splitted$xT$Data)
#' dim(data.splitted$xt$Data)
#' # ... and now the same but with the data stanrdized.
#' data.splitted2 <- dataSplit(x,y,f = 3, test.pos = 2, type = "chronological", scale = TRUE)
#' head(data.splitted$xT$Data[1,,1,1])
#' c(mean(data.splitted$xT$Data[1,,1,1]),sd(data.splitted$xT$Data[1,,1,1]))
#' head(data.splitted2$xT$Data[1,1,,1,1])
#' c(mean(data.splitted2$xT$Data[1,1,,1,1]),sd(data.splitted2$xT$Data[1,1,,1,1]))
#' 
#' # Split the data in a train and test dataset (i.e., 2 folds) 
#' # indicating the fraction of the train dataset.
#' data.splitted3 <- dataSplit(x,y,f = 0.82, type = "random")
#' dim(y$Data)
#' dim(data.splitted3$yT$Data)
#' dim(data.splitted3$yt$Data)
dataSplit <- function(x,y, f = 3/4, type = "random", test.pos = NULL, scale = FALSE) {
  if (f < 1) {
    if (type == "random") {
      indT <- sample(1:getShape(y,dimension = "time"),size = floor(f*getShape(y,dimension = "time")),replace = FALSE) %>% sort()
      indt <- setdiff(1:getShape(y,dimension = "time"),indT)}
    else if (type == "chronological") {
      indT <- 1:floor(f*getShape(y,dimension = "time"))
      indt <- setdiff(1:getShape(y,dimension = "time"),indT)}
  }
  else {
    size_fold <- floor(getShape(y,dimension = "time")/f)
    if (type == "random") {
      inds_fold <- array(data = sample(1:(size_fold*f), size = length(1:(size_fold*f)), replace = FALSE), dim = c(size_fold,f))}
    else if (type == "chronological") {
      inds_fold <- array(data = 1:(size_fold*f), dim = c(size_fold,f))}
    if (length(1:getShape(y,dimension = "time")) != length(1:(size_fold*f))) {
      if (test.pos != f) {
        indT <- c(inds_fold[,-test.pos],setdiff(1:getShape(y,dimension = "time"), 1:(size_fold*f)))
        indt <- inds_fold[,test.pos]}
      if (test.pos == f) {
        indt <- c(inds_fold[,test.pos],setdiff(1:getShape(y,dimension = "time"), 1:(size_fold*f)))
        indT <- c(inds_fold[,-test.pos])}}
    else{
      indT <- c(inds_fold[,-test.pos])
      indt <- inds_fold[,test.pos]}}
  xT <- subsetDimension(x,dimension = "time",indices = indT)
  xt <- subsetDimension(x,dimension = "time",indices = indt)
  yT <- subsetDimension(y,dimension = "time",indices = indT)
  yt <- subsetDimension(y,dimension = "time",indices = indt)
  
  if (scale) xt <- localScaling(xt, base = xT, scale = TRUE)
  if (scale) xT <- localScaling(xT, base = xT, scale = TRUE)
  return(list("xT" = xT, "xt" = xt, "yT" = yT, "yt" = yt))}

