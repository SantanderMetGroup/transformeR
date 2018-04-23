##     binaryGrid.R Convert grid values to a binary variable
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

#' @title Convert grid values to a binary variable
#' @description Convert a real variable into a binary variable (i.e., 0 or 1) filtering by a threshold. 
#' @param x The input grid to be converted to binary. It can be an object or a matrix/array.
#' @param condition Inequality operator to be applied considering the given threshold.
#' \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#' \code{"LT"} = lower than, \code{"LE"} = lower or equal than. Values that accomplish the condition turn to 1 whereas the others turn to 0.
#' @param threshold An integer. Threshold used as reference for the condition. Default is NULL. 
#' @param partial A logic value. If TRUE, only values that do not accomplish the condition will turn to 0 and the others will remain with their original grid real value. 
#' For example, if condition = "GT" and threshold = 1 then every value lower than 1 will turn to 0. Default is FALSE.
#' @param ref.obs Grid of observations. It is used to determine the number of days where there is no event (or value equal to 0).
#' @param ref.pred Grid of predictions- It is used to calculate the threshold that will further be used to convert to binary the grid x.
#' @return A new grid object with binary values
#' @details The function works for regular and irregular grids as downloaded from \pkg{loadeR}).
#' @author J. Bano-Medina
#' @importFrom stats quantile
#' @export
#' @examples
#' # Take a look at the data:
#' head(VALUE_Iberia_pr$Data)
#' # Convert to complete binary variable:
#' ybin <- binaryGrid(VALUE_Iberia_pr,threshold = 1)
#' head(ybin$Data)
#' # Convert to partial binary variable:
#' ybin2 <- binaryGrid(VALUE_Iberia_pr,threshold = 1, partial = TRUE)
#' head(ybin2$Data)

binaryGrid <- function(x, condition = "GE", threshold = 1, partial = FALSE, ref.obs = NULL, ref.pred = NULL) {
  condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
  dimNames <- getDim(x)
  loc <- FALSE
  if (!isRegular(x)) {loc <- TRUE}
  x <- redim(x, loc = loc)
  for (j in 1:dim(x$Data)[which(getDim(x) == "member")]) {
    if (is.null(threshold)) {
      if (isRegular(x)) {
        xx <- suppressWarnings(array3Dto2Dmat(subsetGrid(x,members = 1)$Data))
        xx.obs <- suppressWarnings(array3Dto2Dmat(subsetGrid(ref.obs,members = 1)$Data))
        if (is.null(ref.pred)) {xx.pred <- xx} else {xx.pred <- suppressWarnings(array3Dto2Dmat(subsetGrid(ref.pred,members = 1)$Data))}
      } else {
        xx <- x$Data[1,,]
        xx.obs <- ref.obs$Data[1,,]
        if (is.null(ref.pred)) {xx.pred <- xx} else {xx.pred <- ref.pred$Data[1,,]}
      }
      
      frec <- apply(X = xx.obs, MARGIN = 2, function(X) {
        return(length(which(X == 0))/length(which(!is.na(X))))
      })
      xbin <- xx
      for (i in 1:length(frec)) {
        if (condition == "LT" | condition == "LE") {frec[i] <- 1 - frec[i]}
        thre <- quantile(xx.pred[,i],frec[i], na.rm = TRUE)
        xbin[,i] <- binaryGrid.(xx[,i], condition = condition, threshold = thre, partial = partial)
      }
      
    } else {
      if (isRegular(x)) {
        xx <- suppressWarnings(array3Dto2Dmat(subsetGrid(x,members = 1)$Data))
      } else {
        xx <- subsetGrid(x,members = 1)$Data
      }
      xbin <- binaryGrid.(xx, condition = condition, threshold = threshold, partial = partial)
    }
    
    if (isRegular(x)) {
      x$Data[j,,,] <- mat2Dto3Darray(xbin,x$xyCoords$x,x$xyCoords$y) 
    } else {
      x$Data[j,,] <- xbin
    }
  }
  x$Data <- drop(x$Data)
  attr(x$Data, "dimensions") <- dimNames
  return(x)}

binaryGrid. <- function(x, condition, threshold, partial) {
  ineq1 <- switch(condition,
                  "GT" = ">",
                  "GE" = ">=",
                  "LT" = "<",
                  "LE" = "<=")
  ineq2 <- switch(condition,
                  "GT" = "<=",
                  "GE" = "<",
                  "LT" = ">=",
                  "LE" = ">")
  if (!partial) {
    ind0 <- eval(parse(text = paste("x", ineq2, "threshold")))
    ind1 <- eval(parse(text = paste("x", ineq1, "threshold")))
    x[ind0] <- 0
    x[ind1] <- 1
  } else {
    x[eval(parse(text = paste("x", ineq2, "threshold")))] <- 0}
  return(x)}
