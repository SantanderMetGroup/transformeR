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
#' @param ref.pred Grid of predictions. It is used to calculate the threshold that will further be used to convert to binary the grid x.
#' @param values A vector of length 2. For example, values = c(0,1), which is the DEFAULT. Then every sample satisfying the condition is 
#' equal to the second element of 'values' (i.e., in our example would be equal to 1), whereas if a sample does 
#' not satisfy the condition then takes the first element (i.e., in our example would be equal to 0). 
#' @param simulate A logical value. If TRUE then the output is an stochastic sample for given the probability of rain 'p'. 
#' Therefore only where the input grid 'x' is a grid of probabilities (e.g., for example coming from a logistic regression).
#' @return A new grid object with binary values
#' @details The function works for regular and irregular grids as downloaded from \pkg{loadeR}).
#' @author J. Bano-Medina
#' @importFrom stats quantile runif
#' @export
#' @examples \donttest{
#' require(climate4R.datasets)
#' data(VALUE_Iberia_pr$Data)
#' # Take a look at the data:
#' head(VALUE_Iberia_pr$Data)
#' # Convert to complete binary variable:
#' ybin <- binaryGrid(VALUE_Iberia_pr,threshold = 1)
#' head(ybin$Data)
#' # Convert to partial binary variable:
#' ybin2 <- binaryGrid(VALUE_Iberia_pr,threshold = 1, partial = TRUE)
#' head(ybin2$Data)
#' # Convert to binary simulating:
#' dat <- gridArithmetics(ybin,0.5) # to build a dataset with probabilities
#' ybin3 <- binaryGrid(dat,simulate = TRUE)
#' head(ybin3$Data)
#' }

binaryGrid <- function(x, 
                       condition = "GE", 
                       threshold = NULL, 
                       partial = FALSE, 
                       ref.obs = NULL, 
                       ref.pred = NULL, 
                       values = c(0,1),
                       simulate = FALSE) {
  condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
  loc <- FALSE
  if (!isRegular(x)) {loc <- TRUE}
  x <- redim(x, loc = loc)
  nMemb <- dim(x$Data)[which(getDim(x) == "member")]
  for (j in 1:nMemb) {
    if (is.null(threshold)) {
      if (isTRUE(simulate)) {
        if (isRegular(x)) {
          xx <- suppressWarnings(array3Dto2Dmat(redim(subsetGrid(x,members = j), member = FALSE)$Data))
        } else {
          xx <- x$Data[j,,]
        }
        s <- matrix(runif(nrow(xx)*ncol(xx),min = 0,max = 1),nrow = nrow(xx), ncol = ncol(xx))
        xbin <- (xx > s)*1
      } else {
        ref.obs <- redim(ref.obs, loc = loc)
        if (isRegular(x)) {
          xx <- suppressWarnings(array3Dto2Dmat(redim(subsetGrid(x,members = j), member = FALSE)$Data))
          xx.obs <- suppressWarnings(array3Dto2Dmat(redim(subsetGrid(ref.obs,members = 1),member = FALSE)$Data))
          if (is.null(ref.pred)) {xx.pred <- xx} else {xx.pred <- suppressWarnings(array3Dto2Dmat(redim(subsetGrid(ref.pred,members = 1), member = FALSE)$Data))}
        } else {
          xx <- x$Data[j,,]
          xx.obs <- ref.obs$Data[1,,]
          if (is.null(ref.pred)) {xx.pred <- xx} else {xx.pred <- redim(ref.pred, loc = loc)$Data[1,,]}
        }
        
        frec <- apply(X = xx.obs, MARGIN = 2, function(X) {
          return(length(which(X == 0))/length(which(!is.na(X))))
        })
        xbin <- xx
        for (i in 1:length(frec)) {
          if (condition == "LT" | condition == "LE") {frec[i] <- 1 - frec[i]}
          thre <- quantile(xx.pred[,i],frec[i], na.rm = TRUE)
          xbin[,i] <- binaryGrid.(xx[,i], condition = condition, threshold = thre, partial = partial, values = values)
        }
      }  
    } else {
      if (isRegular(x)) {
        xx <- suppressWarnings(array3Dto2Dmat(redim(subsetGrid(x,members = j), member = FALSE)$Data))
      } else {
        xx <- subsetGrid(x,members = j)$Data
      }
      xbin <- binaryGrid.(xx, condition = condition, threshold = threshold, partial = partial, values = values)
    }
    
    if (isRegular(x)) {
      x$Data[j,,,] <- mat2Dto3Darray(xbin,x$xyCoords$x,x$xyCoords$y) 
    } else {
      x$Data[j,,] <- xbin
    }
  }
  if (nMemb == 1) {x <- redim(x,drop = TRUE) %>% redim(member = FALSE, loc = loc)}
  return(x)}

binaryGrid. <- function(x, condition, threshold, partial, values) {
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
    x[ind0] <- values[1]
    x[ind1] <- values[2]
  } else {
    x[eval(parse(text = paste("x", ineq2, "threshold")))] <- values[1]}
  return(x)}
