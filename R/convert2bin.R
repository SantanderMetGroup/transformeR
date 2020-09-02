##     convert2bin.R Convert grid values to a binary variable
##
##     Copyright (C) 2020 Santander Meteorology Group (http://www.meteo.unican.es)
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
#' @param threshold Upon or equal the threshold the grid values turn to 1, whereas below it turns to 0. This can be a single value or a vector. In case it is a vector it will apply a different threshold on each station. If threshold is NULL, then the data is converted to binary by a value that makes the ref.pred to have the same number of 0 values than those in the ref.obs. Default is NULL. 
#' @param partial If TRUE, only values below the threshold will turn to 0 and the others will remain with their original grid real value. Default is FALSE.
#' @param ref.obs Grid of observations. It is used to determine the number of days where there is no event (or value equal to 0).
#' @param ref.pred Grid of predictions. It is used to calculate the threshold that will further be used to convert the grid  \code{x} to binary.
#' @return A new grid object with binary values
#' @details The function only works for a vector of observations/predictands downloaded from \pkg{loadeR}).
#' @author J. Bano-Medina
#' @importFrom stats quantile
#' @export
#' @family downscaling.helpers
#' @examples \donttest{
#' require(climate4R.datasets)
#' data(VALUE_Iberia_pr)
#' # Take a look at the data:
#' head(VALUE_Iberia_pr$Data)
#' # Convert to complete binary variable:
#' ybin <- convert2bin(VALUE_Iberia_pr,threshold = 1)
#' head(ybin$Data)
#' # Convert to partial binary variable:
#' ybin2 <- convert2bin(VALUE_Iberia_pr, threshold = 1, partial = TRUE)
#' head(ybin2$Data)
#' }

convert2bin <- function(x, threshold = NULL, partial = FALSE, ref.obs = NULL, ref.pred = NULL) {
  dimNames <- getDim(x)
  if (is.null(ref.pred)) {ref.pred <- x}
  if (is.null(threshold)) {
    if (length((dim(ref.obs$Data))) < 2) {ref.obs$Data <- matrix(ref.obs$Data, nrow = length(ref.obs$Data), ncol = 1) }
    frec <- apply(X = ref.obs$Data, MARGIN = 2, function(X){
      return(length(which(X == 0))/length(which(!is.na(X) == TRUE)))})
    for (i in 1:length(frec)) {
      thre <- quantile(ref.pred$Data[,i],frec[i], na.rm = TRUE)
      x$Data[,i] <- convert2bin.(x$Data[,i], threshold = thre, partial = partial)}}
  else {
    x$Data <- convert2bin.(x$Data, threshold = threshold, partial = partial)}
  attr(x$Data, "dimensions") <- dimNames
  return(x)}

convert2bin. <- function(x,threshold, partial) {
  if (!partial) {
    x[x >= threshold] <- 1 
    x[x < threshold] <- 0}
  else {
    x[x < threshold] <- 0}
  return(x)
}
