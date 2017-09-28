##     convert2bin.R Convert grid values to binary variable
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
#' @param grid The input grid to be converted to binary. It can be an object or a matrix/array.
#' @param threshold Upon or equal the threshold the grid values turn to 1, whereas below it turns to 0. Default is 0.5.
#' @param partial If TRUE, only values below the threshold will turn to 0 and the others will remain with their original grid real value. Default is FALSE.
#' @return A new grid object with binary values
#' @details The function only works for a vector of observations/predictands downloaded from \code{\link{loadeR}}).
#' 
#' Appends the attribute \code{subset = "convert2bin"} in the \code{Variable} element.
#' 
#' @author J. Bano-Medina
#' @export
#' @family subsetting
#' @examples
#' data("VALUE_Iberia_tp")
#' # Take a look at the data:
#' head(VALUE_Iberia_tp$Data)
#' # Convert to complete binary variable:
#' bin.total <- convert2bin(VALUE_Iberia_tp,threshold = 1)
#' head(bin.total$Data)
#' # Convert to partial binary variable:
#' bin.partial <- convert2bin(VALUE_Iberia_tp,threshold = 1, partial = TRUE)
#' head(bin.partial$Data)

convert2bin <- function(x,threshold = 0.5, partial = FALSE) {
  if (class(x) == 'list') {
    if (!partial) {
      x$Data[x$Data >= threshold] <- 1 
      x$Data[x$Data < threshold] <- 0}
    else {
      x$Data[x$Data < threshold] <- 0}}
  
  if (class(x) == 'numeric' || class(x) == 'matrix' || class(x) == 'array') {
    if (!partial) {
      x[x >= threshold] <- 1 
      x[x < threshold] <- 0}
    else {
      x[x < threshold] <- 0}}
  
  return(x)}