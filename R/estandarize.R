##     eatandarize.R Estandarize to 0 means and 1 unit standard deviation.
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
#' @title Estandarize a dataset.
#' @description Estandarize all given variables of a dataset to 0 mean and 1 unit standard deviatoin. If the input dataset if a dataset used for test or validation then it is standarized by the train mean and standard deviatoins.
#' @param grid  Is a grid object to be estandarized as returned from \code{link[downscaleR]{prepare_predictors}} or \code{link[downscaleR]{prepare_newdata}}, depending if it is a "train" or a "test" dataset, respectively.
#' @param dataset Indicates if the data is going to be used as a "train" or "test" dataset. There is no default option.
#' @param grid.ref If the grid is a "test" dataset the this parameter is the grid object of the "train" dataset as returned from \code{link[downscaleR]{prepare_predictors}}.
#' @return A new grid object with estandarized values.
#' @details The function estandarizes via the internal R function \code{\link[base]{scale}}.
#' @author J. Bano-Medina
#' @importFrom matlab repmat
#' @export
#' @examples 
#' # Loading data
#' x <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' x <- subsetGrid(x, years = 1985:1995)
#' y <- VALUE_Iberia_tp
#' # Preparing data and estandarizing...
#' xtrain <- prepare_predictors(x = x, y = y)
#' xtrain <- estandarize(xtrain, "train")
#' xtest <- prepare_newdata(x,xtrain)
#' xtest <- estandarize(xtest, "test", xtrain)
#' str(xtrain)
#' str(xtest)
estandarize <- function(grid, dataset, grid.ref = NULL){
  if (dataset == "train") {
    if (!is.null(grid$x.global)) {
      grid$x.global <- scale(grid$x.global, center = TRUE, scale = TRUE)}
    if (!is.null(grid$x.local)) {
      for (i in 1:length(xT.local$x.local)) {
        grid$x.local[[i]]$member_1 <- scale(grid$x.local[[i]]$member_1, center = TRUE, scale = TRUE)}}}
  if (dataset == "test") {
    if (!is.null(grid$newdata.global)) {
      grid$newdata.global$member_1  <- (grid$newdata.global$member_1  - repmat(attributes(grid.ref$x.global)[2]$`scaled:center`,c(dim(grid$newdata.global$member_1)[1],1))) / repmat(attributes(grid.ref$x.global)[3]$`scaled:scale`,c(dim(grid$newdata.global$member_1)[1],1))}
    if (!is.null(grid$newdata.local)) {
      for (i in 1:length(xT.local$x.local)) {
        grid$newdata.local[[i]]$member_1  <- (grid$newdata.local[[i]]$member_1  - repmat(attributes(grid.ref$x.local[[i]]$member_1)[2]$`scaled:center`,c(dim(grid$newdata.local[[i]]$member_1)[1],1))) / repmat(attributes(grid.ref$x.local[[i]]$member_1)[3]$`scaled:scale`,c(dim(grid$newdata.local[[i]]$member_1)[1],1))}}}
  return(grid)}