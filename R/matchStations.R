##     matchStations.R Convert grid values to a binary variable
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

#' @title Order the stations according to a particular order
#' @description Convert a real variable into a binary variable (i.e., 0 or 1) filtering by a threshold. 
#' @param obj A station object to be ordered, normally predictions.
#' @param ref A station object used as reference to order, normally observations.
#' @return A new grid object with binary values.
#' @author J. Bano-Medina
#' @importFrom stats quantile
#' @export


matchStations <- function(obj,ref) {
  ind <- match(ref$Metadata$station_id,obj$Metadata$station_id)
  nStations <- dim(obj$Data)[which(getDim(obj) == "loc")]
  out <- lapply(1:nStations, FUN = function(z) {
    subsetGrid(obj,station.id = ind[z])
  }) %>% bindGrid(dimension = "loc") %>% redim(drop = TRUE)
}
