#     setTimeResolution.R Set Grid Temporal Resolution
#
#     Copyright (C) 2021 Santander Meteorology Group (http://www.meteo.unican.es)
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

#'@title Set Time Resolution
#'@description Set the time resolution for a C4R grid object. Several time resolutions are available.  
#'@param grid grid or station data.
#'@param time_resolution time resolution to be set in the grid.
#'@return A C4R grid object that will contain the attribute \code{time_resolution} in grid$Variable.
#'@details 
#'The available time resolutions are "1h", "3h", "6h", "12h", "DD", "MM", "YY" or "unknown".
#'@author O.Mirones
#'@export


setTimeResolution <- function(grid, time_resolution){
  
  time_resolution <- match.arg(time_resolution, c("1h","3h","6h","12h","DD","MM","YY","unknown"))
  attr(grid[["Variable"]], "time_resolution") <- time_resolution
  
  return(grid)
}
