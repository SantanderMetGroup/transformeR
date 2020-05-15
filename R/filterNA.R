##     filterNA.R Remove missing values from grids
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

#' @title Remove missing values from grids
#' @description Removes the dates for all grid points whenever there exists missing values (i.e., NaN or NA).
#' @param grid The input grid to be filtered. 
#' @return A new grid object without missing values
#' @details The function also takes care of adjusting dates and other relevant metadata (via the internal \code{\link{subsetDimension}}).
#' 
#' @importFrom magrittr %>% extract2
#' @author J. Bano-Medina, J. Bedia
#' @family downscaling.helpers
#' @export
#' @examples
#' # Check if the dataset contains missing values (YES):
#' anyNA(VALUE_Iberia_pr$Data)
#' getShape(VALUE_Iberia_pr)
#' na.filtered <- filterNA(VALUE_Iberia_pr)
#' # Check if the dataset contains missing values (NO):
#' anyNA(na.filtered$Data)
#' getShape(na.filtered)

filterNA <- function(grid) {
    if (!is.na(getShape(grid,"member"))) stop("No multimember objects allowed due to possible temporal inconsistencies")
    if (!is.na(getShape(grid,"var"))) stop("No multigrid objects allowed due to possible temporal inconsistencies")
    if (!anyNA(grid$Data)) {
        message("NOTE: No missing values were found in the input grid")
    } else {
        time.ind <- grep("time", getDim(grid))
        if (isRegular(grid)) {
            dims <- getShape(grid)
            dim(grid$Data) <- c(getShape(grid,"time"),getShape(grid,"lat")*getShape(grid,"lon"))
        }
        mask <- apply(grid$Data,MARGIN = 2, FUN = function(z) is.na(z) %>% all())
        grid$Data[,mask] <- -Inf
        if (isRegular(grid)) dim(grid$Data) <- dims
        
        
        na.index <- apply(grid$Data,MARGIN = time.ind,anyNA) %>% which()
        na.index <- setdiff(1:getShape(grid, "time"), na.index)
        grid <- subsetDimension(grid, dimension = "time", indices = na.index)
        
        if (isRegular(grid)) {
            dims <- getShape(grid)
            dim(grid$Data) <- c(getShape(grid,"time"),getShape(grid,"lat")*getShape(grid,"lon"))
        }
        grid$Data[,mask] <- NA
        if (isRegular(grid)) dim(grid$Data) <- dims
        
        attr(grid$Variable, "subset") <- "filterNA"
    }
    return(grid)
}
