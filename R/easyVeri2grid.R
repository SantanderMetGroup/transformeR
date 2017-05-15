#     easyVeri2grid easyVerification matrix to climatological grid conversion
#
#     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
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



#' @title easyVerification matrix to climatological grid conversion
#' @description Convert a xyz-type verification matrix as returned by \code{veriApply} to a climatological grid
#' @param easyVeri.mat A matrix containing the verification measure,
#'  as returned by \code{\link[easyVerification]{veriApply}}
#' @param obs.grid The grid containing the verifying reference used in the call to \code{veryApply}
#'  producing the score matrix.
#' @param verifun Optional. Character string indicating the value of the \code{verifun} argument. Just for 
#' a better traceability and metadata completeness. 
#' @return A climatological grid.
#' @seealso \code{\link{climatology}}, \code{\link{plotClimatology}}.
#' 
#' @export
#' 
#' @author J. Bedia

easyVeri2grid <- function(easyVeri.mat, obs.grid, verifun = NULL) {
      x <- obs.grid$xyCoords$x     
      y <- obs.grid$xyCoords$y     
      if (length(x) != ncol(easyVeri.mat) | length(y) != nrow(easyVeri.mat)) {
            stop("XY coordinates and matrix dimensions do not match")      
      }
      obs.grid$Data <- easyVeri.mat
      attr(obs.grid$Data, "dimensions") <- c("lat", "lon")
      obs.grid <- redim(obs.grid, member = FALSE)
      # Fake climatology:fun attribute
      clim.att <- ifelse(is.null(verifun), "easyVeri", verifun)
      attr(obs.grid[["Data"]], "climatology:fun") <- clim.att
      return(obs.grid)
}


#' @title Identify significant verification points
#' @description Two-tailed alternative hypothesis testing for \pkg{easyVerification} "sigma" outputs
#' @param ref.mean Numeric of length one. Null hypothesis reference mean (e.g. 0.5 for AUC tests, 0 for skill scores)
#' @param est.mean easyVerification output matrix with scores/measures. This is the \code{cat[num]} element of the \code{veriApply} output
#' @param sigma Standard deviation of the estimated mean. This is the \code{cat[num].sigma} element of the \code{veriApply} output.
#' @param n Sample mean (number of ensemble members)
#' @return A easyVerification-like matrix of p-values. Ready to be passed to \code{\link{easyVeri2grid}}
#'  for climatological grid conversion, and possibly to \code{\link{map.stippling}} after that, to depict significant points
#'  in a verification map.
#' @importFrom stats pnorm 
#' @author J Bedia
#' @export
#' @seealso \code{\link{easyVeri2grid}}, for conversion of verification matrices into climatological grids.
#'  Also, \code{\link{map.stippling}}, to draw significant points on verification maps.

easyVeri.pval <- function(ref.mean, est.mean, sigma, n) {
    z <- (est.mean - ref.mean) / (sigma / sqrt(n))
    pval <- z
    pval[z < 0 & !is.na(z)] <- 2 * pnorm(z[z < 0 & !is.na(z)])
    pval[z >= 0 & !is.na(z)] <- 2 * (1 - pnorm(z[z >= 0 & !is.na(z)]))
}


