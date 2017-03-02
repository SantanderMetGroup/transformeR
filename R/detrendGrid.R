#     detrendGrid.R Linear detrending of a grid
#
#     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Linear detrending
#' @description Perform a linear detrending along the time dimension of a grid
#' @param grid Input grid (possibly multimember)
#' @param grid2 Optional grid. If provided, the output is the detrended \code{grid2}
#'  using the regression coefficients calculated using \code{grid}. Default to \code{NULL}.
#' @template templateParallelParams
#' @return A detrended grid. 
#' @details  Performs a simple linear detrending by fitting a linear model and retaining the residuals.
#' An attribute indicating the linear detrending is added to the \code{Variable} component of the output grid.
#' 
#' In the presence of missing data in the time series, it operates by filtering them prior to linear model fitting. The
#' missing data positions are then restored back to the output detrended series.
#' 
#' @template templateParallel
#' @export
#' @importFrom parallel stopCluster parApply
#' @importFrom stats lm coef
#' @importFrom magrittr %>%
#' @export
#' @author J Bedia, J Fernandez, M.D. Frias
#' @examples 
#' data("iberia_ncep_ta850")
#' monthly <- aggregateGrid(iberia_ncep_ta850, aggr.m = list(FUN = "mean"))
#' plot(monthly$Data[,4,2], ty = 'l')
#' abline(reg = lm(monthly$Data[,4,2] ~ I(1:length(monthly$Data[,4,2]))), lty = 2)
#' det <- detrendGrid(monthly, parallel = FALSE)
#' # Detrended series in red
#' lines(det$Data[,4,2], col = "red")
#' abline(reg = lm(det$Data[,4,2] ~ I(1:length(det$Data[,4,2]))), col = "red", lty = 2)
#' legend("topright", c("Raw", "Detrended"), lty = 1, col = c(1,2))


detrendGrid <- function(grid,
                        grid2 = NULL,
                        parallel = FALSE,
                        max.ncores = 16,
                        ncores = NULL) {
      grid <- redim(grid, var = TRUE, member = TRUE)
      parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
      if (parallel.pars$hasparallel) {
            apply_fun <- function(...) {
                  parallel::parApply(cl = parallel.pars$cl, ...)
            }
            on.exit(parallel::stopCluster(parallel.pars$cl))
      } else {
            apply_fun <- apply
      }
      arr <- grid$Data
      x <- as.numeric(as.Date(getRefDates(grid)))
      dimNames <- getDim(grid)
      mar <- grep("^time$", dimNames, invert = TRUE)
      message("[", Sys.time(), "] - Detrending...")
      if (is.null(grid2)) {
            arr <- unname(apply_fun(X = arr, MARGIN = mar, FUN = function(y) {
                  out <- rep(NA, length(x))
                  ind <- intersect(which(!is.na(y)), 1:length(x))
                  out[ind] <- tryCatch(expr = summary(lm(y ~ I(x)))$resid + mean(y, na.rm = TRUE),
                                       error = function(err) {
                                             out
                                       })
                  return(out)
            }))
            dimNames <- getDim(grid) 
            perm <- match(dimNames, c("time", "var", "member", "lat", "lon"))
            arr <- aperm(arr, perm)
      } else {
            grid2 <- redim(grid2, var = TRUE, member = TRUE)
            dims <- getShape(grid2)
            x2 <- as.numeric(as.Date(getRefDates(grid2)))
            arr <- unname(apply_fun(X = arr, MARGIN = mar, FUN = function(y) {
                  tryCatch(expr = {
                        coefs <- coef(lm(y ~ I(x)))
                        coefs[1] + x2 * coefs[2]
                  },
                  error = function(err) {
                        rep(NA, length(x2))
                  })
            }))
            # apply drops the time dimension if a singleton
            # (https://radfordneal.wordpress.com/2008/08/20/design-flaws-in-r-2-%E2%80%94-dropped-dimensions/)
            if (length(dims) != length(dim(arr))) arr <- unname(abind(arr, along = -1L))
            dimNames <- getDim(grid2) 
            perm <- match(dimNames, c("time", "var", "member", "lat", "lon"))
            arr <- aperm(arr, perm)
            resid <- grid2$Data - arr
            clim <- redim(grid, drop = TRUE) %>% climatology(by.member = FALSE) %>% redim(var = TRUE, member = TRUE) %>% getElement("Data")
            nmem <- getShape(grid2, "member")
            ntime <- getShape(grid2, "time")
            arr <- lapply(1:nmem, function(x) {
                  aux <- asub(resid, idx = x, dims = grep("member", dimNames), drop = FALSE)
                  aux1 <- lapply(1:ntime, function(y) {
                        aux1 <- asub(aux, idx = y, dims = grep("^time", dimNames), drop = FALSE)
                        aux1 + clim
                  })
                  unname(do.call("abind", c(aux1, along = grep("^time", dimNames))))
            })
            arr <- unname(do.call("abind", c(arr, along = grep("member", dimNames))))
            grid <- grid2
      }
      message("[", Sys.time(), "] - Done.")
      grid$Data <- arr
      attr(grid$Data, "dimensions") <- dimNames
      grid <- redim(grid, drop = TRUE) 
      attr(grid$Variable, "detrended:method") <- "linear"
      return(grid)
}
# End
