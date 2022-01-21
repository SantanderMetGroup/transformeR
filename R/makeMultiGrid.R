#     makeMultiGrid.R Multigrid constructor
#
#     Copyright (C) 2018 Santander Meteorology Group (http://www.meteo.unican.es)
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

#' @title Multigrid constructor 
#' @description Constructs a (possibly multimember) multigrid from different (multimember) grids.
#' A multigrid can be considered as a \dQuote{stack} of grids with similar spatiotemporal extents,
#'  useful to handle sets of predictors as a single block.
#' @param ... Input grids to form the multigrid. These must be compatible in time and space (see details). 
#' For flexibility, they can be introduced as a list or directly as consecutive arguments.
#' @param spatial.tolerance numeric. Coordinate differences smaller than \code{spatial.tolerance} will be considered equal 
#' coordinates. Default to 0.001 --assuming that degrees are being used it seems a reasonable rounding error after interpolation--.
#' This value is passed to the \code{\link{identical}} function to check for spatial consistency of the input grids.
#' @param skip.temporal.check Logical. Should temporal matching check of input fields be skipped?
#' Default to \code{FALSE}. This option can be useful in order to construct multigrids 
#' from grids of different temporal characteristics. See details.
#'  (e.g., a multi-seasonal multifield), to be passed to \code{\link[visualizeR]{spatialPlot}}
#' @return A (multimember) multigrid object encompassing the different input (multimember) grids
#' @details The function makes a number of checks in order to test the spatiotemporal compatibility of the input multi-member grids.
#'  Regarding the temporal concordance, it can be skipped by setting the argument \code{skip.temporal.check} to \code{TRUE}.
#'  It is implicitly assumed that all temporal data from the different
#'  multimember grids correspond to the same time zone ("GMT"). The time zone itself is not important, as long as it is the
#'  same across datasets, because temporal consistency is checked on a daily basis (not hourly), allowing the inclusion of 
#'  predictors with different verification times and temporal aggregations. For instance, instantaneous geopotential at 12:00 
#'  is compatible with mean daily surface temperature, always that both variables correspond to the same days. Different time 
#'  resolutions are not compatible and will return an error (for instance, 6-hourly data is incompatible with daily values, 
#'  because their respective time series for a given season have different lengths).
#'  
#'  The spatial consistency of the input grids is also checked. In order to avoid possible errors from the user, the spatial
#'   consistency (i.e., equal XY coordinates) of the input grids must be ensured before attempting the creation of the multigrid,
#'   otherwise giving an error. This can be achieved either through the specification of the same 'lonLim' and 'latLim' argument
#'   values when loading the grids, or using the \code{\link{interpGrid}} interpolator in conjuntion with the \code{\link{getGrid}}
#'   method.
#'   
#'  \strong{Variable names} 
#'  
#'  When a vertical level of the variable is defined (i.e., is not \code{NA}), 
#'  the short name of the variable is modified to the standard format \code{"var@@level"} 
#'  (e.g., \code{"ta@@850"} for air temperature at 850mb surface pressure level). This way, the same
#'  variable at different vertical levels can be differentiated at a glance using \code{\link{getVarNames}}, 
#'  for instance.
#'  
#'  
#'  
#' @note A multigrid can not be passed to the interpolator \code{\link{interpGrid}} directly. Instead, the 
#' multimember grids should be interpolated individually prior to multigrid construction.  
#' @export
#' @importFrom abind abind
#' @family downscaling.helpers
#' @author J. Bedia 
#' @seealso \code{\link{interpGrid}} for spatial consistency of input grids.
#' 
#' @examples \donttest{
#' require(climate4R.datasets)
#' # Creation of a multigrid from three different grids:
#' data(NCEP_Iberia_ta850)
#' data(NCEP_Iberia_hus850)
#' data(NCEP_Iberia_psl)
#' # An example of different temporal aggregations, temporally compatible:
#' # sea-level pressure is a daily mean, while specific humidity and air temperature
#' # (850 mb surface isobaric pressure level) are instantaneous data verifying at 12:00 UTC:
#' # air temperature
#' mf <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' 
#' # The new object inherits the global attributes from the first grid, as it is assumed
#' # that all input grids come from the same data source:
#' attributes(mf)
#' # The data structure has now one additional dimension ("var"), along which the data arrays
#' # have been binded:
#' str(mf$Data)
#' 
#' # Example of multimember multigrid creation from several multimember grids:
#' # Load three different multimember grids with the same spatiotemporal ranges:
#' data("CFS_Iberia_tas")
#' data("CFS_Iberia_hus850")
#' data("CFS_Iberia_pr")
#' mm.mf <- makeMultiGrid(CFS_Iberia_tas, CFS_Iberia_hus850, CFS_Iberia_pr)
#' # Different fields should not be plotted together in the same plot directly, unless 
#' # their units are compatible.
#' # subsetGrid and visualizeR::spatialPlot can be used to this aim, if needed.
#' # For instance:
#' tas <- subsetGrid(mm.mf, var = "tas")
#' require(visualizeR)
#' spatialPlot(climatology(tas), backdrop.theme = "coastline", rev.colors = TRUE)
#' }

makeMultiGrid <- function(..., spatial.tolerance = 1e-3, skip.temporal.check = FALSE) {
    field.list <- list(...)
    stopifnot(is.logical(skip.temporal.check))
    checkgrid <- unlist(lapply(field.list, function(x) isGrid(x)))
    if (any(!checkgrid)) {
        field.list <- unlist(field.list, recursive = FALSE)
        checkgrid <- unlist(lapply(field.list, function(x) isGrid(x)))   
    }
    loc <- FALSE
    if (!isRegular(field.list[[1]])) loc <- TRUE
    if (any(!checkgrid)) {
        stop("Invalid input data")
    } else { 
        climfun <- attr(field.list[[1]]$Data, "climatology:fun")
        if (length(checkgrid) == 1) {
            warning("One single grid was provided as input")  
            # field.list[[1]]$Dates <- list(field.list[[1]]$Dates)
        } else {
            ## Climatologies ----------
            # field.list <- lapply(1:length(field.list), function(x) redim(field.list[[x]], drop = TRUE))
            field.list <- lapply(1:length(field.list), function(x) redim(field.list[[x]], var = TRUE, loc = loc))
            ### check var dimension position
            varind <- unique(vapply(1:length(field.list), FUN.VALUE = integer(1), function(x) which(getDim(field.list[[x]]) == "var")))
            if (length(varind) > 1) stop("Input grids have different dimensions")#hay que discutir esto
            tol <- spatial.tolerance
            for (i in 2:length(field.list)) {
                # Spatial test
                if (!all.equal(field.list[[1]]$xyCoords, field.list[[i]]$xyCoords,
                               check.attributes = FALSE, tolerance = tol)) {
                    stop("Input data are not spatially consistent")
                }
                # temporal test
                if (!skip.temporal.check) {
                    if (!identical(as.POSIXlt(field.list[[1]]$Dates$start)$yday,
                                   as.POSIXlt(field.list[[i]]$Dates$start)$yday) | !identical(as.POSIXlt(field.list[[1]]$Dates$start)$year,
                                                                                              as.POSIXlt(field.list[[i]]$Dates$start)$year)) {
                        stop("Input data are not temporally consistent.\nMaybe the 'skip.temporal.check' argument should be set to TRUE?")
                    }
                }
                # data dimensionality
                suppressMessages(checkDim(field.list[[1]], field.list[[i]]))
            }
            # Atributos de la variable ----------------
            # Lista de todos los atributos de todos los grids, menos el primero ('names')
            aux.attr.list <- lapply(1:length(field.list), function(x) attributes(field.list[[x]]$Variable))
            auxl <- lapply(1:length(aux.attr.list), function(x) names(aux.attr.list[[x]]))
            #all.attrs <- Reduce(union, auxl)[-1]
            aux.attrs <- Reduce(union, auxl)
            all.attrs <- aux.attrs[-which(aux.attrs=="names")] # filter no-names attrs, regardless of the position in the list
            aux.attr.list <- NULL
            l <- vector("list", length(all.attrs))
            names(l) <- all.attrs
            for (i in 1:length(field.list)) {
                attrnames <- names(attributes(field.list[[i]]$Variable))[-1]
                for (j in 1:length(all.attrs)) {
                    atributo.ind <- unlist(lapply(attrnames, function(x) identical(x, all.attrs[j])))
                    atributo <- attrnames[atributo.ind]
                    if (length(atributo) != 0) {
                        expr <- attr(field.list[[i]]$Variable, which = atributo)
                        tryCatch({l[[j]][(length(l[[j]]) + 1):((length(l[[j]])) + length(atributo))] <- 
                            deparse(expr)}, error = function(err) { deparse(expr) })
                    } else {
                        l[[j]][(length(l[[j]]) + 1):((length(l[[j]])) + length(atributo))] <- NA
                    }
                }
            }
            l <- lapply(l, "gsub", pattern = "\\\"", replacement = "")
            # varName and levels
            levs <- unname(sapply(field.list, "getGridVerticalLevels"))                                 
            varnames <- sapply(field.list, FUN = "getVarNames")
            attributes(field.list[[1]]$Variable) <- l
            names(field.list[[1]]$Variable) <- c("varName","level")
            field.list[[1]]$Variable[["varName"]] <- varnames
            field.list[[1]]$Variable[["level"]] <- levs
            ## $Dates -------------------
            field.list[[1]]$Dates <- lapply(1:length(field.list), function(x) field.list[[x]]$Dates)
            ## Select larger string of dim names -------------
            dimNames <- lapply(1:length(field.list), function(x) getDim(field.list[[x]]))
            dimNames <- dimNames[[which(lengths(dimNames) == max(lengths(dimNames)))[1]]]
            ## Bind data ----------
            field.list[[1]]$Data <- unname(do.call("abind",
                                                   c(lapply(1:length(field.list),
                                                            function(x) field.list[[x]]$Data),
                                                     along = varind))) 
            attr(field.list[[1]]$Data, "dimensions") <- dimNames
        }
        if (!is.null(climfun)) attr(field.list[[1]]$Data, "climatology:fun") <- climfun 
    }
    invisible(field.list[[1]])
}
# End

