#     prinComp.R Principal Component Analysis of grid data
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

#' @title Principal Component Analysis of Grids
#' @description Performs a Principal Component Analysis of grids, multigrids or multimember multigrids. The core of this
#'  function is \code{stats::prcomp}, with several specific options to deal with climate data.
#' @param grid A grid (gridded or station dataset), multigrid, multimember grid or multimember multigrid object
#' @param n.eofs Integer vector. Number of EOFs to be retained. Default to \code{NULL}, indicating
#'  that either all EOFs are kept, or that the next argument will be used as criterion
#'  for its determination. See next argument and details.
#' @param v.exp Maximum fraction of explained variance, in the range (0,1]. Used to determine the number of EOFs 
#' to be retained, as an alternative to \code{n.eofs}. Default to \code{NULL}. See details.
#' @param which.combine Optional. A character vector with the short names of the variables of the multigrid used
#' to construct 'combined' PCs (use the \code{\link{getVarNames}} helper if not sure about variable names).
#' @param scaling Method for performing the scaling (and centering) of the input raw data matrix. Currently only the \code{"gridbox"} option is available.
#' Currently accepted choices are \code{"field"} (the default) and \code{"gridbox"}. See details.
#' @param keep.orig Logical flag indicating wheter to return the input data -the standardized input data matrices-
#'  used to perform the PCA (\code{keep.orig = TRUE}) or not (\code{FALSE}). Default to \code{FALSE}.
#' @param quiet True to silence all the messages (but not the warnings)

#' @return A list of \emph{N} elements for multigrids, where \emph{N} is the number of input variables used, and 
#' \emph{N+1} if combined PCs are calculated, placed in the last place under the \code{"COMBINED"} name.
#'  In case of single grids (1 variable only), a list of length 1  (without the combined element). For each element of the list, the following objects are returned, either in the form of
#'   another list (1 element for each member) for multimembers, or not in the case of non multimember inputs:
#'  \itemize{
#'  \item \code{PCs}: A matrix of principal components, arranged in columns by decreasing importance order 
#'  \item \code{EOFs}: A matrix of EOFs, arranged in columns by decreasing importance order
#'  \item \code{orig}: Either the original variable in the form of a 2D-matrix (when \code{keep.orig = TRUE}),
#'  or \code{NA} when \code{keep.origin = FALSE} (the default). In both cases, the parameters used for input data standardization
#'  (mean and standard deviation) are returned as attributes of this component (see the examples).
#'  }
#'  The \dQuote{order of importance} is given by the explained variance of each PC, as indicated
#'  in the attribute \code{"explained_variance"} as a cumulative vector.
#'  Additional information is returned via the remaining attributes (see details), including geo-referencing and time.

#' @note Performing PCA analysis on multimember multigrids may become time-consuming and computationally expensive. 
#' It is therefore advisable to avoid the use of this option for large datasets, and iterate over single
#' multimember grids instead.
#' 
#' @details
#' 
#' \strong{Number of EOFs}
#' 
#' \code{n.eofs} and \code{v.exp} are alternative choices for the determination
#'  of the number of EOFs (hence also the corresponding PCs) to be retained. If both are \code{NULL} (the default)
#'  , all EOFs will be retained. If both are given a value different from \code{NULL}, 
#'  the \code{n.eofs} argument will prevail, and \code{v.exp} will be ignored, with a warning.
#'  When dealing with multigrids, the \code{n.eofs} argument can be either a single value or a vector
#'  of the same length as the number of variables contained in the multigrid plus (possibly) the COMBINED field if any.
#'  The same behaviour holds for \code{v.exp}. 
#'  
#' \strong{Scaling and centering}
#' 
#' In order to eliminate the effect of the varying scales of the different climate variables, the input
#' data matrix is always scaled and centered, and there is no choice to avoid this step. However, the mean
#' and standard deviation can be either computed for each grid box individually (\code{"gridbox"}) or for all
#' grid-boxes globally (i.e., at the field level, \code{"field"}). The last case is preferred in order to preserve
#'  the spatial structure of the original field, and has been set as the default, 
#'  returning one single mean and sigma parameter for each variable. If the \code{"gridbox"}
#' approach is selected, a vector of length \emph{n}, where \emph{n} is the number of grid-cells composing the 
#' grid, is returned for both the mean and sigma parameters (this is equivalent to using the \code{\link{scale}}
#' function with the input data matrix). 
#' 
#' The method used is returned as a global attribute of the returned object (\code{"scaled:method"}), and the 
#' \emph{mu} and \emph{sigma} parameters are returned as attributes of the corresponding variables
#'  (\code{"scaled:scale"} and \code{"scaled:center"} respectively).
#'  
#' As in the case of \code{n.eofs} and \code{v.exp} arguments, it is possible to indicate one single approach
#'  for all variables within multigrids (using one single value, as by default), or indicate a specific approach for
#'  each variable sepparately (using a vector of the same length as the number of variables contained in the multigrid). However,
#'   the latter approach is rarely used and it is just implemented for maximum flexibility in the downscaling experimental setup.
#'  
#' 
#' \strong{Combined EOF analysis}
#' 
#' When dealing with multigrid data, apart from the PCA analysis performed on each variable individually,
#' a combined analysis considering some or all variables together can be done. This is always returned in the last element
#' of the output list under the name \code{"COMBINED"}. The variables used for combination (if any) are controlled by the 
#' argument \code{which.combine}. 
#'
#' @export
#' @importFrom abind asub
#' @importFrom stats cov sd prcomp
#' @importFrom magrittr %>% %<>% 
#' @family pca
#' @references
#' Gutierrez, J.M., R. Ancell, A. S. Cofiño and C. Sordo (2004). Redes Probabilisticas
#'  y Neuronales en las Ciencias Atmosfericas. MIMAM, Spain. 279 pp.
#'   \url{http://www.meteo.unican.es/en/books/dataMiningMeteo}
#' @author J. Bedia, M. de Felice 
#' @examples 
#' data("NCEP_Iberia_hus850", "NCEP_Iberia_psl", "NCEP_Iberia_ta850")
#' multigrid <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' # In this example, we retain the PCs explaining the 99\% of the variance
#' pca <- prinComp(multigrid, v.exp = c(.95,0.90,.90), keep.orig = FALSE)
#' # The output is a named list with the PC's and EOFs (plus additional atttributes) for each variable
#' # within the input grid:
#' str(pca)
#' # Note that, apart from computing the principal components and EOFs for each grid, 
#' # it also returns, in the last element of the output list,
#' # the results of a PC analysis of the combined variables when which.combine is activated:
#' pca <- prinComp(multigrid, v.exp = c(.99,.95,.90,.95),
#'                 which.combine = c("air850", "slp), keep.orig = FALSE)
#' names(pca)
#' str(pca)
#' # A special attribute indicates the variables used for combination
#' attributes(pca$COMBINED)
#' # The different attributes of the pca object provide information regarding the variables involved
#' # and the geo-referencing information
#' str(attributes(pca))
#' # In addition, for each variable (and their combination), the scaling and centering parameters 
#' # are also returned. There is one value of each parameter per grid point. For instance, 
#' # the parameters for the specific humidity field are:
#' attributes(pca$shum850[[1]]$orig)$`scaled:center`
#' attributes(pca$shum850[[1]]$orig)$`scaled:scale`
#' # In addition, the (cumulative) explained variance of each PC is also returned:
#' vexp <- attributes(pca$shum850[[1]])$explained_variance
#' # The classical "scree plot":
#' barplot(1-vexp, names.arg = paste("PC",1:length(vexp)), las = 2, 
#'         ylab = "Fraction of unexplained variance")
#' 
#' # This is an example using a multimember object:
#' data("CFS_Iberia_hus850")
#' # In this case we retain the first 5 EOFs:
#' pca.mm <- prinComp(CFS_Iberia_hus850, n.eofs = 5) 
#' # Note that now the results of the PCA for the variable are a named list, with the results 
#' # for each member sepparately considered
#' str(pca.mm)
#' 
#' # The most complex situation comes from multimember multigrids:
#' data("CFS_Iberia_pr", "CFS_Iberia_tas")
#' # Now the multimember multigrid is constructed
#' mm.multigrid <- makeMultiGrid(CFS_Iberia_tas, CFS_Iberia_pr)
#' # Use different n.eofs for each variable:
#' pca.mm.mf <- prinComp(mm.multigrid, n.eofs = c(3,5))

prinComp <- function(grid,
                     n.eofs = NULL,
                     v.exp = NULL,
                     which.combine = NULL,
                     scaling = "gridbox",
                     keep.orig = FALSE,
                     quiet = FALSE) {
    if (!is.null(n.eofs) & !is.null(v.exp)) {
        message("NOTE: The 'v.exp' argument was ignored as 'n.eofs' has been indicated")
    }
    if (is.null(n.eofs) & is.null(v.exp)) {
        message("NOTE: All possible PCs/EOFs retained: This may result in an unnecessarily large object")
    }
    grid %<>% redim(var = TRUE, member = TRUE)
    n.vars <- getShape(grid, "var")
    var.names <- getVarNames(grid)
    if (!is.null(which.combine)) {
        if (n.vars == 1) {
            which.combine <- NULL
            message("NOTE: Only one variable available: 'which.combine' was set to NULL")
        } else {
            n.vars <- n.vars + 1
        }
    }
    if (!is.null(n.eofs)) {
        v.exp <- NULL
        if (any(n.eofs < 1)) {
            stop("Invalid number of EOFs selected", call. = FALSE)
        }
        if (length(n.eofs) == 1) n.eofs <- rep(n.eofs, n.vars)
        if (length(n.eofs) != n.vars) stop("The length of 'n.eofs' and the number of variables of the input grid differ\nForgot to include the combined PC?", call. = FALSE)
    }
    if (!is.null(v.exp)) {
        if (any(v.exp <= 0) | any(v.exp > 1)) {
            stop("The explained variance thresholds must be in the range (0,1]", call. = FALSE)
        }
        if (length(v.exp) == 1) v.exp <- rep(v.exp, n.vars)
        if (length(v.exp) != n.vars) stop("The length of 'v.exp' and the number of variables of the input grid differ\nForgot to include the combined PC?", call. = FALSE)
    }
    if (anyNA(grid$Data)) {
        stop("There are missing values in the input data array", call. = FALSE)
    }
    # Spatial check # doesn't work with stations - to be fixed with isMultiPoint helper
    if (length(grid$xyCoords$x) < 2 | length(grid$xyCoords$y) < 2) {
        stop("The dataset is not a field encompassing multiple grid-cells", call. = FALSE)
    }
    scaling <- match.arg(scaling, choices = c("field", "gridbox"), several.ok = TRUE)
    if (length(scaling) == 1) {
        scaling <- rep(scaling, length(var.names))  
    } else {
        if (length(scaling) != length(var.names)) stop("The length of 'scaling' argument should be either 1 or equal to the number of variables contained in the input grid", call. = FALSE)    
    }
    # field choice is temporarily disabled:
    if (any(scaling == "field")) {
        scaling <- gsub("field", "gridbox", scaling)
        message("NOTE: 'field' scaling is currently unavailable. Scaling was set to 'gridbox'")
    }
    # Variables-members as a nested list of matrices and scaling
    Xsc.list <- lapply(1:length(var.names), function(x) {
        l <- suppressWarnings(subsetGrid(grid, var = var.names[x])) %>% redim(member = TRUE)
        n.mem <- getShape(l, "member")
        lapply(1:n.mem, function(m) {
            subsetGrid(l, members = m, drop = TRUE)[["Data"]] %>% array3Dto2Dmat()
        }) 
    }) 
    names(Xsc.list) <- var.names
    Xsc.list %<>% prinComp.scale(scaling)
    # Combined PCs
    if (!is.null(which.combine)) {
        if (!all(which.combine %in% names(Xsc.list))) stop("Variables in 'which.combine' not found in the input grid", call. = FALSE)
        Xsc.list[["COMBINED"]] <- Xsc.list[match(which.combine, names(Xsc.list))] %>% combine.PCs()
        if (!quiet) message("[", Sys.time(), "] Performing PC analysis on ", n.vars - 1, " variables plus a combination ...")
    } else {
        if (!quiet) {
            if (n.vars > 1) {
                message("[", Sys.time(), "] Performing PC analysis on ", n.vars, " variables ...")
            } else {
                message("[", Sys.time(), "] Performing PC analysis on ", n.vars, " variable ...")
            }
        }
    }
    # PCA
    pca.list <- prinComp.(Xsc.list, n.eofs, v.exp, keep.orig) 
    Xsc.list <- NULL
    # Attributes
    names(pca.list) <- var.names
    levs <- unname(getGridVerticalLevels(grid))
    if (!is.null(which.combine)) {
        names(pca.list)[length(pca.list)] <- "COMBINED"
        attr(pca.list[["COMBINED"]], "combined_variables") <- which.combine
        levs %<>% append(NA)
    }
    attr(pca.list, "level") <- levs
    attr(pca.list, "dates_start") <- getRefDates(grid)
    attr(pca.list, "dates_end") <- getRefDates(grid, which = "end")
    attr(pca.list, "season") <- getSeason(grid)
    attr(pca.list, "xCoords") <- grid$xyCoords$x
    attr(pca.list, "yCoords") <- grid$xyCoords$y
    attr(pca.list, "projection") <- attr(grid$xyCoords, "projection")
    if (length(pca.list[[1]]) > 1) {
        for (i in 1:length(pca.list)) {
            names(pca.list[[i]]) <- grid$Members
        }
    }
    if (!quiet) message("[", Sys.time(), "] Done.")
    return(pca.list)
}
# End      
     

#' @title Local/Global grid scaling     
#' @description Scale a grid prior to PCA analysis (internal of \code{prinComp})
#' @param var.list A named nested list (variables-members) of fields in the form of 2D matrices (after \code{array3Dto2Dmat})
#' @param scaling Character vector of scaling types (same length as \code{var.list})
#' @return A named list of the same structure as the input \code{var.list} with the scaled (and centered) fields (the
#' names of the elements of the list are the variable names).
#' @details The \code{"gridbox"} scaling performs the scaling independently for each grid/station point (the mean and sd is calculated
#' sepparately for each point). On the contrary, the \code{"field"} approach substracts the same global mean and divides by the global sd
#' considering the whole field.
#' @keywords internal
#' @author J Bedia

prinComp.scale <- function(var.list, scaling) {
    out <- lapply(1:length(var.list), function(i) {
        if (scaling[i] == "field") {
            lapply(1:length(var.list[[i]]), function(x) {
                mu <- mean(var.list[[i]][[x]], na.rm = TRUE)
                sigma <- sd(var.list[[i]][[x]], na.rm = TRUE)
                Xsc <- scale(var.list[[i]][[x]], center = rep(mu, ncol(var.list[[i]][[x]])),
                             scale = rep(sigma, ncol(var.list[[i]][[x]])))
                attr(Xsc, "scaled:method") <- scaling[i]
                return(Xsc)
            })
        } else {
            lapply(1:length(var.list[[i]]), function(x) {
                Xsc <- scale(var.list[[i]][[x]], center = TRUE, scale = TRUE)
                attr(Xsc, "scaled:method") <- scaling[i]
                return(Xsc)
            })
        }   
    })
    names(out) <- names(var.list)
    return(out)
}    

#' @title Field Combination
#' @description Combine different fields into one single input 2D matrix for PCA (internal of \code{\link{prinComp}})
#' @param Xsc.list A nested list of (scaled and centered) input variables (as returned by \code{\link{prinComp.scale}})
#' @return A list of members with 2D combined scaled and centered matrices, resulting from the combination of the
#' input fields. A special attribute \code{"combined_variables"} indicated the variables combined
#' @keywords internal
#' @author J Bedia

combine.PCs <- function(Xsc.list) {
    aux.list <- vector("list", length(Xsc.list[[1]]))
    for (i in 1:length(aux.list)) {
        aux <- lapply(1:length(Xsc.list), function(x) Xsc.list[[x]][[i]])
        aux.list[[i]] <- do.call("cbind", aux)
    }
    attr(aux.list, "combined_variables") <- names(Xsc.list)
    return(aux.list)
}

#' @title Principal Component Analysis
#' @description Internal of prinComp, performing the PCA
#' @param Xsc.list A nested list of (scaled and centered) input variables (as returned by \code{\link{prinComp.scale}})
#' @param n.eofs n.eofs vector or NULL
#' @param v.exp explained variance vector or NULL
#' @return A list
#' @keywords internal
#' @author J Bedia, M de Felice

prinComp. <- function(Xsc.list, n.eofs, v.exp, keep.orig) {
    pca.list <- vector("list", length(Xsc.list))
    for (i in 1:length(pca.list)) {
        pca.list[[i]] <- lapply(1:length(Xsc.list[[i]]), function(x) {
            aux <- Xsc.list[[i]][[x]]
            is.combined <- ifelse(is.null(attr(aux, "scaled:center")), TRUE, FALSE)
            # Compute PCA with prcomp
            pr <- prcomp(aux)
            # Explained variance
            explvar <- cumsum((pr$sdev^2)/sum(pr$sdev^2))
            # Number of EOFs to be retained
            n <- if (!is.null(v.exp)) {
                findInterval(v.exp[i], explvar) + 1
            } else { 
                if (!is.null(n.eofs)) {
                    n.eofs[i]
                } else {
                    length(explvar)
                }
            }
            EOFs <- pr$rotation[ , 1:n, drop = FALSE]
            explvar <- explvar[1:n]
            PCs <- pr$x[ , 1:n, drop = FALSE]
            out <- if (is.combined) {
                list("PCs" = PCs, "EOFs" = EOFs, "orig" = NULL)
            } else {
                attrs <- attributes(aux)
                if (!keep.orig) aux <- NA
                mostattributes(aux) <- attrs
                list("PCs" = PCs, "EOFs" = EOFs, "orig" = aux)
            }
            attr(out, "explained_variance") <- explvar
            return(out)
        })
    }
    return(pca.list)
} 

