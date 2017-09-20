    ## grid2PCs.R Projection of a grid onto an EOF

    ## Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)

    ## This program is free software: you can redistribute it and/or modify
    ## it under the terms of the GNU General Public License as published by
    ## the Free Software Foundation, either version 3 of the License, or
    ## (at your option) any later version.

    ## This program is distributed in the hope that it will be useful,
    ## but WITHOUT ANY WARRANTY; without even the implied warranty of
    ## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ## GNU General Public License for more details.

    ## You should have received a copy of the GNU General Public License
    ## along with this program.  If not, see <http://www.gnu.org/licenses/>. 


#' @title Projection of a grid onto an EOF
#' @description Projection of a grid onto a user-defined EOF
#' @param prinCompObj An object created by \code{\link{prinComp}}
#' @param grid Input grid to project. Multigrids are not allowed.
#' @param n.pcs Number of principal components to be retained. Default to the number of EOFs contained in the input \code{prinCompObj}.
#' @return A list, each component corresponding to a member (a list of length 1 if the projected grid is not multimember),
#' containing a matrix of PCs, being the number of columns defined by \code{n.pcs}, and the number of rows by the
#' length of the time dimension.
#' @details The function is intended to project a grid onto user specified EOFs, contained in \code{prinCompObj}.
#' Note that the grid should have the same spatial extent and resolution than the original EOF, so input grids from
#' different models should be first interpolated. Also, to ensure consistency, both objects should contain the same 
#' variable name.  
#' 
#' Note that the function is not currently implemented to deal with decadal predictions
#'  (i.e., 'runtime' dimension is not handled)
#'  
#' @author J Bedia
#' @export
#' @importFrom magrittr %>% extract2
#' @seealso \code{\link{prinComp}} for EOF analysis.
#' @family pca
#' @examples 
#' # Obtain the Sea-level pressure PCs of CFSv2 forecast using the NCEP reanalysis EOF:
#' data("NCEP_Iberia_psl")
#' NCEP_psl_PCAobject <- prinComp(NCEP_Iberia_psl, v.exp = .95)
#' data("CFS_Iberia_psl")
#' # Need to be in the same spatial coordinates
#' CFS_psl_grid <- interpGrid(CFS_Iberia_psl, new.coordinates = getGrid(NCEP_Iberia_psl))
#' # Need to have the same short name of the variable
#' CFS_psl_grid$Variable$varName <- "slp"
#' CFSv2_PCs <- grid2PCs(NCEP_psl_PCAobject, grid = CFS_psl_grid)
#' str(CFSv2_PCs)

grid2PCs <- function(prinCompObj, grid, n.pcs = NULL) {
    gridName <- getVarNames(grid)
    if (!all(gridName %in% names(prinCompObj))) {
        stop("Input PCA object and grid variable names do not match", call. = FALSE)
    }
    grid <- redim(grid, member = TRUE, var = FALSE, runtime = FALSE)
    dimNames <- getDim(grid)
    if ("var" %in% dimNames) stop("Multigrids are not allowed in 'grid' argument", call. = FALSE)
    EOF <- prinCompObj[[gridName]][[1]][["EOFs"]]
    mu <- attr(prinCompObj[[gridName]][[1]][["orig"]], "scaled:center")
    sigma <- attr(prinCompObj[[gridName]][[1]][["orig"]], "scaled:scale")
    prinCompObj <- NULL
    lat.ind <- grep("lat", dimNames)
    lon.ind <- grep("lon", dimNames)
    if (prod(dim(grid$Data)[c(lat.ind,lon.ind)]) != nrow(EOF)) {
        stop("Incompatible array dimensions. Input grid and EOF must be in the same grid", call. = FALSE)
    }
    if (is.null(n.pcs)) {
        n.pcs <- ncol(EOF)
    } else if (n.pcs > ncol(EOF)) {
        message("Number of PCs requested exceeds EOF dimensions. ", ncol(EOF), " PCs will be returned.")
        n.pcs <- ncol(EOF)
    }
    n.mem <- getShape(grid, "member")
    PC.list <- lapply(1:n.mem, function(i) {
        X <- subsetGrid(grid, members = i, drop = TRUE) %>% extract2("Data") %>% array3Dto2Dmat()
        X  <-  (X - mu) / sigma 
        PCs <- X %*% EOF
        PCs[, 1:n.pcs, drop = FALSE]
    })
    return(PC.list)
}
# End

