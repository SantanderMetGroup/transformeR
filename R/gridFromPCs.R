#' @title Reconstruct a Grid from EOFs and principal components
#' @description Reconstructs a grid of a climatic variable from the outputs of a
#' principal components analysis
#' @param prinCompObj A EOF analysis object as returned by \code{\link{prinComp}}
#' @param var Character string indicating the variable to be re-constructed. In case of
#' PCA analyses performed on a single variable (either multimember or not), this can be omitted.
#' @return A list similar to the \code{loadGridData} output (package \pkg{loadeR}), but simplified. See details.
#' @details The output of this function returns the minimum required information to use the
#'  \code{\link{plotMeanGrid}} method, and is intended for comparison and visual analysis
#'  of the differences between the original grids and the reconstructed ones, for instance
#'  in determining an optimal number of PCs etc... Hence, the temporal information (i.e.,
#'   the \code{Dates} object) is lost, and should be retrieved from the original grid/multigrid
#'   object used to compute the PC/EOF analysis.
#' @importFrom abind abind
#' @importFrom utils tail
#' @export
#' @author J. Bedia 
#' @seealso \code{\link{prinComp}}, \code{\link{plotEOF}}
#' @examples 
#' # Multigrid constructor:
#' multigrid <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_ta850, NCEP_Iberia_psl)
#' # In this example, we retain the first 10 PCs
#' pca <- prinComp(multigrid, n.eofs = 10)
#' # We recover the sea-level pressure grid from the its PCs:
#' names(pca)
#' psl2 <- gridFromPCs(pca, "psl")
#' str(psl2)
#' # The attributes of psl2 indicate that this is a reconstructed grid from 10 PCs, 
#' # explaining 99\% of the variance:
#' attributes(psl2)
#' multigrid$Variable$varName
#' # psl is the 3rd one
#' 
#' # An example of multimember reconstruction from a multimember PC analysis:
#' data("CFS_Iberia_tas")
#' # Note that multimember pca analysis takes some time, depending on the computer
#' pca2 <- prinComp(CFS_Iberia_tas, n.eofs = 10)
#' tas_recovered <- gridFromPCs(pca2)
#' plotMeanGrid(tas_recovered, multi.member = TRUE)
#' # Also note that now the length of the "nPCs" and "explained_variance" matches 
#' # the number of members:
#' attributes(tas_recovered)


gridFromPCs <- function(prinCompObj, var) {
      varNames <- attributes(prinCompObj)$names 
      if (length(varNames) == 1L) {
            var <- varNames
            var.ind <- 1
      } else {
            if (var == "COMBINED") {
                  stop("The combined grid can't be reconstructed (only individual variables)")
            }
            var.ind <- match(var, varNames)
            if (is.na(var.ind)) {
                  stop("Variable not found.\nCheck the 'variables' attribute of the input")
            }
      }
      # scaling <- attributes(prinCompObj)$"scaled:method"
      x <- attributes(prinCompObj)$xCoords
      y <- attributes(prinCompObj)$yCoords
      proj <- attributes(prinCompObj)$projection
      pco <- prinCompObj[[var.ind]]
      prinCompObj <- NULL
      scale <- lapply(1:length(pco), function(x) attributes(pco[[x]])$"scaled:scale")
      center <- lapply(1:length(pco), function(x) attributes(pco[[x]])$"scaled:center")
      exv <- lapply(1:length(pco), function(x) attributes(pco[[x]])$"explained_variance")
      level <- attributes(pco)$level  
      Members <- names(pco)
      aux.list  <- lapply(1:length(pco), function(n) {
            Xhat <- tcrossprod(pco[[n]]$PCs, pco[[n]]$EOFs) * scale[[n]] + center[[n]]
            mat2Dto3Darray(Xhat, x, y)
      })
      pco <- NULL
      dimNames <- attr(aux.list[[1]], "dimensions")
      Data <- unname(do.call("abind", c(aux.list, along = -1L)))
      if (identical(Data, drop(Data))) {
            dimNames <- append(dimNames, "member", after = 0L)
            mm <- TRUE
      } else {
            Data <- drop(Data)
            mm <- FALSE
      }
      attr(Data, "dimensions") <- dimNames
      out <- list("Variable" = list("varName" = var, "level" = level), "Data" = Data, "xyCoords" = list("x" = x, "y" = y), "Members" = Members)
      if (!mm) {
            out <- out[-length(out)]      
      }
      attr(out, "nPcs") <- unlist(lapply(1:length(exv), function(x) length(exv[[x]])))
      attr(out, "explained_variance") <- unlist(lapply(1:length(exv), function(x) round(tail(exv[[x]], 1), 2)))
      attr(out$xyCoords, "projection") <- proj
      return(out)
}
# End




