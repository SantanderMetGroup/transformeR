#' @title Plot EOFs
#' @description Plots an arbitrary number of EOFs. Useful to have a quick overview of the main spatial modes
#'  of a (possibly multimember) grid.
#' @param prinCompObj A PCA object as returned by \code{\link{prinComp}}
#' @param var Character string indicating the variable whose EOFs are to be displayed. If the PCA analysis has
#' been applied to 1 single grid, this argument can be omitted.
#' @param n.eofs Number of EOFs to be displayed. Default to NULL, indicating that all computed EOFS
#' will be represented
#' @param member An integer indicating the position of the member whose EOFs are to be displayed. Default 1, 
#' corresponding to the first member. Ignored for non multimember grids.
#' @return A plot with as many panels as EOFs requested, in the original units of the variable
#' @export
#' @author J. Bedia 
#' @seealso \code{\link{prinComp}}
#' @examples 
#' # Winter temperature at 850 mb isobaric surface pressure level is loaded (period 1981--2010):
#' data("NCEP_Iberia_hus850")
#' # PCA analysis, retaining the PCs that explain 95\% of the total variance:
#' pca <- prinComp(NCEP_Iberia_hus850, v.exp = .95)
#' # Plot of all EOFs
#' plotEOF(pca)
#' # Plot the first 4 EOFs:
#' plotEOF(pca, n.eofs = 4)
#' 
#' # Example with PCA analysis of a multigrid (multiple variables)
#' data("NCEP_Iberia_ta850")
#' data("NCEP_Iberia_psl")
#' multigrid <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_ta850, NCEP_Iberia_psl)
#' # PCA analysis, retaining the first 9 PCs of each variable:
#' pca2 <- prinComp(multigrid, n.eofs = 9)
#' names(pca2)
#' # EOFs for geopotential
#' plotEOF(pca2, "ta850")
#' plotEOF(pca2, "psl", n.eofs = 2)



plotEOF <- function(prinCompObj, var = NULL, member = 1, n.eofs = NULL) {
      member <- as.integer(member)
      varNames <- attributes(prinCompObj)$names
      if (length(varNames) == 1) {
            ind.var <- 1
      } else {
            if (is.null(var)) {
                  stop("The argument 'var' is missing, with no default value")
            }
            ind.var <- match(var, varNames)
            if (is.na(ind.var)) {
                  stop("Variable given in 'var' not found")
            }
            if (var == "COMBINED") {
                  stop("It is not possible to display the combined EOF")
            }
      }
      if (length(prinCompObj[[1]]) == 1) {
            if (member != 1) {
                  warning("Argument 'member' was ignored")
            }
            member <- 1
      }
      if (!(member %in% 1:length(prinCompObj[[1]]))) {
            stop("'member' value must be between 1 and the total number of members (", length(prinCompObj[[1]]), " in this case)")
      }
      eofs <- prinCompObj[[ind.var]][[member]]$EOFs
      if (is.null(n.eofs)) {
            n.eofs <- ncol(eofs)
      }
      if (!(n.eofs %in% 1:ncol(eofs))) {
            stop("'n.eofs' value must be between 1 and the total number of possible EOFs (", ncol(eofs), " in this case)")
      }
      x <- attributes(prinCompObj)$xCoords
      y <- attributes(prinCompObj)$yCoords
      mu <- attributes(prinCompObj[[ind.var]][[member]])$"scaled:center"
      sigma <- attributes(prinCompObj[[ind.var]][[member]])$"scaled:scale"
      if (is.null(mu)) mu <- 0
      if (is.null(sigma)) sigma <- 1
      out <- list("Data" = mat2Dto3Darray(t(eofs[,1:n.eofs]*sigma + mu), x, y), "xyCoords" = list(x = x, y = y))
      multiPlot(out, split.dim.name = "time", titles = paste("EOF", 1:n.eofs), multi.member = FALSE)
}
# End

