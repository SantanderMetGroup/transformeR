#' @title Reconstruct a Grid from EOFs and principal components
#' @description Reconstructs a grid of a climatic variable from the outputs of a
#' principal components analysis
#' @param prinCompObj A EOF analysis object as returned by \code{\link{prinComp}}
#' @param var Character string indicating the variable to be re-constructed. In case of
#' PCA analyses performed on a single variable (either multimember or not), this can be omitted.
#' @return A grid
#' @importFrom abind abind
#' @importFrom utils tail
#' @importFrom magrittr %>% %<>% multiply_by add
#' @export
#' @author J. Bedia 
#' @family pca
#' @examples 
#' # Multigrid constructor:
#' multigrid <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_ta850, NCEP_Iberia_psl)
#' # In this example, we retain the first 10 PCs
#' pca <- prinComp(multigrid, n.eofs = 10)
#' # We recover the sea-level pressure grid from the its PCs:
#' names(pca)
#' psl2 <- gridFromPCA(pca, "psl")
#' str(psl2)
#' # The attributes of psl2 indicate that this is a reconstructed grid from 10 PCs, 
#' # explaining 99\% of the variance:
#' attributes(psl2)
#' multigrid$Variable$varName
#' # psl is the 3rd one
#' 
#' # An example of multimember reconstruction from a multimember PC analysis:
#' data("NCEP_Iberia_psl")
#' plotClimatology(climatology(NCEP_Iberia_psl), backdrop.theme = "coastline",
#'                 main = "DJF Mean Sea-level Pressure (1983-2002)")
#' pca2 <- prinComp(NCEP_Iberia_psl, n.eofs = 1)
#' psl_recovered <- gridFromPCA(pca2)
#' plotClimatology(climatology(psl_recovered), backdrop.theme = "coastline", 
#'                 main = "DJF Mean Sea-level Pressure (1983-2002)\n-Reconstructed from first EOF-")
#' # The mean field is similar, but there are (small) differences in temporal variability:
#' plot(NCEP_Iberia_psl$Data[,3,3], ty = 'l', ylab = "psl")
#' lines(psl_recovered$Data[,3,3], col = "red")
#' title("Time series for a selected grid-point")
#' legend("top", c("original", "reconstructed"), lty = 1, col = c(1,2), bty = "n")
#' # The 1st EOF captures 85% of total MSLP field variance:
#' attributes(psl_recovered)$'explained_variance'

gridFromPCA <- function(prinCompObj, var) {
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
    x <- attributes(prinCompObj)$xCoords
    y <- attributes(prinCompObj)$yCoords
    start <- attributes(prinCompObj)$dates_start
    end <- attributes(prinCompObj)$dates_end
    proj <- attributes(prinCompObj)$projection
    pco <- prinCompObj[[var.ind]]
    level <- attributes(prinCompObj)$"level"[[var.ind]]
    prinCompObj <- NULL
    scale <- lapply(1:length(pco), function(x) attributes(pco[[x]][["orig"]])$"scaled:scale")
    center <- lapply(1:length(pco), function(x) attributes(pco[[x]][["orig"]])$"scaled:center")
    exv <- lapply(1:length(pco), function(x) attributes(pco[[x]])$"explained_variance")
    Members <- names(pco)
    aux.list  <- lapply(1:length(pco), function(n) {
        tcrossprod(pco[[n]]$PCs, pco[[n]]$EOFs) %>% t() %>% multiply_by(scale[[n]]) %>% add(center[[n]]) %>% t() %>% mat2Dto3Darray(x, y)
    })
    pco <- NULL
    Data <- unname(do.call("abind", c(aux.list, along = -1L)))
    attr(Data, "dimensions") <- c("member", "time", "lat", "lon")
    out <- list("Variable" = list("varName" = var, "level" = level),
                "Data" = Data,
                "xyCoords" = list("x" = x, "y" = y),
                "Dates" = list("start" = start, "end" = end))
    if (getShape(out, "member") > 1) c(out, Members = Members)
    attr(out, "nPcs") <- unlist(lapply(1:length(exv), function(x) length(exv[[x]])))
    attr(out, "explained_variance") <- unlist(lapply(1:length(exv), function(x) round(tail(exv[[x]], 1), 2)))
    attr(out$xyCoords, "projection") <- proj
    out %<>% redim(out, drop = TRUE)
    return(out)
}
# End

