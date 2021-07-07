#' @title PCA object EOFs to climatology grid
#' @description Converts a \code{prinComp} output to a climatology of EOFs. 
#' @param prinCompObj PCA object
#' @param ind.var index position of the target variable
#' @param member Integer. index position of the member whose EOFs are to be displayed.
#'  Default to 1 (ignored if no members available)
#' @param n.eofs Integer. Number of EOFs to be displayed (from 1 to \code{n.eofs}). Default to one -first- EOF.
#' @param rescale Logical flag. Default to \code{TRUE}, which rescales the EOF to the original input variable units.
#' @return A climatological grid. Note that EOFs are treated as members
#' @importFrom magrittr %>% 
#' @export
#' @author J Bedia
#' @examples \dontrun{
#' require(climate4R.datasets)
#' data("NCEP_Iberia_hus850", "NCEP_Iberia_psl", "NCEP_Iberia_ta850")
#' multigrid <- makeMultiGrid(NCEP_Iberia_hus850, NCEP_Iberia_psl, NCEP_Iberia_ta850)
#' # In this example, we retain the PCs explaining the 99\% of the variance
#' pca <- prinComp(multigrid, v.exp = c(.95,0.90,.90), keep.orig = FALSE)
#' require(visualizeR)
#' # Original EOF
#' EOF2clim(pca, ind.var = "psl") %>% spatialPlot(backdrop.theme = "coastline")
#' # Rescaled EOF
#' EOF2clim(pca, ind.var = "psl", rescale = FALSE) %>% spatialPlot(backdrop.theme = "coastline")
#' }


EOF2clim <- function(prinCompObj, ind.var, member = 1L, n.eofs = 1L, rescale = TRUE) {
  varNames <- attributes(prinCompObj)$names
  levs <- attributes(prinCompObj)$level
  x <- attributes(prinCompObj)$xCoords
  y <- attributes(prinCompObj)$yCoords
  start <- attr(prinCompObj, "dates_start") %>% head(1)
  end <- attr(prinCompObj, "dates_end") %>% tail(1)
  season <- attr(prinCompObj, "season")
  mu <- attributes(prinCompObj[[ind.var]][[member]][["orig"]])$"scaled:center"
  sigma <- attributes(prinCompObj[[ind.var]][[member]][["orig"]])$"scaled:scale"
  eofs <- prinCompObj[[ind.var]][[member]]$EOFs[, 1:n.eofs, drop = FALSE]
  prinCompObj <- NULL
  # Rescale EOFs
  if (isTRUE(rescale)) {
    eofs <- (eofs * sigma + mu)
  }
  aux <- t(eofs) %>% mat2Dto3Darray(x, y) %>% list()
  # Recover grids structure (EOFS are treated as members, time = 1 like a climatology
  Data <- do.call("abind", c(aux, along = -1)) %>% unname()
  attr(Data, "dimensions") <- c("time", "member", "lat", "lon")
  attr(Data, "climatology:fun") <- "transformeR::prinComp"
  xyCoords = list("x" = x, "y" = y)
  Dates <- list("start" = start, "end" = end)
  attr(Dates, "season") <- season
  start <- end <- NULL
  Variable = list("varName" = paste(varNames[ind.var], "EOFs", sep = "_"), "level" = levs[ind.var])
  out <- list("Variable" = Variable,
              "Data" = Data,
              "xyCoords" = xyCoords,
              "Dates" = Dates)
  return(out)
}
