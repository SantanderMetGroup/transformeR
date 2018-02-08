#' @title Basic arithmetic operations between grids and numbers
#' @description Perform basic arithmetic operations between two or more grids (or factors).
#' @param ... Input grids or factors (numbers).
#' @param operator Character string of arithmetic simbols ("+", "-", "/", "*").  Variable name of the grid
#' @param template grid from which the grid structure is keeped for the output data (Default is the first
#' grid in "...").
#' @return A grid
#' @author M. Iturbide
#' @export
#' @examples 
#' #' # Maximum July surface temp forecast climatology
#' data("CMIP5_Iberia_pr")
#' data("EOBS_Iberia_pr")
#' cmip5 <- interpGrid(CMIP5_Iberia_pr, getGrid(EOBS_Iberia_pr))
#' relative.delta <- gridArithmetics(cmip5, EOBS_Iberia_pr, 
#'                                   EOBS_Iberia_pr, 
#'                                   100, 
#'                                   operator = c("-", "/", "*"), 
#'                                   template = NULL)




gridArithmetics <- function(..., operator = "*", template = NULL){
      field.list <- list(...)
      if (length(operator) == 1) operator <- rep(operator, length(field.list) - 1)
      if(length(operator) != (length(field.list) - 1)) stop("Incorrect number of operators")
      if (is.null(template)) template <- field.list[[1]]
      indgrid <- which(unlist(lapply(field.list, "isGrid")))
      # indnum <- which(unlist(lapply(field.list, "is.numeric")))
      for (i in 1:length(indgrid)) {
            field.list[[indgrid[i]]] <- field.list[[indgrid[i]]]$Data
      }
      aux <- field.list[[1]]
      for (k in 1:(length(field.list) - 1)) {
            aux <- do.call(operator[k], list(aux, field.list[[k + 1]]))
      }
      dimNames <- getDim(template)
      template$Data <- aux
      attr(template$Data, "dimensions") <- dimNames
      return(template)
}
