#' @title Basic arithmetic operations between grids and numbers
#' @description Perform basic arithmetic operations between two or more grids (or factors).
#' @param ... Input grids or factors (numbers). The first element must be a grid, the rest could
#' be either grids or numbers.
#' @param operator Character string of arithmetic simbols ("+", "-", "/", "*"). The length must 
#' be 1 (the same operator is used for all elements in \code{...}) or \code{length(...) - 1} (See Details).
#' @param template grid from which the structure (dates, variable name, etc..) is kept for 
#' the output data (Default is the first grid in \code{...}).
#' @details Lets imagine that elements in \code{...} are A, B and C, where A is a grid and
#' B and C are either grids or numbers. If operator is, for example, "+" (or c("+", "+")), 
#' then the mathematical expression evaluated by \code{gridArithmetics} is: A + B + C. 
#' If operator is, for example, \code{c("+", "/")}, then the expression is (A + B)/C.
#' If we would like to evaluate an expression such as A/(B + C), \code{gridArithmetics}
#' must be applied two times (see examples).
#' 
#' @return A grid
#' @author M. Iturbide
#' @export
#' @examples \donttest{ 
#' require(climate4R.datasets)
#' data("CMIP5_Iberia_pr")
#' data("EOBS_Iberia_pr")
#' cmip5 <- interpGrid(CMIP5_Iberia_pr, getGrid(EOBS_Iberia_pr))
#' A <- cmip5 ; B <- EOBS_Iberia_pr; C <- 100
#' 
#' # To evaluate (A - B) / B * C:
#' relative.delta <- gridArithmetics(A, B, B, C, 
#'                                   operator = c("-", "/", "*"), 
#'                                   template = NULL)
#'                                   
#' # More than one step needed to evaluate an expression of the type B/(A - B):
#' grid1 <- gridArithmetics(A, B, operator = "-")
#' newgrid <- gridArithmetics(B, grid1, operator = "/")
#' }




gridArithmetics <- function(..., operator = "*", template = NULL){
      field.list <- list(...)
      if (length(operator) == 1) operator <- rep(operator, length(field.list) - 1)
      if (length(operator) != (length(field.list) - 1)) stop("Incorrect number of operators")
      indgrid <- which(unlist(lapply(field.list, "isGrid")))
      # indnum <- which(unlist(lapply(field.list, "is.numeric")))
      loc <- FALSE
      if(!isRegular(field.list[[1]])) loc <- TRUE
      for (i in 1:length(indgrid)) {
            field.list[[indgrid[i]]] <- redim(field.list[[indgrid[i]]], 
                                              runtime = TRUE, var = TRUE,
                                              loc = loc)
      }
      if (is.null(template)) template <- field.list[[1]]
      for (i in 1:length(indgrid)) {
            field.list[[indgrid[i]]] <- field.list[[indgrid[i]]]$Data
      }
      aux <- field.list[[1]]
      for (k in 1:(length(field.list) - 1)) {
            if(is.array(field.list[[k + 1]])) dim(field.list[[k + 1]]) <- dim(aux)
            aux <- do.call(operator[k], list(aux, field.list[[k + 1]]))
      }
      dimNames <- getDim(template)
      template$Data <- aux
      attr(template$Data, "dimensions") <- dimNames
      template <- redim(template, drop = TRUE) 
      template <- redim(template,  member = FALSE, loc = loc)
      redim(template, )
      return(template)
}
