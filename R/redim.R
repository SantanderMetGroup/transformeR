#' @title Complete missing dimensions of Grid objects
#' @description Add missing dimensions to the 'Data' array as singletons. For internal usage mainly.
#' @param obj A regular or irregular (e.g. station data) grid 
#' @param member logical. Add 'member' dimension (default = TRUE)
#' @param runtime logical. Add 'runtime' dimension (default = FALSE)
#' @param var logical. Add 'var' dimension (default = FALSE)
#' @param loc logical. Only if \code{obj} is a stations dataset. If TRUE, the \code{"loc"} 
#' dimension is not replaced by fake \code{"lat"} and \code{"lon"} dimensions (default is FALSE).
#' @param drop logical. Drop dimensions of length = 1 (default = FALSE)
#' @return The same object with all the dimensions (i.e. member, time, loc)
#' @details The function does not handle multigrids (i.e. \code{"var"} dimension).
#'  Thus, multigrids need to be subsetted along \code{"var"} prior to redimensioning via \code{\link{subsetGrid}}.
#' @keywords internal
#' @export
#' @importFrom abind abind adrop
#' @importFrom stats na.omit
#' @importFrom magrittr %<>%
#' @author M. Iturbide, J. Bedia
#' @family internal.helpers
#' @examples
#' data("EOBS_Iberia_tas")
#' getShape(EOBS_Iberia_tas)
#' a <- redim(EOBS_Iberia_tas)
#' getShape(a)
#' b <- redim(EOBS_Iberia_tas, var = TRUE)
#' getShape(b)
#' # This one is probably never needed, but for illustration:
#' y <- redim(EOBS_Iberia_tas, member = FALSE, var = TRUE)
#' getShape(y)
#' # 'drop = TRUE' performs the reverse operation:
#' z <- redim(b, drop = TRUE)
#' getShape(z)
#' 
#' # Example with station data
#' data("VALUE_Iberia_pr")
#' getShape(VALUE_Iberia_pr)
#' # By default, the internal data array is converted to the time.lat-lon structure.
#' # This is a conveninet behaviour for internal usage:
#' getShape(redim(VALUE_Iberia_pr, member = TRUE))
#' # Use the argument 'loc=TRUE' to avoid this behaviour:
#' getShape(redim(VALUE_Iberia_pr, member = TRUE, loc = TRUE))

redim <- function(grid,
                  member = TRUE,
                  runtime = FALSE,
                  var = FALSE,
                  loc = FALSE,
                  drop = FALSE) {
      stopifnot(is.logical(member) | is.logical(runtime) | is.logical(drop))
      dimNames <- getDim(grid)
      if (loc) {
           if (!any(getDim(grid) == "time")) {
                 dimNames <- c(dimNames, "loc")
                 grid$Data <- unname(abind(grid$Data, NULL, along = 2)) 
                 attr(grid$Data, "dimensions") <- dimNames
           } else if (!"loc" %in% dimNames & getShape(grid)["lon"] == 1) {
                  # recover loc dimension  
                  ind <- match("lat", dimNames)
                  dimNames <- c(dimNames[-c(ind,ind + 1)], "loc")
                  grid$Data <- adrop(grid$Data, drop = ind + 1)
                  attr(grid$Data, "dimensions") <- dimNames
           } 
      }
      if (!drop) {
            if ("loc" %in% dimNames & !loc) {
                  # Add singleton 'coordinates dimension' dimension  
                  ind <- match("loc", dimNames)
                  dimNames <- c(dimNames[-ind], "lat", "lon")
                  grid$Data <- unname(abind(grid$Data, NULL, along = ind + 1))
                  attr(grid$Data, "dimensions") <- dimNames
            }
            # Add singleton 'lat' dimension  
            if (!("lat" %in% dimNames) & !loc) {
                  dimNames <- c("lat", dimNames)
                  grid$Data <- unname(abind(grid$Data, NULL, along = 0))
                  attr(grid$Data, "dimensions") <- dimNames
            }
            # Add singleton 'lon' dimension  
            if (!("lon" %in% dimNames) & !loc) {
                  dimNames <- c("lon", dimNames)
                  grid$Data <- unname(abind(grid$Data, NULL, along = 0))
                  attr(grid$Data, "dimensions") <- dimNames
            }
            # Add singleton 'time' dimension 
            if (!("time" %in% dimNames)) {
                  dimNames <- c("time", dimNames)
                  grid$Data <- unname(abind(grid$Data, NULL, along = 0))
                  attr(grid$Data, "dimensions") <- dimNames
            }
            # Add singleton 'member' dimension  
            if (!("member" %in% dimNames) & isTRUE(member)) {
                  dimNames <- c("member", dimNames)
                  grid$Data <- unname(abind(grid$Data, NULL, along = 0))
                  attr(grid$Data, "dimensions") <- dimNames
            }
            # Add singleton 'runtime' dimension to deterministic/obs 
            if (!("runtime" %in% dimNames) & isTRUE(runtime)) {
                  dimNames <- c("runtime", dimNames)
                  grid$Data <- unname(abind(grid$Data, NULL, along = -1))    
                  attr(grid$Data, "dimensions") <- dimNames
            }
            # Add singleton var dimension to deterministic/obs
            if (!("var" %in% dimNames) & isTRUE(var)) {
                  dimNames <- c("var", dimNames)
                  grid$Data <- unname(abind(grid$Data, NULL, along = -1))    
                  attr(grid$Data, "dimensions") <- dimNames
            }
            dimNames <- c( "var", "runtime", "member", "time", "lat", "lon")
            if (isTRUE(loc)) dimNames <- c( "var", "runtime", "member", "time", "loc")
            dimNames.aux <- attr(grid$Data, "dimensions")
            perm <- na.omit(match(dimNames, dimNames.aux))
            grid$Data <- aperm(grid$Data, perm)
            grid[["Data"]] <- unname(grid$Data)
            attr(grid[["Data"]], "dimensions") <- dimNames.aux[perm]
      } else {
            shp <- dim(grid[["Data"]])
            if (1 %in% shp) {
                  dimNames <- dimNames[-(which(!is.na(match(shp, 1))))]
                  grid$Data <- drop(grid$Data)
                  attr(grid$Data, "dimensions") <- dimNames
                  if ("lat" %in% dimNames & !("lon" %in% dimNames)) {
                        attr(grid$Data, "dimensions")[attr(grid$Data, "dimensions") == "lat"] <- "loc"
                  }
            }
      }
      if (is.null(dim(grid$Data))) grid$Data %<>% as.array()
      return(grid) 
}
#End

