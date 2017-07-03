#' @title Complete missing dimensions of Grid or Station objects
#' @description Add missing dimensions to the 'Data' array as singletons. For internal usage mainly.
#' @param obj A grid or station data
#' @param member logical. Add 'member' dimension (default = TRUE)
#' @param runtime logical. Add 'runtime' dimension (default = FALSE)
#' @param var logical. Add 'var' dimension (default = FALSE)
#' @param station logical. Only if \code{obj} is a stations dataset. If TRUE, the \code{"station"} 
#' dimension is not replaced by fake \code{"lat"} and \code{"lon"} dimensions (default is FALSE).
#' @param drop logical. Drop dimensions of length = 1 (default = FALSE)
#' @return The same object with all the dimensions (i.e. member, time, station)
#' @details The function does not handle multigrids (i.e. \code{"var"} dimension).
#'  Thus, multigrids need to be subsetted along \code{"var"} prior to redimensioning via \code{\link{subsetGrid}}.
#' @keywords internal
#' @export
#' @importFrom abind abind
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

redim <- function(obj,
                  member = TRUE,
                  runtime = FALSE,
                  var = FALSE,
                  station = FALSE,
                  drop = FALSE) {
      stopifnot(is.logical(member) | is.logical(runtime) | is.logical(drop))
      dimNames <- getDim(obj)
      if (!drop) {
            if ("station" %in% dimNames & !station) {
                  # Add singleton 'coordinates dimension' dimension  
                  ind <- match("station", dimNames)
                  dimNames <- c(dimNames[-ind], "lat", "lon")
                  obj$Data <- unname(abind(obj$Data, NULL, along = ind + 1))
                  attr(obj$Data, "dimensions") <- dimNames
            }
            # Add singleton 'lat' dimension  
            if (!("lat" %in% dimNames) & !station) {
                  dimNames <- c("lat", dimNames)
                  obj$Data <- unname(abind(obj$Data, NULL, along = 0))
                  attr(obj$Data, "dimensions") <- dimNames
            }
            # Add singleton 'lon' dimension  
            if (!("lon" %in% dimNames) & !station) {
                  dimNames <- c("lon", dimNames)
                  obj$Data <- unname(abind(obj$Data, NULL, along = 0))
                  attr(obj$Data, "dimensions") <- dimNames
            }
            # Add singleton 'time' dimension 
            if (!("time" %in% dimNames)) {
                  dimNames <- c("time", dimNames)
                  obj$Data <- unname(abind(obj$Data, NULL, along = 0))
                  attr(obj$Data, "dimensions") <- dimNames
            }
            # Add singleton 'member' dimension  
            if (!("member" %in% dimNames) & isTRUE(member)) {
                  dimNames <- c("member", dimNames)
                  obj$Data <- unname(abind(obj$Data, NULL, along = 0))
                  attr(obj$Data, "dimensions") <- dimNames
            }
            # Add singleton 'runtime' dimension to deterministic/obs 
            if (!("runtime" %in% dimNames) & isTRUE(runtime)) {
                  dimNames <- c("runtime", dimNames)
                  obj$Data <- unname(abind(obj$Data, NULL, along = -1))    
                  attr(obj$Data, "dimensions") <- dimNames
            }
            # Add singleton var dimension to deterministic/obs
            if (!("var" %in% dimNames) & isTRUE(var)) {
                  dimNames <- c("var", dimNames)
                  obj$Data <- unname(abind(obj$Data, NULL, along = -1))    
                  attr(obj$Data, "dimensions") <- dimNames
            }
            dimNames <- c( "var", "runtime", "member", "time", "lat", "lon")
            if (isTRUE(station)) dimNames <- c( "var", "runtime", "member", "time", "station")
            dimNames.aux <- attr(obj$Data, "dimensions")
            perm <- na.omit(match(dimNames, dimNames.aux))
            obj$Data <- aperm(obj$Data, perm)
            obj[["Data"]] <- unname(obj$Data)
            attr(obj[["Data"]], "dimensions") <- dimNames.aux[perm]
      } else {
            shp <- dim(obj[["Data"]])
            if (1 %in% shp) {
                  dimNames <- dimNames[-(which(!is.na(match(shp, 1))))]
                  obj$Data <- drop(obj$Data)
                  attr(obj$Data, "dimensions") <- dimNames
                  if ("lat" %in% dimNames & !("lon" %in% dimNames)) {
                        attr(obj$Data, "dimensions")[attr(obj$Data, "dimensions") == "lat"] <- "station"
                  }
            }
      }
      if (is.null(dim(obj$Data))) obj$Data %<>% as.array()
      return(obj) 
}
#End

