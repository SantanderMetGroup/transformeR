#'@title Convert an sf object to a C4R grid
#'@description Convert an \pkg{sf} point layer to a C4R grid
#'@param sp An \pkg{sf} object with point geometry.
#'@param varName Character with the variable name for the resulting grid.
#'@param level Character with the level for the resulting grid.
#'@param dates Named list of the form: list(start = NULL, end = NULL), for the resulting grid.
#'@param metadata Named list of characters with further metadata for the resulting grid.
#'@seealso \code{\link[transformeR]{clim2sgdf}}
#'@return A C4R grid, with the metadata specified by the input arguments.
#'@keywords internal
#'@author M. Iturbide
#'@export
#'@importFrom sf st_coordinates st_drop_geometry st_crs

sp2grid <- function(sp, member = FALSE, varName = NULL, level = NULL, dates = list(start = NULL, end = NULL), metadata = NULL) {
  if (!inherits(sp, "sf")) stop("Input 'sp' must be an sf object")
  grid <- list("Variable" = list("varName" = varName, "level" = level))
  coords <- sf::st_coordinates(sp)
  ord <- order(-coords[, 2], coords[, 1])
  df <- t(sf::st_drop_geometry(sp[ord, , drop = FALSE]))
  x <- sort(unique(coords[, 1]))
  y <- sort(unique(coords[, 2]))
  grid[["Data"]] <- mat2Dto3Darray(df, x, y)
  if (member) attr(grid[["Data"]], "dimensions") <- c("member", "lat", "lon")
  grid[["xyCoords"]] <- list("x" = x, "y" = y)
  attr(grid[["xyCoords"]], "projection") <- sf::st_crs(sp)$input
  if (length(x) > 1) attr(grid[["xyCoords"]], "resX") <- median(diff(x))
  if (length(y) > 1) attr(grid[["xyCoords"]], "resY") <- median(diff(y))
  grid[["Dates"]] <- dates
  if (!is.null(metadata)) {
    for (i in 1:length(metadata)) {
      attr(grid, names(metadata)[i]) <- metadata[[i]]
    }
  }
  return(grid)
} 