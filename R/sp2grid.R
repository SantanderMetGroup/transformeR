#'@title Convert a SpatialGridDataFrame to a C4R grid
#'@description Convert a SpatialGridDataFrame (package \pkg{sp}) to a C4R grid
#'@param sp A SpatialGridDataFrame, as returned by function \code{gdalwarp} (package \pkg{gdalUtils}).
#'(SpatialPointsDataFrame-s not implemented yet).
#'@param varName Character with the variable name for the resulting grid.
#'@param level Character with the level for the resulting grid.
#'@param dates Named list of the form: list(start = NULL, end = NULL), for the resulting grid.
#'@param metadata Named list of characters with furher metadata for the resulting grid.
#'@seealso \code{\link[visualizeR]{clim2sgdf}}
#'@return A C4R grid, with the metadata specified by the input arguments.
#'@keywords internal
#'@author M. Iturbide
#'@export

sp2grid <- function(sp, member = FALSE, varName = NULL, level = NULL, dates = list(start = NULL, end = NULL), metadata = NULL) {
  grid <- list("Variable" = list("varName" = varName, "level" = level))
  df <- t(sp@data)
  x <- seq(sp@grid@cellcentre.offset[1], by = sp@grid@cellsize[1], length.out = sp@grid@cells.dim[1])
  y <- seq(sp@grid@cellcentre.offset[2], by = sp@grid@cellsize[2], length.out = sp@grid@cells.dim[2])
  grid[["Data"]] <- mat2Dto3Darray(df, x, y)
  if (member) attr(grid[["Data"]], "dimensions") <- c("member", "lat", "lon")
  grid[["xyCoords"]] <- list("x" = x, "y" = y)
  attr(grid[["xyCoords"]], "projection") <-  sp@proj4string
  attr(grid[["xyCoords"]], "resX") <- sp@grid@cellsize[1]
  attr(grid[["xyCoords"]], "resY") <- sp@grid@cellsize[2]
  grid[["Dates"]] <- dates
  if (!is.null(metadata)) {
    for (i in 1:length(metadata)) {
      attr(grid, names(metadata)[i]) <-  metadata[[i]]
    }
  }
  return(grid)
}

