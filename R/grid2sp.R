#'@title Grid to SpatialGridDataFrame or SpatialPointsDataFrame
#'@description Convert a grid to a SpatialGridDataFrame object from package sp
#'@param grid A C4R grid
#'@seealso \code{\link{sp2grid}}
#'@return A \pkg{sp} object of the class \code{\link[sp]{SpatialGridDataFrame}} or 
#'\code{\link[sp]{SpatialPointsDataFrame}} (Depending on the input grid).
#'If the inpurt grid contains members (member dimension) a list of Spatial*
#'objects is returned (each slot in the list is a member). 
#'@details This function was built based on function \code{\link[transformeR]{clim2sgdf}}
#'@author M. Iturbide
#'@export
#'@importFrom sp GridTopology SpatialGridDataFrame is.projected CRS
#'@importFrom abind abind
#' @examples \donttest{
#' require(climate4R.datasets)
#' library(sp)
#' # Climatologies of different members:
#' data("CFS_Iberia_tas")
#' x <- grid2sp(climatology(CFS_Iberia_tas))
#' plot(x[1]) # Plot the first member
#' 
#' # Daily data:
#' x <- grid2sp(CFS_Iberia_tas)
#' plot(x[[1]][1]) # Plot the first day of the first member
#' 
#' data("EOBS_Iberia_tas")
#' x <- grid2sp(EOBS_Iberia_tas)
#' plot(x[1]) # Plot the first day
#' }

grid2sp <- function(grid) {
      grid <- redim(grid, drop = FALSE)
      dimNames <- getDim(grid)
      time.ind <- grep("member", dimNames)
      n.mem <- getShape(grid, "member")
      n.time <- getShape(grid, "time")
      co <- getCoordinates(grid)
      if (isRegular(grid)) co <- expand.grid(co$y, co$x)[2:1]
      le <- nrow(co)
      out <- lapply(1:n.mem, function(m) {
            grid.m <- redim(subsetGrid(grid, members = m), member = FALSE)
            
            if (isRegular(grid.m)) {
                  aux <- vapply(1:n.time, FUN.VALUE = numeric(le), FUN = function(x) {
                        z <- asub(grid.m[["Data"]], idx = x, dims = 1, drop = TRUE)
                        z <- unname(abind(z, along = -1L))
                        attr(z, "dimensions") <- c("time", "lat", "lon")
                        array3Dto2Dmat(z)
                  })
                  # Data reordering to match SpatialGrid coordinates
                  aux <- data.frame(aux[order(-co[,2], co[,1]), ])
            } else {
                  aux <- redim(grid.m, loc = !isRegular(grid.m), drop = TRUE)$Data
                  if (n.mem > 1) {
                        naind <- lapply(1:n.mem, function(i) which(!is.na(aux[i,]), arr.ind = TRUE))
                        naind <- Reduce(intersect, naind)
                        aux <- data.frame(t(aux[,naind]))
                  } else {
                        naind <- which(!is.na(aux), arr.ind = TRUE)
                        aux <- data.frame(as.numeric(aux[naind]))
                  }
            }
            # Defining grid topology -----------------
            aux.grid <- getGrid(grid)
            if (!isRegular(grid)) {
                  df <- sp::SpatialPointsDataFrame(co[naind,], aux, match.ID = FALSE)
            } else {
                  cellcentre.offset <- vapply(aux.grid, FUN = "[", 1L, FUN.VALUE = numeric(1L))
                  cellsize <- vapply(c("resX", "resY"), FUN.VALUE = numeric(1L), FUN = function(x) attr(aux.grid, which = x))
                  aux.grid <- getCoordinates(grid)
                  cells.dim <- vapply(aux.grid, FUN.VALUE = integer(1L), FUN = "length")
                  grd <- sp::GridTopology(c(cellcentre.offset[["x"]], cellcentre.offset[["y"]]), cellsize, c(cells.dim[["x"]], cells.dim[["y"]]))
                  df <- sp::SpatialGridDataFrame(grd, aux)
            }
            prj <- attr(grid$xyCoords, "projection")
            if (!is.null(prj)) {
                  testprj <- tryCatch({CRS(prj)}, error = function(e) {NULL})
                  if (!is.null(testprj)) sp::proj4string(df) <- prj
            }
            names(df@data) <- grid[["Dates"]][["start"]]
            df
      })
      if (n.mem == 1) {
            out <- out[[1]] 
      } else if (n.mem != 1 & n.time == 1) {
            dfm <- data.frame(lapply(out, function(x) x@data))
            if (!is.null(grid[["Members"]])) {
                  names(dfm) <- grid[["Members"]]
            } else {
                  names(dfm) <- paste0("Member_", 1:n.mem)
            }
            out <- out[[1]]
            out@data <- dfm
      }
      return(out)
}
