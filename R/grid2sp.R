#'@title Grid to sf object
#'@description Convert a grid to an \pkg{sf} object with point geometry
#'@param grid A C4R grid
#'@seealso \code{\link{sp2grid}}
#'@return An \pkg{sf} object (or a list of \pkg{sf} objects if input has multiple members).
#'If the input grid contains members (member dimension) a list of objects is returned
#'(each slot in the list is a member).
#'@details This function keeps the historical name \code{grid2sp} for backward compatibility,
#'but it now uses \pkg{sf} internally.
#'@author M. Iturbide
#'@export
#'@importFrom sf st_as_sf st_crs st_drop_geometry
#'@importFrom abind abind
 
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
                  ord <- order(-co[, 2], co[, 1])
                  coords <- co[ord, , drop = FALSE]
                  aux <- data.frame(aux[ord, , drop = FALSE])
            } else {
                  aux <- redim(grid.m, loc = !isRegular(grid.m), drop = TRUE)$Data
                  if (n.mem > 1) {
                        naind <- lapply(1:n.mem, function(i) which(!is.na(aux[i, ]), arr.ind = TRUE))
                        naind <- Reduce(intersect, naind)
                        aux <- data.frame(t(aux[, naind]))
                  } else {
                        naind <- which(!is.na(aux), arr.ind = TRUE)
                        aux <- data.frame(as.numeric(aux[naind]))
                  }
                  coords <- co[naind, , drop = FALSE]
            }

            prj <- attr(grid$xyCoords, "projection")
            crs <- suppressWarnings(tryCatch(sf::st_crs(prj), error = function(e) NA))
            aux$x <- coords[, 1]
            aux$y <- coords[, 2]
            df <- sf::st_as_sf(aux, coords = c("x", "y"), crs = crs)
            names(df)[seq_len(ncol(sf::st_drop_geometry(df)))] <- as.character(grid[["Dates"]][["start"]])
            df
      })
      if (n.mem == 1) {
            out <- out[[1]]
      } else if (n.mem != 1 & n.time == 1) {
            dfm <- data.frame(lapply(out, function(x) sf::st_drop_geometry(x)))
            if (!is.null(grid[["Members"]])) {
                  names(dfm) <- grid[["Members"]]
            } else {
                  names(dfm) <- paste0("Member_", 1:n.mem)
            }
            out <- out[[1]]
            geom <- sf::st_geometry(out)
            out <- sf::st_as_sf(dfm)
            sf::st_geometry(out) <- geom
      }
      return(out)
}
