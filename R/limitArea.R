#' @title Limit the area in an object returned by function \code{\link{getGrid}} 
#' @param coordinates A list as returned by function \code{\link{getGrid}}.
#' @param latLim Same as \code{lonLim} argument, but for latitude.
#' @param lonLim Vector of length = 2, with minimum and maximum longitude coordinates, in decimal degrees,
#' of the bounding box defining the subset. For single-point subsets, a numeric value with the
#' longitude coordinate. If \code{NULL} (default), no subsetting is performed on the longitude dimension
#' @return same as the input.
#' @author M. Iturbide
#' @family subsetting
#' @export
limitArea <- function(coordinates, lonLim, latLim) {
      args.x <- as.list(c(coordinates[["x"]], attr(coordinates, "resX"))) 
      args.y <- as.list(c(coordinates[["y"]], attr(coordinates, "resY"))) 
      xy <- list(x = do.call("seq", args.x), y = do.call("seq", args.y))
      minlims <- c(lonLim[1], latLim[1])
      maxlims <- c(lonLim[2], latLim[2])
      mincoor <- lapply(1:length(xy), function(i) {
            mini <- which.min(abs(xy[[i]] - minlims[i]))
            if (xy[[i]][mini] > minlims[i]) mini <- mini - 1 
            xy[[i]][mini]
      })
      maxcoor <- lapply(1:length(xy), function(i) {
            maxi <- which.min(abs(xy[[i]] - maxlims[i]))
            if (xy[[i]][maxi] < maxlims[i]) maxi <- maxi + 1 
            xy[[i]][maxi]
      })
      coordinates[["x"]] <- c(mincoor[[1]], maxcoor[[1]])
      coordinates[["y"]] <- c(mincoor[[2]], maxcoor[[2]])
      return(coordinates)
}

#end