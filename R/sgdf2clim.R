#' @title Conversion from SpatialGridDataFrame-class objects to climate4R climatology grids
#' @description Convert a SpatialGridDataFrame (package \pkg{sp}) to a C4R climatological grid
#' @param sp A SpatialGridDataFrame-class object, as returned by function \code{gdalwarp} (package \pkg{gdalUtils}).
#'(SpatialPointsDataFrame not implemented yet).
#' @param varName Character with the variable name for the resulting grid.
#' @param level Character with the level for the resulting grid.
#' @param dates Named list of the form: list(start = NULL, end = NULL), for the resulting grid. Dates as POISXct vectors.
#' @param seasons Integer vector defining the grid season
#' @param attr.list Key-value list of additional attributes to be included in the output grid as metadata.
#' @seealso \code{\link[transformeR]{clim2sgdf}}, that performs the inverse operation
#' @return A C4R climatological grid (i.e.: time dimension is a singleton), with the metadata specified by the input arguments.
#' @keywords internal
#' @author M. Iturbide, J. Bedia
#' @export
#' @importFrom abind abind
#' @importFrom magrittr %>%

sgdf2clim <- function(sp, member = FALSE, varName = NULL, level = NULL,
                      dates = list(start = NULL, end = NULL),
                      season = NULL,
                      attr.list = NULL) {
    
    grid <- list("Variable" = list("varName" = varName, "level" = level))
    
    x <- seq(sp@grid@cellcentre.offset[1],
             by = sp@grid@cellsize[1],
             length.out = sp@grid@cells.dim[1])
    y <- seq(sp@grid@cellcentre.offset[2],
             by = sp@grid@cellsize[2],
             length.out = sp@grid@cells.dim[2])
    
    # co <- expand.grid(x, rev(y)) ## Orden en el que estan los datos
    # co.ref <- expand.grid(y, x)[,2:1] ## Orden en el que mat2Dto3Darray los espera
    # ## Very slow - unfeasible solution:
    # coi <- complex(length.out = nrow(co), real = co[,1], imaginary = co[,2])
    # co.refi <- complex(length.out = nrow(co.ref), real = co.ref[,1], imaginary = co.ref[,2])
    # ind <- vector("integer", length(coi))
    # for (i in 1:length(coi)) {
    #     message(i)
    #     ind[i] <- which((coi - co.refi[i]) == 0)
    # }
    
    l <- lapply(1:length(x), function(i) rev(seq(i, by = length(x), length.out = length(y))))
    ind <- do.call("c", l)
    l <- lapply(1:ncol(sp@data), function(i) {
        aux <- sp@data[ind, i, drop = TRUE] # Re-ordered vector following mat2Dto3Darray ordering
        arr <- matrix(aux,
                      ncol = length(x),
                      nrow = length(y)) %>% t() %>% abind(along = -1L) %>% aperm(perm = c(1,3,2))
        attr(arr, "dimensions") <- c("time", "lat", "lon")
        grid[["Data"]] <- arr
        grid[["xyCoords"]] <- list("x" = x, "y" = y)
        attr(grid[["xyCoords"]], "projection") <-  sp@proj4string
        attr(grid[["xyCoords"]], "resX") <- sp@grid@cellsize[1]
        attr(grid[["xyCoords"]], "resY") <- sp@grid@cellsize[2]
        grid[["Dates"]] <- dates
        attr(grid$Dates, "season") <- season
        return(grid)
    })
    grid <- do.call("bindGrid", c(l, dimension = "member"))
    l <- NULL
    if (!is.null(attr.list)) {
        for (i in 1:length(attr.list)) {
            attr(grid, names(attr.list)[i]) <- attr.list[[i]]
        }
    }
    return(grid)
}

