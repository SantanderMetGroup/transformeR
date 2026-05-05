#' @title Conversion from sf objects to climate4R climatology grids
#' @description Convert an \pkg{sf} object to a C4R climatological grid
#' @param sp An \pkg{sf} object with point geometry.
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
#' @importFrom sf st_coordinates st_drop_geometry st_as_sf st_crs

sgdf2clim <- function(sp, varName = NULL, level = NULL,
                      dates = list(start = NULL, end = NULL),
                      season = NULL,
                      attr.list = NULL) {

    if (!inherits(sp, "sf")) stop("Input 'sp' must be an sf object")
    grid <- list("Variable" = list("varName" = varName, "level" = level))
    coords <- sf::st_coordinates(sp)
    x <- sort(unique(coords[, 1]))
    y <- sort(unique(coords[, 2]))
    ord <- order(-coords[, 2], coords[, 1])
    values <- sf::st_drop_geometry(sp[ord, , drop = FALSE])

    l <- lapply(seq_len(ncol(values)), function(i) {
        arr <- matrix(values[[i]], ncol = length(x), nrow = length(y)) %>%
            t() %>% abind(along = -1L) %>% aperm(perm = c(1, 3, 2)) %>% unname()
        attr(arr, "dimensions") <- c("time", "lat", "lon")
        grid[["Data"]] <- arr
        attr(grid$Data, "climatology:fun") <- "transformeR::sgdf2clim"
        grid[["xyCoords"]] <- list("x" = x, "y" = y)
        attr(grid[["xyCoords"]], "projection") <- sf::st_crs(sp)$input
        if (length(x) > 1) attr(grid[["xyCoords"]], "resX") <- median(diff(x))
        if (length(y) > 1) attr(grid[["xyCoords"]], "resY") <- median(diff(y))
        grid[["Dates"]] <- dates
        attr(grid$Dates, "season") <- season
        return(grid)
    })
    grid <- do.call("bindGrid", c(l, dimension = "member")) 
    if (!is.null(attr.list)) {
        for (i in 1:length(attr.list)) {
            attr(grid, names(attr.list)[i]) <- attr.list[[i]]
        }
    }
    return(grid)
}

#' @title Climatology to sf object
#' @description Convert a climatological climate4R grid to an \pkg{sf} object. See details.
#' @param clim A climatological grid, as returned by function \code{\link{climatology}}
#' @param set.min Minimum value, as passed by \code{\link[visualizeR]{spatialPlot}}
#' @param set.max Maximum value, as passed by \code{\link[visualizeR]{spatialPlot}}
#' @seealso \code{\link{climatology}}, \code{\link[visualizeR]{spatialPlot}}
#' @return An \pkg{sf} object with point geometry.
#' @details This function is intended for internal usage by \code{\link[visualizeR]{spatialPlot}}.
#'
#' Note that the function can be only applied to \dQuote{climatological} climate4R grids, i.e., grids
#'  whose time dimension is a singleton, typically after the application of function
#'  \code{\link{climatology}}, although not necessarily so.
#'
#' @keywords internal
#' @export
#' @author J. Bedia 
#' @seealso \code{sgdf2clim}, to perform the reverse operation 

clim2sgdf <- function(clim, set.min = NULL, set.max = NULL) {
    grid <- redim(clim, member = TRUE, var = FALSE) 
    if (getShape(clim, dimension = "time") != 1L) {
        stop("The input grid is not a climatology. Suggestion: use function 'climatology' first")
    }
    dimNames <- getDim(grid) 
    is.multigrid <- "var" %in% dimNames
    if (is.multigrid) {
        if ("member" %in% dimNames) { 
            n.mem <- getShape(grid, "member")
            if (n.mem > 1) message("NOTE: The multimember mean will be displayed for each variable in the multigrid")
            grid <- suppressMessages(aggregateGrid(grid, aggr.mem = list(FUN = "mean", na.rm = TRUE)))
            dimNames <- getDim(grid)
        }
        attr(grid[["Data"]], "dimensions") <- gsub("var", "member", dimNames)
    }
    grid <- redim(grid, drop = FALSE)
    dimNames <- getDim(grid)
    mem.ind <- grep("member", dimNames)
    n.mem <- getShape(grid, "member")
    co <- getCoordinates(grid)
    if (isRegular(grid)) co <- expand.grid(co$y, co$x)[2:1]
    le <- nrow(co)

    if (isRegular(grid)) {
        aux <- vapply(1:n.mem, FUN.VALUE = numeric(le), FUN = function(x) {
            z <- asub(grid[["Data"]], idx = x, dims = mem.ind, drop = TRUE)
            z <- unname(abind(z, along = -1L))
            attr(z, "dimensions") <- c("time", "lat", "lon")
            array3Dto2Dmat(z)
        })
        ord <- order(-co[, 2], co[, 1])
        aux <- data.frame(aux[ord, , drop = FALSE])
        coords <- co[ord, , drop = FALSE]
    } else {
        aux <- redim(grid, loc = !isRegular(grid), drop = TRUE)$Data
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

    if (!is.null(set.max)) aux[aux > set.max] <- set.max
    if (!is.null(set.min)) aux[aux < set.min] <- set.min

    if (is.multigrid) {
        vname <- attr(grid$Variable, "longname")
        if (!is.null(grid$Variable$level)) {
            auxstr <- paste(vname, grid$Variable$level, sep = "@")
            vname <- gsub("@NA", "", auxstr)
        }
        vname <- gsub("\\s", "_", vname)
        vname <- make.names(vname, unique = TRUE)
    } else {
        vname <- paste0("Member_", 1:n.mem)
    }
    names(aux) <- vname
    aux$x <- coords[, 1]
    aux$y <- coords[, 2]
    sf::st_as_sf(aux, coords = c("x", "y"), crs = sf::st_crs(attr(grid$xyCoords, "projection")))
}