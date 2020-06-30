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
    # ## Very slow - unfeasible solution for large grids:
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


#' @title Climatology to SpatialGridDataFrame or SpatialPointsDataFrame
#' @description Convert a climatological climate4R grid to a SpatialGridDataFrame object from package sp. See details.
#' @param clim A climatological grid, as returned by function \code{\link{climatology}}
#' @param set.min Minimum value, as passed by \code{\link[visualizeR]{spatialPlot}}
#' @param set.max Maximum value, as passed by \code{\link[visualizeR]{spatialPlot}}
#' @seealso \code{\link{climatology}}, \code{\link[visualizeR]{spatialPlot}}
#' @return A \pkg{sp} object of the class \code{\link[sp]{SpatialGridDataFrame}}
#' @details This function is intended for internal usage by \code{\link[visualizeR]{spatialPlot}},
#' that accepts all possible arguments that can be passed to \code{\link[sp]{spplot}} for plotting. 
#' However, it may be useful for advanced \pkg{sp} users in different contexts
#' (e.g. for reprojecting via \code{\link[sp]{spTransform}} etc.).
#' 
#' Note that the function can be only applied to \dQuote{climatological} climate4R grids, i.e., grids
#'  whose time dimension is a singleton, typically after the application of function 
#'  \code{\link{climatology}}, although not necessarily so.
#'  
#' @keywords internal
#' @export
#' @author J. Bedia
#' @importFrom sp GridTopology SpatialGridDataFrame
#' @seealso \code{sgdf2clim}, to perform the reverse operation
#' @examples \donttest{
#' require(climate4R.datasets) 
#' data("CFS_Iberia_tas")
#' # Climatology is computed:
#' clim <- climatology(CFS_Iberia_tas, by.member = TRUE)
#' sgdf <- clim2sgdf(clim, NULL, NULL)
#' class(sgdf)
#' sp::spplot(sgdf)
#' }


clim2sgdf <- function(clim, set.min = NULL, set.max = NULL) {
    grid <- redim(clim, member = TRUE, var = FALSE)
    # if (is.null(attr(grid[["Data"]], "climatology:fun"))) {
    #     stop("The input grid is not a climatology: Use function 'climatology' first")
    # }
    if (getShape(clim, dimension = "time") != 1L) {
        stop("The input grid is not a climatology. Suggestion: use function 'climatology' first")
    }
    dimNames <- getDim(grid)
    ## Multigrids are treated as realizations, previously aggregated by members if present
    is.multigrid <- "var" %in% dimNames
    if (is.multigrid) {
        if ("member" %in% dimNames) {
            mem.ind <- grep("member", dimNames)
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
    #############hemen nago!
    if (isRegular(grid)) {
        aux <- vapply(1:n.mem, FUN.VALUE = numeric(le), FUN = function(x) {
            z <- asub(grid[["Data"]], idx = x, dims = mem.ind, drop = TRUE)
            z <- unname(abind(z, along = -1L))
            attr(z, "dimensions") <- c("time", "lat", "lon")
            array3Dto2Dmat(z)
        })
        # Data reordering to match SpatialGrid coordinates
        aux <- data.frame(aux[order(-co[,2], co[,1]), ])
    } else {
        aux <- redim(grid, loc = !isRegular(grid), drop = TRUE)$Data
        if (n.mem > 1) {
            naind <- lapply(1:n.mem, function(i) which(!is.na(aux[i,]), arr.ind = TRUE))
            naind <- Reduce(intersect, naind)
            aux <- data.frame(t(aux[,naind]))
        } else {
            naind <- which(!is.na(aux), arr.ind = TRUE)
            aux <- data.frame(as.numeric(aux[naind]))
        }
    }
    # Set min/max values, if provided
    if (!is.null(set.max)) aux[aux > set.max] <- set.max
    if (!is.null(set.min)) aux[aux < set.min] <- set.min
    # Panel names 
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
    # Defining grid topology -----------------
    aux.grid <- getGrid(grid)
    if (!isRegular(grid)) {
        df <- sp::SpatialPointsDataFrame(co[naind,], aux)
    } else {
        cellcentre.offset <- vapply(aux.grid, FUN = "[", 1L,
                                    FUN.VALUE = numeric(1L))
        cellsize <- vapply(c("resX", "resY"), FUN.VALUE = numeric(1L),
                           FUN = function(x) attr(aux.grid, which = x))
        aux.grid <- getCoordinates(grid)
        cells.dim <- vapply(aux.grid, FUN.VALUE = integer(1L), FUN = "length")
        grd <- sp::GridTopology(c(cellcentre.offset[["x"]],
                                  cellcentre.offset[["y"]]),
                                cellsize,
                                c(cells.dim[["x"]], cells.dim[["y"]]))
        df <- sp::SpatialGridDataFrame(grd, aux)
    }
    return(df)
}




