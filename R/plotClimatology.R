#     plotClimatology.R Lattice plot methods for climatological grids
#
#     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.



#' @title Lattice plot methods for climatological grids
#' @description A wrapper for the lattice (trellis) plot methods for spatial data in \code{sp::spplot}
#' @param grid Input grid
#' @param backdrop.theme Reference geographical lines to be added to the plot. See Details. 
#' @param set.min Numeric value indicating an absolute minimum value (default to \code{NULL}). All grid values below this are mapped to \code{set.min}. See details.
#' @param set.max Same as \code{set.min} argument, but to force a ceiling. 
#' @param ... Further arguments passed to \code{spplot}
#' @details The function applies the \code{\link[sp]{spplot}} method after conversion of the climatological map(s) to a
#'  \code{SpatialGridDataFrame}.
#'  
#'  The \code{set.min} and \code{set.max} options are useful in order to preserve adequate ranges for map representation,
#'   avoiding the influence of extreme values. Note that this is different than setting a range of values with an 
#'   interval using the \code{at} argument. The latter choice, that overrides \code{set.min} and \code{set.max},
#'    leaves blank grid points for outlying values.
#'  
#'  \strong{Multigrids}
#'  
#'  Multigrids of climatologies can be created using \code{makeMultiGrid} 
#'  for trellis visualization of different variables, or for instance, for the comparison of
#'  raw and corrected/downscaled scenarios side to side. In case of multimember multigrids, 
#'  the function will internally compute the ensemble mean of each variable in the multigrid
#'   for representation (with a message).
#'  
#'  \strong{Backdrop theme}
#'  
#'  Current implemented options are \code{"none"} and \code{"coastline"}, which contains
#'  a simplied vector theme delineating the world coastlines. Any other themes can be introduced
#'  by the user using the \code{sp.layout} options in \code{spplot}.
#'  
#'  \strong{Controlling graphical parameters}
#'  
#'  Many different aspects of the map can be controlled passing the relevant arguments to 
#'  \code{spplot}. Fine control of graphical parameters for the trellis display can
#'  be also controlled using \code{\link[lattice]{trellis.par.set}}.
#
#' @return As spplot, \code{plotClimatology} returns a lattice plot of class \dQuote{trellis}. 
#'  If you fail to \dQuote{see} it, explicitly call \code{print(plotClimatology(...))}.
#' 
#' @references \itemize{
#' \item Bivand, R.S., Pebesma, E.J., Gomez-Rubio, V., 2013. Applied Spatial Data Analysis with R, 2nd ed, useR! Springer, NY.
#' \item For some graticulate customization examples, visit the \emph{sp Gallery}: \url{https://edzer.github.io/sp/}
#' }
#'    
#' @importFrom abind abind
#' @importFrom sp spplot SpatialGridDataFrame SpatialPointsDataFrame GridTopology
#' @importFrom grDevices colorRampPalette
#' 
#' @export
#' 
#' @author J. Bedia
#' @seealso \code{\link{climatology}} for details on climatology calculation.
#'  \code{\link{map.stippling}}, for adding a custom point layer on top of the map.
#' Also see \code{\link[sp]{spplot}} in package \pkg{sp} for further information on plotting capabilities and options
#' @examples 
#' data("CFS_Iberia_tas")
#' # Climatology is computed:
#' clim <- climatology(CFS_Iberia_tas, by.member = TRUE)
#' plotClimatology(clim)
#' # Geographical lines can be added using the argument 'backdrop.theme':
#' plotClimatology(clim, backdrop.theme = "coastline")
#' 
#' # Further arguments can be passed to 'spplot'...
#' 
#' # ... a subset of members to be displayed, using 'zcol':
#' plotClimatology(clim,
#'                 backdrop.theme = "coastline",
#'                 zcol = 1:4)
#'                 
#' # ... regional focuses (e.g. the Iberian Peninsula). 
#' plotClimatology(clim,
#'                 backdrop.theme = "countries",
#'                 xlim = c(-10,5), ylim = c(35,44),
#'                 zcol = 1:4,
#'                 scales = list(draw = TRUE))
#' 
#' # Changing the default color palette and ranges:
#' plotClimatology(clim,
#'                 backdrop.theme = "coastline",
#'                 zcol = 1:4,
#'                 col.regions = cm.colors(27), at = seq(10,37,1))
#'                 
#' # For ensemble means climatology should be called with 'by.member' set to FALSE:
#' clim <- climatology(CFS_Iberia_tas, by.member = FALSE)
#' 
#' # Adding contours to the plot is direct with argument 'contour':
#' plotClimatology(clim,
#'                 scales = list(draw = TRUE),
#'                 contour = TRUE,
#'                 main = "tas Predictions July Ensemble Mean")
#'                 
#'                 
#' ## Example of multigrid plotting
#' data("NCEP_Iberia_psl")
#' ## Winter data are split into monthly climatologies
#' monthly.clim.grids <- lapply(getSeason(NCEP_Iberia_psl), function(x) {
#'       climatology(subsetGrid(NCEP_Iberia_psl, season = x))
#' })
#' ## Skip the temporal checks, as grids correspond to different time slices
#' mg <- do.call("makeMultiGrid",
#'               c(monthly.clim.grids, skip.temporal.check = TRUE))
#'               ## We change the panel names
#' plotClimatology(mg, 
#'                 backdrop.theme = "coastline",
#'                 names.attr = c("DEC","JAN","FEB"),
#'                 main = "Mean PSL climatology 1991-2010",
#'                 scales = list(draw = TRUE))                
#' 
#' # Station data:
#' data("VALUE_Iberia_tp")
#' plotClimatology(climatology(VALUE_Iberia_tp), backdrop.theme = "countries")



plotClimatology <- function(grid, backdrop.theme = "none", set.min = NULL, set.max = NULL, ...) {
    arg.list <- list(...)
    bt <- match.arg(backdrop.theme, choices = c("none", "coastline", "countries"))
    if (!is.null(set.min) && !is.numeric(set.min)) stop("Invalid 'set.min' value")
    if (!is.null(set.min) && !is.numeric(set.max)) stop("Invalid 'set.max' value")
    df <- clim2sgdf(clim = grid, set.min, set.max)
    ## Backdrop theme 
    if (bt != "none") {
        uri <- switch(bt,
                      "coastline" = system.file("coastline.rda", package = "transformeR"),
                      "countries" = system.file("countries.rda", package = "transformeR"))
        load(uri)      
        if (is.null(arg.list[["sp.layout"]])) {
            arg.list[["sp.layout"]] <- list(l1)
        } else {
            arg.list[["sp.layout"]][[length(arg.list[["sp.layout"]]) + 1]] <- l1
        } 
    }
    ## Default colorbar 
    if (is.null(arg.list[["col.regions"]])) {
        jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                         "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
        arg.list[["col.regions"]] <- jet.colors(101)
    }
    ## Other args 
    arg.list[["obj"]] <- df
    arg.list[["asp"]] <- 1
    do.call("spplot", arg.list)
}      

#'@title Climatology to SpatialGridDataFrame or SpatialPointsDataFrame
#'@description Convert a climatological grid to a SpatialGridDataFrame object from package sp
#'@param clim A climatological grid, as returned by function \code{\link{climatology}}
#'@param set.min Minimum value, as passed by \code{plotClimatology}
#'@param set.max Maximum value, as passed by \code{plotClimatology}
#'@seealso \code{\link{climatology}}, \code{\link{plotClimatology}}
#'@return A \pkg{sp} object of the class \code{\link[sp]{SpatialGridDataFrame}}
#'@details This function is intended for internal usage by \code{\link{plotClimatology}},
#'that accepts all possible arguments that can be passed to \code{\link[sp]{spplot}} for plotting. 
#' However, it may be useful for advanced \pkg{sp} users in different contexts
#' (e.g. for reprojecting via \code{\link[sp]{spTransform}} etc.)
#'@keywords internal
#'@author J. Bedia
#'@export
#'@importFrom sp GridTopology SpatialGridDataFrame
#'@examples 
#' data("CFS_Iberia_tas")
#' # Climatology is computed:
#' clim <- climatology(CFS_Iberia_tas, by.member = TRUE)
#' sgdf <- clim2sgdf(clim, NULL, NULL)
#' class(sgdf)



clim2sgdf <- function(clim, set.min, set.max) {
    grid <- clim
    if (is.null(attr(grid[["Data"]], "climatology:fun"))) {
        stop("The input grid is not a climatology: Use function 'climatology' first")
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
        aux <- redim(grid, loc = !isRegular(grid), drop = T)$Data
        naind <- which(!is.na(aux))
        aux <- data.frame(as.numeric(aux[naind]))
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
        cellcentre.offset <- vapply(aux.grid, FUN = "[", 1L, FUN.VALUE = numeric(1L))
        cellsize <- vapply(c("resX", "resY"), FUN.VALUE = numeric(1L), FUN = function(x) attr(aux.grid, which = x))
        aux.grid <- getCoordinates(grid)
        cells.dim <- vapply(aux.grid, FUN.VALUE = integer(1L), FUN = "length")
        grd <- sp::GridTopology(c(cellcentre.offset[["x"]], cellcentre.offset[["y"]]), cellsize, c(cells.dim[["x"]], cells.dim[["y"]]))
        df <- sp::SpatialGridDataFrame(grd, aux)
    }
    return(df)
}




#' @title Climatological map stippling
#' @description Create a points panel layout to add to \code{plotClimatology}.
#' Typically needed to stipple significant points in climatologies, or other types of coordinates 
#' @param clim A climatology. This can be for instance a verification climatology as produced by \code{\link{easyVeri2grid}}.
#'  This often contains p-values, but not necessarily.
#' @param condition Inequality operator to be applied considering the given \code{threshold}.
#' \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than. Default to \code{"LT"} (see the rationale in the next argument).
#' @param threshold Reference threshold value to specify stippling points. Default to \code{0.05}, 
#' in combination with \code{condition = "LT"}, as tipically used for stippling statistically significant
#' values (p-values).
#' @param ... Further optional style arguments (see the examples). 
#' @return A list with a \code{SpatialPoints} object, 
#' along with optional style arguments like \code{col}, \code{pch}, \code{cex} etc., 
#' to be passed to the \code{sp.layout} argument in \code{plotClimatology}.
#' @export
#' @importFrom sp SpatialPoints
#' @details The function generates a \code{"sp.points"} layout list. Further formatting arguments can be passed here.
#'  For further details and examples see the help of \code{\link[sp]{spplot}}.
#' @seealso \code{\link{plotClimatology}}, to which its output is passed.
#'  \code{\link{map.lines}}, for further map customizations.
#' @author J. Bedia
#' @examples
#' data("CFS_Iberia_tas")
#' p90clim <- climatology(CFS_Iberia_tas,
#'                        by.member = FALSE,
#'                        clim.fun = list("FUN" = quantile, prob = .9))
#' plotClimatology(p90clim, backdrop.theme = "coastline",
#'                 main = "CFSv2 Ensemble mean Tmean 90th percentile (July 2001)")
#' 
#' # We want to highlight the grid points with a 90th percentile > 25.5 degrees, 
#' # on top of the Tmean model climatology:
#' pts <- map.stippling(p90clim, threshold = 15.5, condition = "GT")
#' plotClimatology(climatology(CFS_Iberia_tas),
#'                 backdrop.theme = "coastline",
#'                 sp.layout = list(pts))
#' 
#' # Some useful parameters that can be passed to the layout list:
#' pts <- map.stippling(p90clim, threshold = 15.5, condition = "GT",
#'                      pch = 19, # dots instead of default crosses
#'                      col = "black", # black dots
#'                      cex = .1) # point expansion factor (to make them very small)
#' plotClimatology(climatology(CFS_Iberia_tas),
#'                 backdrop.theme = "coastline",
#'                 sp.layout = list(pts))
#' 
#' # Suppose we want the stippling just in the first and fifth panels, for instance:
#' pts <- map.stippling(p90clim, threshold = 15.5, condition = "GT",
#'                      pch = 19, col = "black", cex = .1,
#'                      which = c(1, 5)) # which controls in which panel(s) the points are displayed
#' plotClimatology(climatology(CFS_Iberia_tas),
#'                 backdrop.theme = "coastline",
#'                 sp.layout = list(pts))


map.stippling <- function(clim, threshold = 0.05, condition = "LT", ...) {
    if (!("climatology:fun" %in% names(attributes(clim$Data)))) {
        stop("Input grid was not recognized as a climatology")
    }
    condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
    ineq <- switch(condition,
                   "GT" = ">",
                   "GE" = ">=",
                   "LT" = "<",
                   "LE" = "<=")
    arg.list <- list(...)
    aux <- array3Dto2Dmat(clim$Data)[1, ]
    ind <- eval(parse(text = paste("which(aux", ineq, "threshold)")))
    coords <- as.matrix(expand.grid(clim$xyCoords$y, clim$xyCoords$x)[2:1][ind, ])
    if (nrow(coords) == 0) stop("None of the grid points is below the specified threshold")
    do.call("list", c("sp.points", SpatialPoints(coords), arg.list))
}


#' @title Add lines and polygons to climatological maps
#' @description Draws user-defined lines or polygons on top of climatological maps.
#' @param lonLim A numeric vector of length 2, with minimum and maximum longitude coordinates (in grid units),
#'  of the rectangle to be drawn. 
#' @param latLim Same as \code{lonLim}, but for the selection of the latitudinal range.
#' @param coords Optional. 2-column numeric matrix with vertex coordinates (1 point by row). 
#' Note that row order matters in order to define the drawing direction.
#'  Also bear in mind that first point (row) should equal last coordinates (row) in the case of closed polygonal areas.
#' @param ... Further optional style arguments (see the examples). 
#' @return A list with a \code{SpatialLines} object, 
#' along with optional style arguments like \code{col}, \code{lty}, \code{lwd} etc., 
#' to be passed to the \code{sp.layout} argument in \code{plotClimatology} 
#' @details The function internally transforms the inputs into \code{\link[sp]{Line}} class objects,
#'  so the displayed outputs are not actually polygons in a formal sense (they can not be filled, for instance).
#'  The purpose of the function is just to highlight specific areas within climatological maps (typically rectangular
#'   windows, but any other shapes like for instance storm tracks can be flexibly specified using \code{coords}). 
#' @author J Bedia
#' @importFrom magrittr %>%
#' @importFrom sp Line Lines SpatialLines
#' @export
#' @seealso \code{\link{plotClimatology}}, to which its output is passed.
#'  \code{\link{map.stippling}}, for further map customizations.
#' @examples 
#' data("CFS_Iberia_tas")
#' # Define a rectangular window centered on the Iberian Peninsula
#' iberia <- map.lines(lonLim = c(-10,3.5), latLim = c(36,43))
#' plotClimatology(climatology(CFS_Iberia_tas), backdrop.theme = "coastline",
#'                 sp.layout = list(iberia))
#' 
#' # Some customization options (awful, yes, but just for illustration):
#' iberia <- map.lines(lonLim = c(-10,3.5), latLim = c(36,44),
#'                     lwd = 3, # line width
#'                     col = "purple", # line color
#'                     lty = 2) # line type
#' plotClimatology(climatology(CFS_Iberia_tas, by.member = FALSE), backdrop.theme = "coastline",
#'                 sp.layout = list(iberia))
#' 
#' # Another window over the Alps
#' alps <- map.lines(lonLim = c(4,16), latLim = c(45,49),
#'                   lwd = 3,
#'                   col = "red") 
#' plotClimatology(climatology(CFS_Iberia_tas, by.member = FALSE), backdrop.theme = "coastline",
#'                 sp.layout = list(iberia, alps))
#' 
#' # Adding a line (real data of a storm-track imported from a csv file)
#' # Source: http://www.europeanwindstorms.org/
#' 
#' dat <- url("http://www.europeanwindstorms.org/repository/Jeanette/Jeanette_track.csv")
#' custom.coords <- read.csv(dat, header = FALSE)[ ,5:4]
#' storm <- map.lines(coords = custom.coords,
#'                    lwd = 3,
#'                    col = "red") 
#' plotClimatology(climatology(CFS_Iberia_tas, by.member = FALSE), backdrop.theme = "coastline",
#'                 sp.layout = list(storm), # Add storm track
#'                 scales = list(draw = TRUE)) # Add coordinate axes

map.lines <- function(lonLim = NULL, latLim = NULL, coords = NULL, ...) {
    if (is.null(lonLim) && is.null(latLim) && is.null(coords)) stop("Undefined polygon coordinates")
    if (is.null(lonLim) && !is.null(latLim) || !is.null(lonLim) && is.null(latLim)) stop("Undefined 'lonLim' or 'latLim'")
    arg.list <- list(...)
    spLines <- if (is.null(coords)) {
        coords <- as.matrix(expand.grid(lonLim, latLim))
        coords[1:2, ] <- coords[2:1, ]
        coords <- rbind(coords, coords[1,])
        Line(coords) %>% list() %>% Lines(ID = "id") %>% list() %>% SpatialLines()
    } else {
        lapply(1:(nrow(coords) - 1), function(x) {
            Line(coords[x:(x + 1),])
        }) %>% Lines(ID = "id") %>% list() %>% SpatialLines()
    }    
    do.call("list", c("sp.lines", spLines, arg.list))
}
