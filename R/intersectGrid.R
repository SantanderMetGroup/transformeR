#' @title Intersection of multiple grids
#' @description Takes multiple input grids and crops the overlapping part 
#' @param ... Input grids
#' @param type Character. Options are "temporal" (default) or "spatial".
#' @param which.return Integer of the index to specify which grids in "..." are to be returned.
#' @return The grids indicated in \code{which.return}, encompassing the overlapping time period.
#' @details If type = "members" the function directly loads the resulting objects to the global
#' environment and argument which.return is ignored (see examples).
#' @author M Iturbide
#' @family subsetting
#' @seealso \code{\link{subsetGrid}}
#' @export
#' @examples \donttest{
#' require(climate4R.datasets) 
#' data("EOBS_Iberia_tas")
#' a <- subsetGrid(EOBS_Iberia_tas, lonLim = c(-8,-1), latLim = c(37, 40))
#' b <- subsetGrid(EOBS_Iberia_tas, lonLim = c(-4,3), latLim = c(39, 43))
#' z <- intersectGrid(a, b, type = "spatial", which.return = 1)
#'  data("CFS_Iberia_tas")
#'  data("CFS_Iberia_pr")
#'  intersectGrid(CFS_Iberia_tas, CFS_Iberia_pr, type = "members")
#' }

intersectGrid <- function(..., type = c("temporal", "spatial", "members"), which.return = 1) {
      type <- match.arg(type, choices = c("temporal", "spatial", "members"))
      if (type == "temporal") {
            intersectGrid.time(..., which.return = which.return)
      } else if (type == "spatial") {
            intersectGrid.spatial(..., which.return = which.return)
      } else if (type == "members") {
            intersectGrid.members(...)
      } else {
            stop("Invalid option for argument 'type'.")
      }
}
      
#' @title Temporal intersection of multiple grids
#' @description Takes multiple input grids and crops the overlapping part along time dimension
#' @param ... Input grids
#' @param which.return Integer of the index to specify which grids in "..." are to be returned.
#' @return The grids indicated in \code{which.return}, encompassing the overlapping time period.
#' @importFrom magrittr %<>% %>% 
#' @author M Iturbide
#' @family subsetting
#' @seealso \code{\link{checkDim}}, \code{\link{checkSeason}}, \code{\link{getYearsAsINDEX}}, \code{\link{getSeason}}, for other time dimension helpers
#' @export

intersectGrid.time <- function(..., which.return = 1) {
  grid.list <- list(...)
  if (!isGrid(grid.list[[1]])) grid.list <- unlist(grid.list, recursive = FALSE)
  if (!isGrid(grid.list[[1]])) stop("Wrong input")
  if (length(grid.list) < length(which.return)) stop("Wrong value for argument which.return")
  ref.dates <- lapply(1:length(grid.list), function(x){
    getRefDates(grid.list[[x]]) %>% as.Date(tz = "GMT", format = "%Y-%m-%d")
  }) 
  auxDates <- ref.dates[[1]]
  for (i in 2:length(grid.list)) {
    auxDates <- intersect(auxDates, ref.dates[[i]]) %>% as.Date(origin = "1970-01-01", tz = "GMT", format = "%Y-%m-%d")
  }
  ind <- lapply(ref.dates, function(x) which(is.element(x, auxDates)))
  out <- lapply(1:length(grid.list), function(x) {
    out.l <- subsetDimension(grid.list[[x]], dimension = "time", indices = ind[[x]])
    seas <- getSeason(out.l)
    attr(out.l$Variable, "time_subset") <- "intersectGrid.time"
    attr(out.l$Dates, "season") <- seas
    out.l
  })
  out <- out[which.return]
  if (length(out) == 1) out <- out[[1]]
  return(out)
}



#' @title Temporal intersection of multiple grids
#' @description Takes multiple input grids and crops the overlapping part along time longitude and latitude dimensions
#' @param ... Input grids
#' @param which.return Integer of the index to specify which grids in "..." are to be returned.
#' @return The grids indicated in \code{which.return}, encompassing the overlapping time period.
#' @importFrom magrittr %<>% %>% 
#' @author M Iturbide
#' @family subsetting
#' @seealso \code{\link{checkDim}}, \code{\link{checkSeason}}, \code{\link{getYearsAsINDEX}}, \code{\link{getSeason}}, for other time dimension helpers
#' @export

intersectGrid.spatial <- function(..., which.return = 1) {
      grid.list <- list(...)
      if (!isGrid(grid.list[[1]])) grid.list <- unlist(grid.list, recursive = FALSE)
      if (!isGrid(grid.list[[1]])) stop("Wrong input")
      if (length(grid.list) < length(which.return)) stop("Wrong value for argument which.return")
      # longitudes
      ref.lons <- lapply(1:length(grid.list), function(x){
            getCoordinates(grid.list[[x]])$x
      }) 
      auxLons <- ref.lons[[1]]
      for (i in 2:length(grid.list)) {
            auxLons <- intersect(auxLons, ref.lons[[i]])
      }
      if (length(auxLons) == 0) {
            message("Longitude intersection skipped. There are not intersecting longitudes")
      } else {
            ind <- lapply(ref.lons, function(x) which(is.element(x, auxLons)))
            out <- lapply(1:length(grid.list), function(x) {
                  subsetDimension(grid.list[[x]], dimension = "lon", indices = ind[[x]])
            })
            grid.list <- out
      }
      #latitudes
      ref.lats <- lapply(1:length(grid.list), function(x){
            getCoordinates(grid.list[[x]])$y
      }) 
      auxLats <- ref.lats[[1]]
      for (i in 2:length(grid.list)) {
            auxLats <- intersect(auxLats, ref.lats[[i]])
      }
      if (length(auxLats) == 0) {
            message("Latitude intersection skipped. There are not intersecting latitudes")
            out <- grid.list
      } else {
            ind <- lapply(ref.lats, function(x) which(is.element(x, auxLats)))
            out <- lapply(1:length(grid.list), function(x) {
                  subsetDimension(grid.list[[x]], dimension = "lat", indices = ind[[x]])
            })
      }
      out <- out[which.return]
      if (length(out) == 1) out <- out[[1]]
      return(out)
}

#' @title Subset two or more grids to the common members.
#' @description Subset two or more grids to the common members.
#' @param ... Grids.
#' @return input grids are updated in the global environment.
#' @author M. Iturbide
#' @export


intersectGrid.members <- function(...){
  grids <- list(...) %>% lapply(redim)
  grids.members <- lapply(grids, "[[", "Members")
  members <- do.call("intersect", grids.members)
  if (is.character(members) & length(members) > 0) {
    ind <- lapply(grids.members, function(m){
      lapply(members, function(i) grep(i, m)) %>% unlist
    })
    out <- lapply(1:length(grids), function(x) subsetGrid(grids[[x]], members = ind[[x]]))
    nmes <- as.character(as.list(substitute(list(...)))[-1L])
    if (length(nmes) < length(grids)) nmes <- paste0(nmes, 1:length(grids))
    names(out) <- nmes
    lapply(1:length(out), function(en) .GlobalEnv[[names(out)[en]]] <- out[[en]])
  } else {
    stop("Check input grids. Ensure the member dimension exists.")
  }
}

#end
