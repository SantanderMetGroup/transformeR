
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
  grid.list <- (...)
  if(length(grid.list) > max(which.return)) stop("Wrong value for argument which.return")
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
