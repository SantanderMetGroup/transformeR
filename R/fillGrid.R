#' @title Fill missing dates and extend the latitude-longitude domain of a given grid.
#' @description fill with NA missing dates in grids and station datasets
#' @param grid grid or station data. The lonLim and latLim arguments are only valid for gridded data.
#' @param tz Optional. Time zone. See Details in \code{\link{fillGridDates}}.
#' @param lonLim Optional. A vector with the minimum and maximum longitude boundaries to be filled with NAs. Default to lonLim = c(-180,180).
#' @param latLim Optional. A vector with the minimum and maximum latitude boundaries to be filled with NAs. Default to latLim = c(-90,90).
#' @return A grid filled with NAs in the previously missing date positions and/or in the latitude-longitude domain indicated.
#' @author J. Baño-Medina
#' @export
#' @examples \donttest{
#' require(climate4R.datasets) 
#' require(visualizeR)
#' grid <- get(data("NCEP_Iberia_psl"))
#' spatialPlot(climatology(grid), backdrop.theme = "coastline")
#' grid <- fillGrid(grid, tz = NULL, lonLim = c(-180,180), latLim = c(-90,90))
#' spatialPlot(climatology(grid), backdrop.theme = "coastline")
#' }
fillGrid <- function(grid, tz = "", lonLim = c(-180,180), latLim = c(-90,90)) {
  if (!is.null(tz)) grid <- fillGridDates(tz = tz)
  if (!is.null(lonLim)) grid <- fillGridSpatial(grid, lonLim = lonLim, latLim = latLim)
  return(grid)
}


#' @title Fill missing dates
#' @description fill with NA missing dates in grids and station datasets
#' @param grid grid or station data
#' @param lonLim Optional. A vector with the minimum and maximum longitude boundaries to be filled with NAs. Default to lonLim = c(-180,180).
#' @param latLim Optional. A vector with the minimum and maximum latitude boundaries to be filled with NAs. Default to latLim = c(-90,90).
#' @return A grid filled with NAs in the previously missing date positions
#' @author J. Baño-Medina
#' @export

fillGridSpatial <- function(grid, lonLim = c(-180,180), latLim = c(-90,90)) {
  resX <- attr(grid$xyCoords,"resX")
  resY <- attr(grid$xyCoords,"resY")
  lonLimGrid <- c(grid$xyCoords$x[1],grid$xyCoords$x[length(grid$xyCoords$x)])
  latLimGrid <- c(grid$xyCoords$y[1],grid$xyCoords$y[length(grid$xyCoords$y)])
  
  la <- rev(seq(lonLimGrid[1],lonLim[1],-resX)[-1])
  a <- array(dim = c(getShape(grid,"time"),length(grid$xyCoords$y),length(la)))
  lb <- seq(lonLimGrid[2],lonLim[2],resX)[-1]
  b <- array(dim = c(getShape(grid,"time"),length(grid$xyCoords$y),length(lb)))
  lonLimFinal <- c(la,grid$xyCoords$x,lb) %>% unique()
  
  lc <- rev(seq(latLimGrid[1],latLim[1],-resY)[-1])
  c <- array(dim = c(getShape(grid,"time"),length(lc),length(lonLimFinal)))
  ld <- seq(latLimGrid[2],latLim[2],resY)[-1]
  d <- array(dim = c(getShape(grid,"time"),length(ld),length(lonLimFinal)))
  latLimFinal <- c(lc,grid$xyCoords$y,ld) %>% unique()
  
  grid %<>% redim()
  grid <- lapply(1:getShape(grid, "member"), FUN = function(z) {
    grid %<>% subsetGrid(members = z) %>% redim(member = FALSE)
    dimNames <- attr(grid$Data,"dimensions")
    ab <- do.call("abind",list(a,grid$Data,b))
    grid$Data <- do.call("abind",list(c,ab,d,"along" = 2))
    attr(grid$Data,"dimensions") <- dimNames
    attr(grid$Data,"dimnames") <- NULL
    
    grid$xyCoords$x <- lonLimFinal
    grid$xyCoords$y <- latLimFinal
    return(grid)
  }) %>% bindGrid(dimension = "member")
}



#' #' @title Fill missing dates
#' #' @description fill with NA missing dates in grids and station datasets
#' #' @param grid grid or station data
#' #' @param tz Optional. Time zone. See Details.
#' #' @details The function attempts to recover the time zone of the input grid when this is correctly defined.
#' #' Otherwise, the function will leave it as unknown. See \code{\link{timezones}} for more details.
#' #' @return A grid filled with NAs in the previously missing date positions
#' #' @author M Iturbide
#' #' @export
#' 
#' 
#' fillGridDates <- function(grid, tz = "") {
#'   station <- ("loc" %in% getDim(grid)) 
#'   grid <- redim(grid, runtime = TRUE, var = TRUE)
#'   start <- getRefDates(grid)
#'   end <- getRefDates(grid, which = "end")
#'   day.step <- as.numeric(names(which.max(table(difftime(c(start, NA), c(NA, start), units = "days")))))
#'   message("Time difference of ", day.step, " days")
#'   formato <- "%Y-%m-%d %H:%M:%S"
#'   if (day.step >= 1) formato <- "%Y-%m-%d"
#'   tz <- attr(start[1], "tzone")
#'   usetz <- TRUE
#'   if (is.null(tz)) {
#'     tz <- ""
#'     usetz <- FALSE
#'     warning("Undefined time zone")
#'   }
#'   start <- as.POSIXlt(start, format = formato, tz = tz)
#'   end <- as.POSIXlt(end, format = formato, tz = tz)
#'   xs <- as.POSIXlt(as.character(seq.POSIXt(start[1], start[length(start)],
#'                                            by = day.step*24*60*60)),
#'                    format = formato, tz = tz)
#'   xe <- as.POSIXlt(as.character(seq.POSIXt(end[1], end[length(end)],
#'                                            by = day.step*24*60*60)),
#'                    format = formato, tz = tz)
#'   end <- NULL
#'   test <- data.frame("date" = start, "wh" = TRUE)
#'   start <- NULL
#'   result <- merge(data.frame("date" = xs), test,
#'                   by.y = "date", by.x = "date", all.x = TRUE)
#'   ind <- which(result[, "wh"])
#'   sh <- getShape(grid)
#'   sh[names(sh) == "time"] <- nrow(result)
#'   result <- NULL
#'   arr <- array(data = NA, dim = sh)
#'   arr[,,, ind ,,] <- grid[["Data"]] 
#'   grid[["Data"]] <- arr
#'   arr <- NULL
#'   attr(grid[["Data"]], "dimensions") <- names(sh)
#'   grid[["Dates"]][["start"]] <- strftime(xs, format = formato, tz = tz, usetz = usetz)
#'   grid[["Dates"]][["end"]] <- strftime(xe, format = formato, tz = tz, usetz = usetz)
#'   xs <- xe <- NULL
#'   grid <- redim(grid, drop = TRUE, loc = station)
#'   return(grid)
#' }
#' 
#' # end

  