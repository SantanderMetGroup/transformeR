#' @title Fill missing dates
#' @description fill with NA missing dates in grids and station datasets
#' @param grid grid or station data
#' @param tz Optional. Time zone (by default CET is used, see e.g. \code{\{as.POSIXlt}).
#' @return A grid filled with NAs in the previously missing date positions
#' @author M Iturbide
#' @export


fillGridDates <- function(grid, tz = ""){
  station <- ("loc" %in% getDim(grid)) 
  grid <- redim(grid, runtime = TRUE, var = TRUE)
  start <- getRefDates(grid)
  end <- getRefDates(grid, which = "end")
  day.step <- as.numeric(names(which.max(table(difftime(c(start, NA), c(NA, start), units = "days")))))
  message("Time difference of ", day.step, " days")
  formato <- "%Y-%m-%d %H:%M:%S"
  if (day.step >= 1) formato <- "%Y-%m-%d"
  start <- as.POSIXlt(start, format = formato, tz = tz)
  end <- as.POSIXlt(end, format = formato, tz = tz)
  xs <- as.POSIXlt(as.character(seq.POSIXt(start[1], start[length(start)],
                                           by = day.step*24*60*60)),
                   format = formato, tz = tz)
  xe <- as.POSIXlt(as.character(seq.POSIXt(end[1], end[length(end)],
                                           by = day.step*24*60*60)),
                   format = formato, tz = tz)
  end <- NULL
  test <- data.frame("date" = start, "wh" = TRUE)
  start <- NULL
  result <- merge(data.frame("date" = xs), test,
                  by.y = "date", by.x = "date", all.x = TRUE)
  ind <- which(result[, "wh"])
  sh <- getShape(grid)
  sh[names(sh) == "time"] <- nrow(result)
  result <- NULL
  arr <- array(data = NA, dim = sh)
  arr[,,, ind ,,] <- grid[["Data"]] 
  grid[["Data"]] <- arr
  arr <- NULL
  attr(grid[["Data"]], "dimensions") <- names(sh)
  grid[["Dates"]][["start"]] <- strftime(xs, format = formato, tz = tz, usetz = TRUE)
  grid[["Dates"]][["end"]] <- strftime(xe, format = formato, tz = tz, usetz = TRUE)
  xs <- xe <- NULL
  grid <- redim(grid, drop = TRUE, loc = station)
  return(grid)
}

# end
  