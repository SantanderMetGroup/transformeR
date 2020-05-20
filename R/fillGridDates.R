#' @title Fill missing dates
#' @description fill with NA missing dates grids and station data
#' @param grid grid or station data
#' @param tz Otptional. Time zone (by default CET is used, see e.g. \code{\{as.POSIXlt}).
#' @author M Iturbide
#' @export


fillGridDates <- function(grid, tz = ""){
  station <- ("loc" %in% getDim(grid)) 
  grid <- redim(grid, runtime = TRUE, var = TRUE)
  start <- grid$Dates$start
  end <- grid$Dates$end
  day.step <- min(difftime(c(start, NA), c(NA, start)), na.rm = TRUE)
  message("Time difference of ", day.step, " days")
  formato <- "%Y-%m-%d %H:%M:%S"
  if (day.step >= 1) formato <- "%Y-%m-%d"
  start <- as.POSIXlt(start, format = formato, tz = tz)
  end <- as.POSIXlt(end, format = formato, tz = tz)
  xs <- as.POSIXlt(as.character(seq.POSIXt(start[1], start[length(start)], by= day.step)), format = formato, tz = tz)
  xe <- as.POSIXlt(as.character(seq.POSIXt(end[1], end[length(end)], by= day.step)), format = formato, tz = tz)
  test <- data.frame("date" = start, "wh" = TRUE)
  result <- merge(data.frame("date" = xs), test, by.y = "date", by.x = "date", all.x = TRUE)
  ind <- which(result[, "wh"])
  sh <- getShape(grid)
  sh[names(sh) == "time"] <- nrow(result)
  arr <- array(data = NA, dim = sh)
  arr[,,, ind ,,] <- grid[["Data"]] 
  grid[["Data"]] <- arr
  attr(grid[["Data"]], "dimensions") <- names(sh)
  grid[["Dates"]][["start"]] <- strftime(xs, format = formato, tz = tz, usetz = TRUE)
  grid[["Dates"]][["end"]] <- strftime(xe, format = formato, tz = tz, usetz = TRUE)
  grid <- redim(grid, drop = TRUE, loc = station)
  return(grid)
}

# end
  