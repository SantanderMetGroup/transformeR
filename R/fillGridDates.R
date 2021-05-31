#     fillGridDates.R Continuous time series filling the missing data with NA when necessary
#
#     Copyright (C) 2021 Santander Meteorology Group (http://www.meteo.unican.es)
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


#' @title Fill missing dates
#' @description fill with NA missing dates in grids and station datasets
#' @param grid grid or station data
#' @param tz Optional. Time zone. See Details.
#' @details The function attempts to recover the time zone of the input grid when this is correctly defined.
#' Otherwise, the function will leave it as unknown. See \code{\link{timezones}} for more details.
#' @return A grid filled with NAs in the previously missing date positions
#' @author M Iturbide
#' @family dateutils
#' @export


fillGridDates <- function(grid, tz = "") {
  station <- ("loc" %in% getDim(grid))
  grid <- setGridDates.asPOSIXlt(grid)
  grid <- redim(grid, runtime = TRUE, var = TRUE)
  start <- getRefDates(grid)
  end <- getRefDates(grid, which = "end")
  timeres <- getTimeResolution(grid)
  if (timeres == "unknown") stop("Unknown grid temporal resolution")
  by <- switch(timeres,
               "1h" = "hour",
               "3h" = 3600*3,
               "6h" = 3600*6,
               "12h" = 3600*12,
               "DD" = "day",
               "MM" = "month",
               "YY" = "year")
  xs <- seq.POSIXt(from = as.POSIXct(start[1]), to = as.POSIXct(start[length(start)]), by = by)
  xe <- seq.POSIXt(from = as.POSIXct(end[1]), to = as.POSIXct(end[length(end)]), by = by)
  end <- NULL
  test <- data.frame("date" = start, "wh" = TRUE)
  start <- NULL
  result <- merge(data.frame("date" = xs), test, by.y = "date", by.x = "date", all.x = TRUE)
  ind <- which(result[ , "wh"])
  sh <- getShape(grid)
  sh[names(sh) == "time"] <- nrow(result)
  result <- NULL
  arr <- array(data = NA, dim = sh)
  arr[,,, ind ,,] <- grid[["Data"]] 
  grid[["Data"]] <- arr
  arr <- NULL
  attr(grid[["Data"]], "dimensions") <- names(sh)
  grid[["Dates"]][["start"]] <- xs
  grid[["Dates"]][["end"]] <- xe
  xs <- xe <- NULL
  grid <- redim(grid, drop = TRUE, loc = station)
  return(grid)
}
# end


#' @title Set Grid dates as POSIXlt (possibly with user-defined time zone)
#' @description Internal utility for date format conversion to POSIX.lt
#' @details The function attempts to identify the format of the grid dates and 
#' to perform an adequate conversion to POSIXlt-class, including the time zone identification.
#' 
#' If no tz is specified, the function tries to "guess" it from the input. 
#' Otherwise it is set to the current time zone, with a warning.
#' 
#' If no hour-minute-sec information is stored, the output will read 00:00:00
#' 
#' @param grid Input C4R grid
#' @param tz Time zone to be coerced to. Default to \code{NULL}, 
#' that will preserve the actual tz if defined, or leave it as undefined otherwise.
#' @return The C4R grid with transformed dates
#' @keywords internal
#' @author J Bedia
#' @family dateutils
#' @examples \dontrun{
#' library(climate4R.datasets)
#' data("CMIP5_Iberia_hus850.rcp85")
#' # Dates are represented as character string
#' class(getRefDates(CMIP5_Iberia_hus850.rcp85))
#' # Conversion to POSIXlt
#' a <- setGridDates.asPOSIXlt(CMIP5_Iberia_hus850.rcp85)
#' range(getRefDates(a))
#' class(getRefDates(a))
#' ## WARNING: Forcing a time zone will change the tz attribute, but not the time
#' # (i.e.: it will alter the actual times):
#' b <- setGridDates.asPOSIXlt(CMIP5_Iberia_hus850.rcp85, tz = "Europe/Madrid")
#' range(getRefDates(b))
#' # Therefore, tz should be used ONLY when the time zone is missing or wrongly specified in the 
#' # input grid and we are sure about the actual TZ of our data
#' # It may also be used to change the representation of the time zone, e.g., changing 
#' # the denomination GMT to the equivalent UTC:
#' c <- setGridDates.asPOSIXlt(CMIP5_Iberia_hus850.rcp85, tz = "UTC")
#' range(getRefDates(c))
#' }


setGridDates.asPOSIXlt <- function(grid, tz = "") {
  ds <- getRefDates(grid)
  de <- getRefDates(grid)
  dateclass <- class(ds)
  ref <- ds[1]
  format <- "%Y-%m-%d %H:%M:%S"
  
  if(any(grepl("POSIXlt", dateclass))) {
    ## Retrieve tz from original data
    if (tz == "") tz <- ref$zone
    grid$Dates$start <- as.POSIXlt(ds, tz = tz, format = format)
    grid$Dates$end <- as.POSIXlt(de, tz = tz, format = format)
    
  } else if (any(grepl("POSIXct", dateclass))) {
    ## Retrieve tz from original data
    if (tz == "") tz <- attr(ref, "tzone")
    grid$Dates$start <- as.POSIXlt.POSIXct(ds, tz = tz, format = format)
    grid$Dates$end <- as.POSIXlt.POSIXct(de, tz = tz, format = format)
    
  } else if (dateclass == "character") {
    if (tz == "") {# Try to guess
      message("[", Sys.time(), "] Trying to determine the time zone...")
      # If dates are defined as a character string, somehow we need to "guess" the format
      # This is done by counting the number of empty spaces between character strings assuming 
      # Year:Month:day Time TimeZone
      split.string <- strsplit(ref, split = "\\s")[[1]]
      # First character of the first part of the date string (should be ALWAYS a number)
      if (isAlphaCharacter(getFirstChar(split.string[1]))) {
        stop("Unrecognized Date Format")
      }
      # If the second part of the string starts with a number, it is a time definition:
      if (is.na(split.string[2])) {# There is only a date with no more info attached to it
        ## TZ is unknown
        tz <- ""
      } else if (isAlphaCharacter(getFirstChar(split.string[2]))) {# It is a time def
        tz <- ""    
      } else {# Should be a time zone def
        # In  this case, the time zone should not be coercible to character.
        # Time zones should never begin with a number, or at least this is the assumption here (see 'OlsonNames()')
        tz <- split.string[2]
      }
      # The third part of the string must be a time zone definition
      if (!is.na(split.string[3])) {
        tz <- split.string[3]
      } else {
        tz <- ""
      } 
      if (tz == "") {
        warning("[", Sys.time(), "] Time zone unknown. It was set to the current auto-detected time zone (",
                as.POSIXlt(Sys.time())$zone,")")
      } else {
        message("[", Sys.time(), "] Time zone identified and set to ", tz, "\nSee \'setGridDates.asPOSIXlt\' to change the time zone")
      }
    }
    grid$Dates$start <- as.POSIXlt.character(ds, tz = tz, format = format)
    grid$Dates$end <- as.POSIXlt.character(de, tz = tz, format = format)
  }
  return(grid)
}


#' @title Get first character string before punctuation or space
#' @description Utility for parsing date elements as character strings
#' @param x character string
#' @return First part of a character string before any punctuation or space character
#' @keywords internal
#' @author J Bedia
#' @family dateutils
#' @examples \dontrun{ 
#'  getFirstChar("2008 Feb 12")
#'  getFirstChar("2008-02-12")
#'  getFirstChar("12:00:00 GMT")
#'  getFirstChar("Etc/UTC")
#'  }

getFirstChar <- function(x) {
  strsplit(x, split = "[[:punct:]]|[[:space:]]")[[1]][1]
}


#' @title Is alphabetic character
#' @description Is alphabetic character? Internal utility for parsing dates
#' @param x Character string
#' @return Logical
#' @keywords internal
#' @author J Bedia
#' @family dateutils

isAlphaCharacter <- function(x) {
  is.na(suppressWarnings(as.numeric(x)))
}


