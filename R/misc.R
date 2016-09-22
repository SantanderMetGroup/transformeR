#' @title Get season 
#' @description Retrieves the season encompassed by a station or grid object
#' @param obj Any object extending the station or grid classes
#' @return An integer vector with the season
#' @author J. Bedia 
#' @export
#' @examples 
#' data(iberia_ncep_ta850)
#' getSeason(iberia_ncep_ta850) # Boreal winter (DJF)

getSeason <- function(obj) {
      if ("season" %in% names(attributes(obj$Dates))) {
            attr(obj$Dates, "season")
      } else {
            dimNames <- getDim(obj)
            aux <- if (any(grepl("var", dimNames))) {
                  as.integer(substr(obj$Dates[[1]]$start, 6,7))      
            } else {
                  as.integer(substr(obj$Dates$start, 6,7))      
            }
            unique(aux)
      }
}
# End


#' @title Get years as a factor
#' @description Extract the year as a factor (e.g. for computing annual statistics)
#' @param obj Any object extending the station or grid classes
#' @return A vector of years of the same length as the time dimension of the object, 
#' seasonally-adjusted in the case of year-crossing seasons (e.g. DJF). See details.
#' @details The function performs a very basic operation, extracting the year element from the 
#' dates previously converted to POSIXlt. The trick lies in the year-crossing seasons. For instance:
#'  by convention, winter 2001 encompasses December 2000 and January, February 2001. Therefore, in order to compute
#' annual statistics for a year-crossing season, it is necessary to modify first the vector of years, 
#' and assign year 2001 to the preceding December. Similarly, the next December 2001 belongs to winter 2002,
#'  and so on... The function is useful for computing and/or plotting annual statistics, seasonal climatologies ... 
#' @note Warning:
#' The function should no be used to extract the vector of actual date years
#' @author J. Bedia 
#' @export
#' @examples 
#' data(iberia_ncep_hus850)
#' getSeason(iberia_ncep_hus850)
#' # Winter 1991-2010
#' range(iberia_ncep_hus850$Dates$start)
#' ## Time series for the first point
#' # Dates vector
#' time <- as.POSIXlt(iberia_ncep_hus850$Dates$start, tz = "GMT")
#' hus850 <- iberia_ncep_hus850$Data[ ,1,1]
#' plot(time, hus850, ty = "l")
#' ## Computation of the annual series for winter specific humidity:
#' par(mfrow = c(2,1))
#' ## Wrong:
#' years <- as.POSIXlt(iberia_ncep_hus850$Dates$start)$year + 1900
#' x <- tapply(hus850, INDEX = list(years), FUN = mean)
#' plot(unique(years), x, ty = "b")
#' points(1990, x[1], col = "red", cex = 2, lwd = 2)
#' ## Correct:
#' years <- getYearsAsINDEX(iberia_ncep_hus850)
#' x <- tapply(hus850, INDEX = years, FUN = mean)
#' plot(unique(years), x, ty = "b")
#' par(mfrow = c(1,1))
#' 

getYearsAsINDEX <- function(obj) {
      season <- getSeason(obj)
      dimNames <- getDim(obj)
      aux.dates <- if (any(grepl("var", dimNames))) {
            obj$Dates[[1]]$start
      } else {
            obj$Dates$start
      }
      yrs <- as.numeric(substr(aux.dates,1,4))
      mon <- as.numeric(substr(aux.dates,6,7))
      if (identical(yrs, unique(yrs))) {
            yrs
            if (!identical(season, sort(season))) {
                  yrs <- yrs + 1
            }
      } else {    
            if (!identical(season, sort(season))) {
                  yy <- unique(yrs)[-1]
                  aux <- match(mon, season)
                  brks <- c(1, which(diff(aux) < 0) + 1, length(aux) + 1)
                  l <- lapply(1:(length(brks) - 1), function(x) {
                        a <- yrs[brks[x]:(brks[x + 1] - 1)]
                        return(rep(yy[x], length(a)))
                  })
                  yrs  <- do.call("c", l)
            }
      }
      return(yrs)
}
# End

#' @title  Retrieve dimensions attribute
#' @description Retrieve dimensions attribute
#' @param obj A grid or station object
#' @return A character vector with the dimensions attribute of the object's \code{Data} component.
#' @keywords internal
#' @export
#' @author J. Bedia

getDim <- function(obj) {
      attr(obj[["Data"]], "dimensions")
}

#' @title  Retrieve array shape 
#' @description Retrieve array attributes 'dimensions' and 'dim'
#' @param obj A grid or station object
#' @return An integer vector with dim values, labelled with the \code{"dimension"} attribute names
#' @keywords internal
#' @export
#' @author J. Bedia

getShape <- function(obj, dimension = NULL) {
      dimNames <- getDim(obj)
      shape <- dim(obj[["Data"]])
      if (!is.null(dimension)) {
            ind <- match(dimension, dimNames)
            if (anyNA(ind)) stop("Input 'dimension' value not found")
            shape <- shape[ind]
            dimNames <- dimNames[ind]
      }
      names(shape) <- dimNames
      return(shape)
}


