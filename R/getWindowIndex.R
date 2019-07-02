#' @title Get the index of the window days.
#' @description Get the index of the days that corresponding to the window and the target days centered in it.
#' @param y A grid or station data containing the observed climate data for the training period
#' @param x A grid or station data containing the simulated climate data for the training period 
#' @param newdata A grid containing the simulated climate for the test period.
#'  \code{"scaling"}, \code{"pqm"} and \code{"gpqm"} \code{"variance"},\code{"loci"} and \code{"ptr"}. See details.
#' @param window vector of length = 2 (or 1) specifying the time window width used to calibrate and the 
#' target days (days that are being corrected). If the window length = 1 the window width is no larger than the 
#' target days. The window is centered on the target day/s (window width >= target days). 
#' @param delta.method Logical (default is FALSE).
#' @keywords internal
#' @export
#' @author M. Iturbide

getWindowIndex <- function(y, x, newdata, window, delta.method = FALSE){
  if (length(window) == 1) window <- rep(window, 2)
  step <- window[2]
  window <- window[1]
  if (window - step < 0) stop("The first argument of window must be equal or higher than the second. See ?biasCorrection")
  datesList <- as.POSIXct(x$Dates$start, tz = "GMT", format = "%Y-%m-%d")
  yearList <- unlist(strsplit(as.character(datesList), "[-]"))
  dayListObs <- array(data = c(as.numeric(yearList[seq(2,length(yearList),3)]),as.numeric(yearList[seq(3,length(yearList),3)])), dim = c(length(datesList),2))
  dayList <- unique(dayListObs,index.return = datesList)
  annual <- TRUE
  if (nrow(dayList) < 360) annual <- FALSE
  indDays <- array(data = NaN, dim = c(length(datesList),1))
  for (d in 1:dim(dayList)[1]) {
    indDays[which(sqrt((dayListObs[,1] - dayList[d,1]) ^ 2 + (dayListObs[,2] - dayList[d,2]) ^ 2) == 0)] <- d
  }
  datesList <- as.POSIXct(newdata$Dates$start, tz = "GMT", format = "%Y-%m-%d")
  yearList <- unlist(strsplit(as.character(datesList), "[-]"))
  dayListSim <- array(data = c(as.numeric(yearList[seq(2,length(yearList),3)]),as.numeric(yearList[seq(3,length(yearList),3)])), dim = c(length(datesList),2))
  indDaysSim <- array(data = NaN, dim = c(length(datesList),1))
  for (d in 1:dim(dayList)[1]) {
    indDaysSim[which(sqrt((dayListSim[,1] - dayList[d,1]) ^ 2 + (dayListSim[,2] - dayList[d,2]) ^ 2) == 0)] <- d
  }
  steps <- floor(dim(dayList)[1]/step)
  #steps loop
  output <- list()
  for (j in 1:steps) {
    days <- ((j - 1) * step + 1):((j - 1) * step + step)
    if (j == steps) days <- days[1]:dim(dayList)[1]
    indObs <- lapply(1:length(days), function(h){
      which(indDays == days[h])
    })
    indObs <- sort(do.call("abind", indObs))
    head <- floor((window - step)/2)
    tail <- head
    before <- after <- FALSE
    if (!annual) {
      before <- min(indDays[indObs]) - 1 - head < 1
      if (before) head <- head + (min(indDays[indObs]) - 1 - head) 
      after <- max(indDays[indObs]) + tail > nrow(dayList)
      if (after) tail <- nrow(dayList) - max(indDays[indObs])   
    }
    indObsWindow <- array(data = NA, dim = c((head + step + tail)*length(indObs)/step,1))
    breaks <- c(which(diff(indObs) != 1), length(indObs))
    for (d in 1:length(breaks)) {
      if (d == 1) {
        piece <- indObs[1:breaks[1]]
      } else {
        piece <- indObs[(breaks[d - 1] + 1):breaks[d]]
      }
      suppressWarnings(indObsWindow[((d - 1) * (head + step + tail) + 1):(d * (head + step + tail))] <- 
                         (min(piece, na.rm = TRUE) - head):(max(piece, na.rm = TRUE) + tail))
    }
    if (annual) {
      indObsWindow[which(indObsWindow <= 0)] <- 1
      indObsWindow[which(indObsWindow >  length(indDays))] <- length(indDays)
      indObsWindow <- unique(indObsWindow)
    }
    indSim <- lapply(1:length(days), function(h){
      which(indDaysSim == days[h])
    })
    indSim <- sort(do.call("abind", indSim))
    names(indSim) <- newdata$Dates$start[indSim]
    indObsWindow <- indObsWindow[which(!is.na(x$Dates$start[indObsWindow]))]
    names(indObsWindow) <- x$Dates$start[indObsWindow]
    indobservations <- match(as.POSIXct(x$Dates$start[indObsWindow], format = "%Y-%m-%d"), as.POSIXct(y$Dates$start, format = "%Y-%m-%d"))
    names(indobservations) <- y$Dates$start[indobservations]
    names(indObs) <- x$Dates$start[indObs]
    indObsObs <- match(as.POSIXct(x$Dates$start[indObs], format = "%Y-%m-%d"), as.POSIXct(y$Dates$start, format = "%Y-%m-%d"))
    output[[paste0("Window", j)]] <- list("obsWindow" = indobservations, "window" = indObsWindow, "step" = indSim)
    if (delta.method) output[[paste0("Window", j)]][["deltaind"]] <- indObsObs
  }
  return(output)
}
