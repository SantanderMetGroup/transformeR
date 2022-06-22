##     gridUncertainty.R Subset two or more grids to the common members
##
##     Copyright (C) 2017 Santander Meteorology Group (http://www.meteo.unican.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
## 
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
## 
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' @title Calculate the uncertainty of the climate change signal.
#' @description Calculate the uncertainty of the climate change signal.
#' @param historical A grid with annual historical data
#' @param anomaly A grid (with time dimension = 1).
#' @return A grid with three values (uncertainty categories); 0 = robust signal, 
#' 1 = no signal or no change, 2 = conflicting signals).
#' @author M. Iturbide
#' @export

gridUncertainty <- function(historical, anomaly){
  si <- signal(historical, anomaly)
  uncer1 <- aggregateGrid(si, aggr.mem = list(FUN = signalAgreement, th = 66, condition = "GT"))
  uncer2 <- aggregateGrid(si, aggr.mem = list(FUN = signalAgreement, th = 66, condition = "LT"))
  uncer3 <- aggregateGrid(anomaly, aggr.mem = list(FUN = modelAgreement, th = 80))
  uncer23 <- binaryGrid(gridArithmetics(uncer2, uncer3, operator = "+"), "GT", 0)  
  uncer.a.aux1 <- gridArithmetics(uncer1, 1, -1, operator = c("-", "*"))
  uncer.a.aux2 <- gridArithmetics(uncer23, 1, -1, operator = c("-", "*"))
  eval(parse(text = paste("uncer.a.aux2$Data[which(uncer.a.aux2$Data == 1)] <- 2")))
  uncer.a.aux <- gridArithmetics(uncer.a.aux1, uncer.a.aux2, operator = c("+"))
  eval(parse(text = paste("uncer.a.aux$Data[which(uncer.a.aux$Data > 1)] <- 2")))
  uncer.a.aux
}

#' @title Calculate signal agreement.
#' @description Calculate signal agreement.
#' @param x binary numeric
#' @param th threshold
#' @param condition character (choices: "GT", "GE", "LT", "LE")
#' @return binary numeric (0 = no agreement, 1 = agreement).
#' @author M. Iturbide
#' @export

signalAgreement <- function(x, th = 66, condition = "GT") {
  condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
  ineq <- switch(condition,
                  "GT" = ">",
                  "GE" = ">=",
                  "LT" = "<",
                  "LE" = "<=")
  eval(parse(text = paste("as.numeric((sum(x)/length(x)*100)", ineq, "th)")))
}

#' @title Calculate model agreement on the sign of change.
#' @description Calculate if signal exists in the anomalies relative to the historical variability.
#' @param x numeric of anomalies
#' @param th percentage value (Default is 80).
#' @return binary numeric (0 = no agreement, 1 = agreement).
#' @author M. Iturbide
#' @export
#' 
modelAgreement <- function(x, th = 80) {
  mp <- mean(x, na.rm = TRUE)
  if (is.na(mp)) {
    1
  } else {
    if (mp > 0) {
      as.numeric(sum(as.numeric(x > 0), na.rm = TRUE) > as.integer(length(x) * th / 100))
    } else if (mp < 0) {
      as.numeric(sum(as.numeric(x < 0), na.rm = TRUE) > as.integer(length(x) * th / 100))
    } else if (mp == 0) {
      1
    }
  }
}

#' @title Calculate if signal exists in the anomalies.
#' @description Calculate if signal exists in the anomalies relative to the historical variability.
#' @param historical A grid with historical data to calculate the variability threshols (ideally annual data)
#' @param anomaly A grid (with time dimension = 1).
#' @return A binary grid (0 = no signal, 1 = signal).
#' @author M. Iturbide
#' @export

signal <- function(historical, anomaly) {
  vth <- getVariabilityThreshold(historical)
  anomaly$Data <- abs(anomaly$Data)
  sig <- gridArithmetics(anomaly, vth, operator = "-")
  binaryGrid(sig, condition = "GT", threshold = 0, values = c(0, 1))
}

#' @title Calculate the variability threshold.
#' @description Subset two or more grids to the common members.
#' @param grid A grid (ideally with annual data).
#' @return A grid.
#' @author M. Iturbide
#' @export

getVariabilityThreshold <- function(grid) {
  climatology(grid, clim.fun = list(FUN = vth.aux))
}


#' @title Compute variability threshold
#' @description Compute variability threshold
#' @param x numeric
#' @param period value of the number of years to consider as a period (default is 20)
#' @return numeric. A single value of the variability threshold as defined in the IPCC AR6 WGI (Atlas chapter)
#' @author M. Iturbide
#' @export

vth.aux <- function(x, period = 20) {
  (sqrt(2) * 1.645 * sd(x))/sqrt(period) 
}



