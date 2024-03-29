#     lambWT.R Calculation of the Weather types (WT) circulation indices from grid
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

#' @title Calculation of Lamb Weather types (WT).
#' @description Calculate automated Lamb WT as defined in Trigo and daCamara (2000), Int J Climatol 
#' @param grid A grid (gridded or station dataset), or multimember grid object of MSLP values.
#' @param center.point A two value vector that must include lon and lat from a location that will work as center point for the Lamb WT.
#' See details. 
#' @param typeU Logical. Should "Unclassified" type be included in the output. Default to \code{FALSE}, so the closest is assigned.
#' @details According to Trigo and daCamara (2000), Int J Climatol, Lamb WT is only applied on North Atlantic domain. 
#' The input grid units must be Pa, not hPa/mbar. If it is not in Pa, the units must be converted.
#' A center location point must be specified by the user. Then, the function calculates from left to right and from first to 16st 
#' the rest of the location point from the grid specified by Trigo and daCamara (2000):
#'  
#'   \tabular{ccccccccccccc}{
#'     \tab  \tab  \tab    \tab  \tab  \tab 01 \tab  \tab  \tab 02 \tab  \tab  \tab    \cr
#'     \tab  \tab  \tab 03 \tab  \tab  \tab 04 \tab  \tab  \tab 05 \tab  \tab  \tab 06 \cr
#'     \tab  \tab  \tab 07 \tab  \tab  \tab 08 \tab  \tab  \tab 09 \tab  \tab  \tab 10 \cr
#'     \tab  \tab  \tab 11 \tab  \tab  \tab 12 \tab  \tab  \tab 13 \tab  \tab  \tab 14 \cr
#'     \tab  \tab  \tab    \tab  \tab  \tab 15 \tab  \tab  \tab 16 \tab  \tab  \tab    
#' }  
#'
#' where the north-south distance is 5º and the west-east distance is 10º. 26 different WTs are defined, 10 pure types (NE, E, SE, S, SW, 
#' W, NW, N, C and A) and 16 hybrid types (8 for each C and A hybrid). 
#' @return The Lamb WT circulation index (and members, if applicable) with:
#' \itemize{
#' \item index: vector with the corresponding weather type from each point of the series, that is defined as follows:
#'
#'\tabular{cccc}{
#' \tab purely anticyclonic:  \tab 1 \tab "A" \cr 
#' \tab anticyclonic northeasterly:  \tab  2  \tab "ANE" \cr 
#' \tab anticyclonic easterly: \tab 3 \tab "AE" \cr
#' \tab anticyclonic southeasterly: \tab 4 \tab "ASE" \cr
#' \tab anticyclonic southerly: \tab 5 \tab "AS" \cr
#' \tab anticyclonic southwesterly: \tab 6 \tab "ASW" \cr
#' \tab anticyclonic westerly: \tab 7 \tab "AW" \cr
#' \tab anticyclonic northwesterly: \tab 8 \tab "ANW" \cr
#' \tab anticyclonic northerly: \tab 9 \tab "AN" \cr
#' \tab northeasterly: \tab 10 \tab "NE" \cr
#' \tab easterly: \tab 11 \tab "E" \cr
#' \tab southeasterly:\tab 12 \tab "SE" \cr
#' \tab southerly: \tab 13 \tab "S" \cr
#' \tab southwesterly: \tab 14 \tab "SW" \cr
#' \tab westerly: \tab 15 \tab "W" \cr
#' \tab northwesterly: \tab 16 \tab "NW" \cr
#' \tab northerly: \tab 17 \tab "N" \cr
#' \tab purely cyclonic:  \tab 18 \tab "C" \cr 
#' \tab cyclonic northeasterly:  \tab  19  \tab "CNE" \cr 
#' \tab cyclonic easterly: \tab 20 \tab "CE" \cr
#' \tab cyclonic southeasterly: \tab 21 \tab "CSE" \cr
#' \tab cyclonic southerly: \tab 22 \tab "CS" \cr
#' \tab cyclonic southwesterly: \tab 23 \tab "CSW" \cr
#' \tab cyclonic westerly: \tab 24 \tab "CW" \cr
#' \tab cyclonic northwesterly: \tab 25 \tab "CNW" \cr
#' \tab cyclonic northerly: \tab 26 \tab "CN" \cr 
#' }
#' 
#' \item pattern: Array with the spatial pattern of the 26 weather types obtained.
#' \item dates and coordinates.
#' \item further arguments related to the Lamb WT index.
#' }
#' @export
#' @examples \dontrun{
#' require(climate4R.indices)
#' data(NCEP_slp_2001_2010)
#' lamb.wt <- lambWT(grid = NCEP_slp_2001_2010)
#' }


lambWT <- function(grid, center.point = c(-5, 55), typeU = FALSE) {

  #  *** PREPARE OUTPUT GRID *** 
  wt <- vector("list", 1)
  names(wt) <- "lamb"
  nWTs <- 26
  centerlon <- center.point[1]
  centerlat <- center.point[2]
  
  suppressMessages(members <- getShape(grid, dimension = "member"))
  if (is.na(members)) {
    grid <- redim(grid)
    members <- getShape(grid, dimension = "member")
  }
  
  wt[[1]] <- c(wt[[1]], vector("list", members)) 
  if (members > 1) names(wt[[1]]) <- paste0("Member_", 1:members)
  
  for (x in 1:members) {
    grid.member <- subsetGrid(grid, members = x)
    memb <- vector("list", 1)
    
    #  *** LAMB WT CALCULATIONS *** 
    #Inicialization of variables:
    n <- getShape(grid.member, dimension = "time")
    wtseries <- vector(mode = "integer", n[[1]]) #Inicialize vector with size = lenght of "time" dimension
    dirdeg <- vector(mode = "numeric",n[[1]])
    d <- vector(mode = "numeric",n[[1]])
    
    #Units conversion:
    slp.units <- c("Pascals", "Pa")
    if (!(attr(grid.member$Variable, "units") %in% slp.units)) { 
      stop("The grid variable must have Sea Level Pressure units in 'Pascals' (Pa).\nSee function convertR::udConvertGrid for unit conversion.")
    }
    
    #Preparing the input of lamb WT
    lon.array <- rep(centerlon, times = 16) + c(-5, 5, -15, -5, 5, 15, -15, -5, 5, 15, -15, -5, 5, 15, -5, 5)
    #if(centerlat > 0){
      lat.array <- rep(centerlat, times = 16) + c(10, 10, 5, 5, 5, 5, 0, 0, 0, 0, -5, -5, -5, -5, -10, -10)
   # }else if(centerlat < 0){
   #   lat.array <- rep(centerlat, times = 16) + c(-10,- 10, -5, -5, -5, -5, 0, 0, 0, 0, 5, 5, 5, 5, 10, 10)
   # }
     
    if(abs(centerlon) >= 165){
      for (i in 1:length(lon.array)) {
        if(lon.array[i] > 180){
          lon.array[i] <- lon.array[i] %% -180
        } else if (lon.array[i] < -180){
          lon.array[i] <- lon.array[i] %% 180
        } else if (lon.array[i] == -180){
          lon.array[i] <- 180
        }
      }
    }
    
    grid.inter <- interpGrid(grid.member, new.coordinates = list(x = lon.array, y = lat.array), method = "nearest")
    X <- grid.inter$Data
    gc()
    
    sf.const <- 1/cospi(centerlat/180)
    latshift <- ifelse(centerlat > 0, -5, 5)
    zw.const1 <- sinpi(centerlat/180)/sinpi((centerlat + latshift)/180)
    zw.const2 <- sinpi(centerlat/180)/sinpi((centerlat - latshift)/180)
    zs.const <- 1/(2*cospi(centerlat/180)^2)
    
      ##FORTRAN code from Colin Harpham, CRU
    if(centerlat > 0){
      w <- 0.005*((X[ , 12] + X[ , 13]) - (X[ , 4] + X[ , 5]))
      s <- (sf.const*0.0025) * (X[ , 5] + 2*X[ , 9] + X[ , 13] - X[ , 4] - 2*X[ , 8] - X[ , 12])
    } else if(centerlat < 0){
      w <- 0.005*((X[ , 4] + X[ , 5]) - (X[ , 12] + X[ , 13]))
      s <- (sf.const*0.0025) * (X[ , 4] + 2*X[ , 8] + X[ , 12] - X[ , 5] - 2*X[ , 9] - X[ , 13])
    }
    
    ind <- which(abs(w) > 0 & !is.na(w))
    dirdeg[ind] <- (atan(s[ind]/w[ind]))*180/pi
    ind <- which(w == 0 & !is.na(w))
    ind1 <- intersect(ind, which(s > 0 & !is.na(s))) 
    dirdeg[ind1] <- 90
    ind1 <- intersect(ind, which(s < 0 & !is.na(s))) 
    dirdeg[ind1] <- -90  
    d[which(w >= 0 & !is.na(w))] <- 270 - dirdeg[which(w >= 0 & !is.na(w))] #SW & NW quadrant
    d[which(w < 0 & !is.na(w))] <- 90 - dirdeg[which(w < 0 & !is.na(w))] #SE & NE quadrant
    
    if(centerlat > 0){
      #westerly shear vorticity
      zw <- (zw.const1*0.005) * ((X[ , 15] + X[ , 16]) - (X[ , 8] + X[ , 9])) - (zw.const2*0.005) * ((X[ , 8] + X[ , 9]) - (X[ , 1] + X[ , 2]))
      
      #southerly shear vorticity  
      zs <- (zs.const*0.0025) * (X[ , 6] + 2*X[ , 10] + X[ , 14] - X[ , 5] - 2*X[ , 9] - X[ , 13]) - (zs.const*0.0025) * (X[ , 4] + 2*X[ , 8] + X[ , 12] - X[ , 3] - 2*X[ , 7] - X[ , 11])
    }else if(centerlat < 0){
      #westerly shear vorticity
      zw <- (zw.const1*0.005) * ((X[ , 1] + X[ , 2]) - (X[ , 8] + X[ , 9])) - (zw.const2*0.005) * ((X[ , 8] + X[ , 9]) - (X[ , 15] + X[ , 16]))
      
      #southerly shear vorticity  
      zs <- (zs.const*0.0025) * (X[ , 3] + 2*X[ , 7] + X[ , 11] - X[ , 4] - 2*X[ , 8] - X[ , 12]) - (zs.const*0.0025) * (X[ , 5] + 2*X[ , 9] + X[ , 13] - X[ , 6] - 2*X[ , 10] - X[ , 14])
    }
    #total shear vorticity
    z <- zw + zs
    # resultant flow
    f <- sqrt(w^2 + s^2)
    
    #define direction sectors form 1 to 8, definition like on http://www.cru.uea.ac.uk/cru/data/hulme/uk/lamb.htm 
    neind <- which(d > 22.5 & d <= 67.5) #NE
    eind <- which(d > 67.5 & d <= 112.5) #E
    seind <- which(d > 112.5 & d <= 157.5) #SE
    soind <- which(d > 157.5 & d <= 202.5) #S
    swind <- which(d > 202.5 & d <= 247.5) #SW
    wind <- which(d > 247.5 & d <= 292.5) #W
    nwind <- which(d > 292.5 & d <= 337.5) #NW
    nind <- which(d > 337.5 | d <= 22.5) #N
    d[neind] = 10; d[eind] = 11; d[seind] = 12; d[soind] = 13
    d[swind] = 14; d[wind] = 15; d[nwind] = 16; d[nind] = 17
    names(d)[neind] <- "NE"; names(d)[eind] <- "E"; names(d)[seind] <- "SE"; names(d)[soind] <- "S"
    names(d)[swind] <- "SW"; names(d)[wind] <- "W"; names(d)[nwind] <- "NW"; names(d)[nind] <- "N"
    
    
    #Define discrete wt series, codes similar to http://www.cru.uea.ac.uk/cru/data/hulme/uk/lamb.htm
    pd <- which(abs(z) < f) 
    wtseries[pd] <- d[pd] #purely directional type
    names(wtseries)[pd] <- names(d)[pd]
    pcyc <- which(abs(z) >= (2*f) & z >= 0) 
    wtseries[pcyc] <- 18 #purely cyclonic type
    names(wtseries)[pcyc] <- "C"
    pant <- which(abs(z) >= (2*f) & z < 0) 
    wtseries[pant] <- 1 #purely anticyclonic type
    names(wtseries)[pant] <- "A"
    hyb <- which(abs(z) >= f & abs(z) < (2*f)) #hybrid type
    hybant <- intersect(hyb, which(z < 0)) #anticyclonic
    hybcyc <- intersect(hyb, which(z >= 0)) #cyclonic
    for (i in 10:17) {
      #directional anticyclonic
      wtseries[intersect(hybant, which(d == i))] <- i - 8
      #mixed cyclonic
      wtseries[intersect(hybcyc, which(d == i))] <- i + 9
      if (i == 10) {names(wtseries)[intersect(hybant, which(d == i))] <- "ANE"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CNE"}
      else if (i == 11) {names(wtseries)[intersect(hybant, which(d == i))] <- "AE"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CE"}
      else if (i == 12) {names(wtseries)[intersect(hybant, which(d == i))] <- "ASE"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CSE"}
      else if (i == 13) {names(wtseries)[intersect(hybant, which(d == i))] <- "AS"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CS"}
      else if (i == 14) {names(wtseries)[intersect(hybant, which(d == i))] <- "ASW"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CSW"}
      else if (i == 15) {names(wtseries)[intersect(hybant, which(d == i))] <- "AW"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CW"}
      else if (i == 16) {names(wtseries)[intersect(hybant, which(d == i))] <- "ANW"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CNW"}
      else {names(wtseries)[intersect(hybant, which(d == i))] <- "AN"; names(wtseries)[intersect(hybcyc, which(d == i))] <- "CN"}
    }
    
  if (typeU == TRUE){
    nWTs <- 27
    indFlow <- which(abs(z) < 6 & f < 6)
    wtseries[indFlow] <- nWTs #Unclassified WT 'U'
    names(wtseries)[indFlow] <- "U";
  } 

    wtseries.2 <- wtseries[1:n[[1]]]
    
    lamb.list <- lapply(1:nWTs, function(y){
      lamb.pattern <- which(wtseries.2 == y)
      #We subset the desired point from slp dataset: 
      grid.wt <- subsetDimension(grid.member, dimension = "time", indices = lamb.pattern)
      suppressMessages(clim <- climatology(grid.wt))
      clim$Dates$start <- grid$Dates$start[1]
      clim$Dates$end <- tail(grid$Dates$end, n=1)
      return(clim)
    })
    
    lamb <- suppressWarnings(bindGrid(lamb.list, dimension = "time"))
    
    memb[[1]]$index <- as.integer(wtseries.2)
    memb[[1]]$pattern <- lamb$Data
    memb[[1]]$params <- rbind(w,s,f,zw,zs,z)
    attr(memb[[1]], "season") <- getSeason(grid)
    attr(memb[[1]], "dates_start") <- grid.member$Dates$start
    attr(memb[[1]], "dates_end") <- grid.member$Dates$end
    attr(memb[[1]], "centers") <- nWTs
    wt[[1]][[x]] <- memb
  }
  
  wt[[1]]$Variable <- grid$Variable
  attr(wt, "xCoords") <- grid$xyCoords$x
  attr(wt, "yCoords") <- grid$xyCoords$y
  attr(wt, "projection") <- attr(grid$xyCoords, "projection")
  
  return(wt)
  
}
