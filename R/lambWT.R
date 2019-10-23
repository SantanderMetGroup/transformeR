#     lambWT.R Calculation of the Weather types (WT) circulation indices from grid
#
#     Copyright (C) 2019 Santander Meteorology Group (http://www.meteo.unican.es)
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
#' @description Calculate automated Lamb WT as defined in Trigo daCamara 2000 and Jones et al. 2012, 
#' both Int J Climatol 
#' @param grid A grid (gridded or station dataset), or multimember grid object of MSLP values.
#' @param center.point A two value vector that must include lon and lat from a location that will work as center point for the Lamb WT.
#' See details. 
#' @param base Baseline grid to be substracted for the calculation of anomalies. Default: NULL. See \code{?scaleGrid}.
#' @param ref Reference grid to be added for the calculation of anomalies. Default: NULL. See \code{?scaleGrid}.
#' @details According to Jones et al. 2012 (Int J Climatol), Lamb WT is only applied on North Atlantic domain. 
#' The input grid units must be Pa, not hPa/mbar. If it is not in Pa, the units will be converted automatically.
#' A center location point must be specified by the user. Then, the function calculates from left to right and from first to 16st 
#' the rest of the location point from the grid specified by Jones et al. 2012:
#'  
#'   \tabular{ccccccccccccc}{
#'     \tab  \tab  \tab    \tab  \tab  \tab 01 \tab  \tab  \tab 02 \tab  \tab  \tab    \cr
#'     \tab  \tab  \tab 03 \tab  \tab  \tab 04 \tab  \tab  \tab 05 \tab  \tab  \tab 06 \cr
#'     \tab  \tab  \tab 07 \tab  \tab  \tab 08 \tab  \tab  \tab 09 \tab  \tab  \tab 10 \cr
#'     \tab  \tab  \tab 11 \tab  \tab  \tab 12 \tab  \tab  \tab 13 \tab  \tab  \tab 14 \cr
#'     \tab  \tab  \tab    \tab  \tab  \tab 15 \tab  \tab  \tab 16 \tab  \tab  \tab    
#' }  
#'
#' where the north-south distance is 5º and the west-east distance is 10º.
#' @return The Lamb WT circulation index (and members, if applicable) with:
#' \itemize{
#' \item index: vector with the corresponding weather type from each point of the series, that is defined as follows:
#'
#' purely anticyclonic = 1
#' 
#' directional anticyclonic from NE to N = 2 to 9
#' 
#' purely directional from NE to N = 10 to 17
#' 
#' purely cyclonic = 18
#' 
#' directional cyclonic from NE to N = 19 to 26
#' 
#' light indeterminate flow N = 27  .
#' \item pattern: Array with the spatial pattern of the 27 weather types obtained.
#' \item dates and coordinates.
#' \item further arguments related to the Lamb WT index.
#' }
#' @importFrom convertR udConvertGrid
#' @export
#' @examples \dontrun{
#' data(NCEP_slp_2001_2010)
#' lamb.wt <- lambWT(grid = NCEP_slp_2001_2010)
#' }


lambWT <- function(grid, center.point = c(-5, 55), base = NULL, ref = NULL) {

  #  *** PREPARE OUTPUT GRID *** 
  wt <- vector("list", 1)
  names(wt) <- "lamb"
  
  if (!is.null(base) & !is.null(ref)){
    grid <- scaleGrid(grid = grid, base = base, ref = ref, type = "center")
  }
  
  suppressMessages(members <- getShape(grid, dimension = "member"))
  if (is.na(members)) {
    grid<-redim(grid)
    members <- getShape(grid, dimension = "member")
  }
  
  wt[[1]] <- c(wt[[1]], vector("list", members)) 
  if(members>1) names(wt[[1]]) <- paste0("Member_", 1:members)
  
  for (x in 1:members){
    grid.member <- subsetGrid(grid, members = x)
    memb <- vector("list", 1)
    
    #  *** LAMB WT CALCULATIONS *** 
    #Inicialization of variables:
    n<-getShape(grid.member, dimension = "time")
    wtseries<-vector(mode = "numeric",n[[1]]) #Inicialize vector with size = lenght of "time" dimension
    dirdeg<-vector(mode = "numeric",n[[1]])
    d<-vector(mode = "numeric",n[[1]])
    
    #Units conversion:
    if (attr(grid.member$Variable, "units") == "hPa" | attr(grid.member$Variable, "units") == "mbar"){
      grid.member <- udConvertGrid(grid.member, new.units = "Pa")
      message("Converting grid units from '",paste(attr(grid.member$Variable, "units")), "' to 'Pa'...")
    }
    
    #Preparing the input of lamb WT
    centerlon<-center.point[1]
    centerlat<-center.point[2]
    
    lon.array <- rep(centerlon, times=16)+c(-5, 5, -15, -5, 5, 15, -15, -5, 5, 15, -15, -5, 5, 15, -5, 5)
    lat.array <- rep(centerlat, times=16)+c(10, 10, 5, 5, 5, 5, 0, 0, 0, 0, -5, -5, -5, -5, -10, 10)
    
    subgrid <- grid.member
    l <- lapply(1:16, function(i){
      subgrid$xyCoords$x <- lon.array[i]
      subgrid$xyCoords$y <- lat.array[i]
      grid.inter<-intersectGrid(grid.member, subgrid, type = c("spatial"), which.return = 1)
      return(grid.inter)
    }) 
    list.grid<-bindGrid(l, dimension = "loc")
    X<-list.grid$Data[1,1, , ]
    
    sf.const<-1/cospi(centerlat/180)
    zw.const1<-sinpi(centerlat/180)/sinpi((centerlat-5)/180)
    zw.const2<-sinpi(centerlat/180)/sinpi((centerlat+5)/180)
    zs.const<-1/(2*cospi(centerlat/180)^2)
    
    ##FORTRAN code from Colin Harpham, CRU
    w <- 0.005*((X[ , 12]+X[ , 13])-(X[ , 4]+X[ , 5]))
    s <- (sf.const*0.0025) * (X[ , 5] + 2*X[ , 9] + X[ , 13] - X[ , 4] - 2*X[ , 8] - X[ , 12])
    
    ind <- which(abs(w) > 0 & !is.na(w))
    dirdeg[ind]<-(atan(s[ind]/w[ind]))*180/pi
    ind <- which(w == 0 & !is.na(w))
    ind1 <- intersect(ind, which(s > 0 & !is.na(s))) 
    dirdeg[ind1] <- 90
    ind1 <- intersect(ind, which(s < 0 & !is.na(s))) 
    dirdeg[ind1] <- -90  
    d[which(w >= 0 & !is.na(w))]<-270-dirdeg[which(w >= 0 & !is.na(w))] #SW & NW quadrant
    d[which(w < 0 & !is.na(w))]<-90-dirdeg[which(w < 0 & !is.na(w))] #SE & NE quadrant
    
    #westerly shear vorticity
    zw <- (zw.const1*0.005) * ((X[ , 15]+X[ , 16])-(X[ , 8]+X[ , 9])) - (zw.const2*0.005) * ((X[ , 8]+X[ , 9])-(X[ , 1]+X[ , 2]))
    
    #southerly shear vorticity  
    zs <- (zs.const*0.0025) * (X[ , 6]+2*X[ , 10] + X[ , 14]-X[ , 5] - 2*X[ , 9]-X[ , 13]) - (zs.const*0.0025) * (X[ , 4]+2*X[ , 8] + X[ , 12]-X[ , 3] - 2*X[ , 7]-X[ , 11])
    
    #total shear vorticity
    z <- zw + zs
    # resultant flow
    f <- sqrt(w^2+s^2)
    
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
    
    #Define discrete wt series, codes similar to http://www.cru.uea.ac.uk/cru/data/hulme/uk/lamb.htm
    pd <- which(abs(z) < f) 
    wtseries[pd] <- d[pd] #purely directional type
    pcyc <- which(abs(z) >= (2*f) & z >= 0) 
    wtseries[pcyc] <- 18 #purely cyclonic type
    pant <- which(abs(z) >= (2*f) & z < 0) 
    wtseries[pant] <- 1 #purely anticyclonic type
    hyb <- which(abs(z) >= f & abs(z) < (2*f)) #hybrid type
    hybant <- intersect(hyb, which(z < 0)) #anticyclonic
    hybcyc <- intersect(hyb, which(z >= 0)) #cyclonic
    for (i in 10:17){
      #directional anticyclonic
      wtseries[intersect(hybant, which(d == i))] <- i-8
      #mixed cyclonic
      wtseries[intersect(hybcyc, which(d == i))] <- i+9
    }
    indFlow <- which(abs(z) < 6 & f < 6) 
    wtseries[indFlow] <- 27 #indeterminate 
    
    wtseries.2<-wtseries[1:n[[1]]]
    
    lamb.list <- lapply(1:27, function(y){
      lamb.pattern <- which(wtseries.2 == y)
      #We subset the desired point from slp dataset: 
      grid.wt <- subsetDimension(grid.member, dimension = "time", indices = lamb.pattern)
      suppressMessages(clim<- climatology(grid.wt))
      return(clim)
    })
    
    lamb <- bindGrid(lamb.list, dimension = "time")
    
    memb[[1]]$index <- wtseries.2
    memb[[1]]$pattern <- lamb$Data
    attr(memb[[1]], "season") <- getSeason(grid)
    attr(memb[[1]], "dates_start") <- grid.member$Dates$start
    attr(memb[[1]], "dates_end") <- grid.member$Dates$end
    attr(memb[[1]], "centers") <- 27
    wt[[1]][[x]] <- memb
  }
  
  wt[[1]]$Variable <- grid$Variable
  attr(wt, "xCoords") <- grid$xyCoords$x
  attr(wt, "yCoords") <- grid$xyCoords$y
  attr(wt, "projection") <- attr(grid$xyCoords, "projection")
  
  return(wt)
  
}
