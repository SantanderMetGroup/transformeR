#' @title Grid containing CORDEX data (KNMI) of daily precipitation for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{CORDEX_Iberia_tp <- loadGridData(dataset = "http://www.meteo.unican.es/tds5/dodsC/cordex/EUR-11/KNMI/ECMWF-ERAINT_evaluation_r1i1p1_RACMO22E_v1_day.ncml",
#' var = "pr", years = years, lonLim = lonLim, latLim = latLim, season = season)}\cr
#' \code{CORDEX_Iberia_tp$Data <- CORDEX_Iberia_tp$Data * 86400}\cr 
#' \code{attr(CORDEX_Iberia_tp$Variable, "units") <- "mm"}\cr
#'  
#' @format A grid
#' @name CORDEX_Iberia_tp
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("CORDEX_Iberia_tp")
#' plotClimatology(climatology(CORDEX_Iberia_tp),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL

#' @title Grid containing CORDEX data (KNMI) of daily mean temperature for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{CORDEX_Iberia_tas <- loadGridData(dataset = "http://www.meteo.unican.es/tds5/dodsC/cordex/EUR-11/KNMI/ECMWF-ERAINT_evaluation_r1i1p1_RACMO22E_v1_day.ncml",
#' var = "tas", years = years, lonLim = lonLim, latLim = latLim, season = season)}\cr
#' \code{CORDEX_Iberia_tas$Data <- CORDEX_Iberia_tas$Data - 273.15}\cr 
#' \code{attr(CORDEX_Iberia_tas$Variable, "units") <- "Celsius"}\cr
#'  
#' @format A grid
#' @name CORDEX_Iberia_tas
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("CORDEX_Iberia_tas")
#' plotClimatology(climatology(CORDEX_Iberia_tas),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL


#' @title Station data containing VALUE daily data of precipitation for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained by means of function \code{\link[loadeR]{loadStationData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' \code{VALUE_Iberia_tp <- loadStationData(dataset = "data/VALUE_ECA_86_v2.zip",
#' var = "precip", years = years, lonLim = lonLim, latLim = latLim, season = season)}\cr 
#' 
#' @format Station data
#' @name VALUE_Iberia_tp
#' @docType data
#' @keywords Station data
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("VALUE_Iberia_tp")
#' plotClimatology(climatology(VALUE_Iberia_tp),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL



#' @title Station data containing VALUE daily data of mean temperature for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained by means of function \code{\link[loadeR]{loadStationData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' \code{VALUE_Iberia_tas <- loadStationData(dataset = "data/VALUE_ECA_86_v2.zip",
#' var = "tmean", years = years, lonLim = lonLim, latLim = latLim, season = season)}\cr 
#' 
#' @format Station data
#' @name VALUE_Iberia_tas
#' @docType data
#' @keywords Station data
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("VALUE_Iberia_tas")
#' plotClimatology(climatology(VALUE_Iberia_tas),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL

#' @title Grid containing CMIP5 data (CNRM) of daily sea level pressure for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{CMIP5_Iberia_psl <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CNRM-CERFACS/CNRM-CM5/historical/day/cnrm-cerfacs_cnrm-cm5_historical_r1i1p1.ncml",
#'                     var = "psl", years = years, lonLim = lonLim, latLim = latLim, season = season,
#'                      time = "DD", aggr.d = "mean")}\cr 
#' @format A grid
#' @name CMIP5_Iberia_psl
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("CMIP5_Iberia_psl")
#' plotClimatology(climatology(CMIP5_Iberia_psl),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL

#' @title Grid containing CMIP5 data (CNRM) of daily air temperature at 850mb for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{CMIP5_Iberia_ta850 <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CNRM-CERFACS/CNRM-CM5/historical/day/cnrm-cerfacs_cnrm-cm5_historical_r1i1p1.ncml",
#'                     var = "ta@85000", years = years, lonLim = lonLim, latLim = latLim, season = season,
#'                      time = "DD", aggr.d = "mean")}\cr 
#' @format A grid
#' @name CMIP5_Iberia_ta850
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("CMIP5_Iberia_ta850")
#' plotClimatology(climatology(CMIP5_Iberia_ta850),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL



#' @title Grid containing CMIP5 data (CNRM) of daily specific humidity at 850mb for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{CMIP5_Iberia_hus850 <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CNRM-CERFACS/CNRM-CM5/historical/day/cnrm-cerfacs_cnrm-cm5_historical_r1i1p1.ncml",
#'                     var = "hus@85000", years = years, lonLim = lonLim, latLim = latLim, season = season,
#'                      time = "DD", aggr.d = "mean")}\cr 
#' @format A grid
#' @name CMIP5_Iberia_hus850
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("CMIP5_Iberia_hus850")
#' plotClimatology(climatology(CMIP5_Iberia_hus850),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL



#' @title Grid containing CMIP5 data (CNRM) of daily precipitation for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{CMIP5_Iberia_tp <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CNRM-CERFACS/CNRM-CM5/historical/day/cnrm-cerfacs_cnrm-cm5_historical_r1i1p1.ncml", 
#'                                       var = "pr", 
#'                                       years = years, 
#'                                       lonLim = lonLim, 
#'                                       latLim = latLim, 
#'                                       season = season)}\cr 
#' @format A grid
#' @name CMIP5_Iberia_tp
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("CMIP5_Iberia_tp")
#' plotClimatology(climatology(CMIP5_Iberia_tp),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL

#' @title Grid containing CMIP5 data (CNRM) of daily mean temperature for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{CMIP5_Iberia_tas <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/cmip5/CNRM-CERFACS/CNRM-CM5/historical/day/cnrm-cerfacs_cnrm-cm5_historical_r1i1p1.ncml", 
#'                                       var = "tas", 
#'                                       years = years, 
#'                                       lonLim = lonLim, 
#'                                       latLim = latLim, 
#'                                       season = season)}\cr 
#' @format A grid
#' @name CMIP5_Iberia_tas
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("CMIP5_Iberia_tas")
#' plotClimatology(climatology(CMIP5_Iberia_tas),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL


#' @title Multimember grid containing CFSv2 seasonal forecast data of daily mean temperature at 850mb for
#' the Iberian Peninsula (DFJ, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' \code{CFS_Iberia_ta850 <- loadECOMS(dataset = "CFSv2_seasonal",
#' var = "ta@850", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean", leadMonth = 1, members = 1:9)}
#' 
#' 
#' @format A multimember grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @references 
#' Saha, S. \emph{et al.}, 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @name CFS_Iberia_ta850
#' @examples
#' data(CFS_Iberia_ta850)
#' plotClimatology(climatology(CFS_Iberia_ta850, clim.fun = list(FUN = sum)),
#'                 backdrop.theme = "coastline")
NULL



#' @title Multimember grid containing CFSv2 seasonal forecast data of daily sea level pressure for
#' the Iberian Peninsula (DFJ, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' \code{CFS_Iberia_psl <- loadECOMS(dataset = "CFSv2_seasonal",
#' var = "psl", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean", leadMonth = 1, members = 1:9)}
#' 
#' 
#' @format A multimember grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @references 
#' Saha, S. \emph{et al.}, 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @name CFS_Iberia_psl
#' @examples
#' data(CFS_Iberia_psl)
#' plotClimatology(climatology(CFS_Iberia_psl, clim.fun = list(FUN = sum)),
#'                 backdrop.theme = "coastline")
NULL




#' @title Multimember grid containing CFSv2 seasonal forecast data of daily specific humidity at 850mb for
#' the Iberian Peninsula (DFJ, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' \code{CFS_Iberia_hus@850 <- loadECOMS(dataset = "CFSv2_seasonal",
#' var = "huss@850", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean", leadMonth = 1, members = 1:9)}
#' 
#' 
#' @format A multimember grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @references 
#' Saha, S. \emph{et al.}, 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @name CFS_Iberia_hus850
#' @examples
#' data(CFS_Iberia_hus850)
#' plotClimatology(climatology(CFS_Iberia_hus850, clim.fun = list(FUN = sum)),
#'                 backdrop.theme = "coastline")
NULL

#' @title Multimember grid containing CFSv2 seasonal forecast data of daily precipitation for
#' the Iberian Peninsula (DFJ, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' \code{CFS_Iberia_tp <- loadECOMS(dataset = "CFSv2_seasonal",
#' var = "tp", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean", leadMonth = 1, members = 1:9)}
#' 
#' 
#' @format A multimember grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @references 
#' Saha, S. \emph{et al.}, 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @name CFS_Iberia_tp
#' @examples
#' data(CFS_Iberia_tp)
#' plotClimatology(climatology(CFS_Iberia_tp, clim.fun = list(FUN = sum)),
#'                 backdrop.theme = "coastline")
NULL

#' @title Multimember grid containing CFSv2 seasonal forecast data of daily mean temperature for
#' the Iberian Peninsula (DFJ, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' \code{CFS_Iberia_tas <- loadECOMS(dataset = "CFSv2_seasonal",
#' var = "tas", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean", leadMonth = 1, members = 1:9)}
#' 
#' 
#' @format A multimember grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @references 
#' Saha, S. \emph{et al.}, 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @name CFS_Iberia_tas
#' @examples
#' data(CFS_Iberia_tas)
#' plotClimatology(climatology(CFS_Iberia_tas, clim.fun = list(FUN = sum)),
#'                 backdrop.theme = "coastline")
NULL


#' @title Grid containing NCEP reanalysis data of daily air temperature at 850mb for the 
#' Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' 
#' \code{NCEP_Iberia_ta850 <- loadECOMS(dataset = "NCEP_reanalysis1",
#' var = "ta@850", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean")}
#' 
#' 
#' @format A grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @name NCEP_Iberia_ta850
#' @examples
#' data(NCEP_Iberia_ta850)
#' plotClimatology(climatology(NCEP_Iberia_ta850),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL


#' @title Grid containing NCEP reanalysis data of daily specific humidity at 850mb for the 
#' Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' 
#' \code{NCEP_Iberia_hus850 <- loadECOMS(dataset = "NCEP_reanalysis1",
#' var = "hus@850", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean")}
#' 
#' 
#' @format A grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @name NCEP_Iberia_hus850
#' @examples
#' data(NCEP_Iberia_hus850)
#' plotClimatology(climatology(NCEP_Iberia_hus850),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL



#' @title Grid containing NCEP reanalysis data of daily sea level pressure for the 
#' Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' 
#' \code{NCEP_Iberia_psl <- loadECOMS(dataset = "NCEP_reanalysis1",
#' var = "psl", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean")}
#' 
#' 
#' @format A grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @name NCEP_Iberia_psl
#' @examples
#' data(NCEP_Iberia_psl)
#' plotClimatology(climatology(NCEP_Iberia_psl),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL



#' @title Grid containing NCEP reanalysis data of daily mean temperature for the 
#' Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' 
#' \code{NCEP_Iberia_tas <- loadECOMS(dataset = "NCEP_reanalysis1",
#' var = "tas", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean")}
#' 
#' 
#' @format A grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @name NCEP_Iberia_tas
#' @examples
#' data(NCEP_Iberia_tas)
#' plotClimatology(climatology(NCEP_Iberia_tas),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL

#' @title Grid containing NCEP reanalysis data of daily precipitation for the 
#' Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the ECOMS-UDG server 
#' (\url{http://www.meteo.unican.es/ecoms-udg}, 
#' log in is requiered, see \code{\link[loadeR]{loginUDG}}) 
#' by means of function \code{\link[loadeR.ECOMS]{loadECOMS}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR.ECOMS}{\code{loadeR.ECOMS}}) 
#' in the following manner:
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#' 
#' 
#' \code{NCEP_Iberia_tp <- loadECOMS(dataset = "NCEP_reanalysis1",
#' var = "pr", years = years, lonLim = lonLim, latLim = latLim, season = season,
#' time = "DD", aggr.d = "mean")}
#' 
#' 
#' @format A grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @name NCEP_Iberia_tp
#' @examples
#' data(NCEP_Iberia_tp)
#' plotClimatology(climatology(NCEP_Iberia_tp),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL





#' @title Grid containing E-OBS daily data of mean temperature for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap}, 
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{EOBS_Iberia_tas <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/eobs/e-obs_v14_0.50regular.ncml", 
#'                                       var = "tg", 
#'                                       years = years, 
#'                                       lonLim = lonLim, 
#'                                       latLim = latLim, 
#'                                       season = season)}\cr 
#' @format A grid
#' @name EOBS_Iberia_tas
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("EOBS_Iberia_tas")
#' plotClimatology(climatology(EOBS_Iberia_tas),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL





#' @title Grid containing E-OBS daily data of precipitation for the Iberian Peninsula (DJF, 1983-2002).
#' @description This R data object was obtained from the UDG server (\url{http://www.meteo.unican.es/udg-tap},
#' log in is requiered, 
#' see \code{\link[loadeR]{loginUDG}}) by means of function \code{\link[loadeR]{loadGridData}} 
#' (package \href{https://github.com/SantanderMetGroup/loadeR}{\code{loadeR}}) in the following manner:
#' 
#' 
#' \code{years <- 1983:2002}\cr 
#' \code{season <- c(12,1,2)}\cr 
#' \code{lonLim <- c(-10, 4)}\cr 
#' \code{latLim <- c(35, 44)}\cr 
#' 
#' 
#' \code{loginUDG("username", "pasword")}\cr 
#'  
#' \code{EOBS_Iberia_tp <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/eobs/e-obs_v14_0.50regular.ncml", 
#'                                       var = "rr", 
#'                                       years = years, 
#'                                       lonLim = lonLim, 
#'                                       latLim = latLim, 
#'                                       season = season)}\cr 
#' 
#' @format A grid
#' @name EOBS_Iberia_tp
#' @docType data
#' @keywords Gridded observations
#' @source  \url{http://www.meteo.unican.es/udg-tap}
#' @seealso \code{\link{makeMultiGrid}}, \code{\link[loadeR]{loadGridData}}
#' @examples 
#' data("EOBS_Iberia_tp")
#' total.precip.annual <- aggregateGrid(EOBS_Iberia_tp,
#'                                      aggr.m = list(FUN = sum),
#'                                      aggr.y = list(FUN = sum))
#' plotClimatology(climatology(total.precip.annual), 
#'                 backdrop.theme = "countries", scales = list(draw = TRUE),
#'                 main = "Mean Total DJF precip (mm) 1998-2000")
NULL






#' @title Grid containing NCEP reanalysis data of sea-level pressure for the Iberian Peninsula.
#' @description The data correspond to the wintertime (DJF) period 1991-2010, and it consists of daily mean data, computed as the
#' mean of the four 6-hourly model outputs. 
#' @format A grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @name iberia_ncep_psl
#' @examples
#' data(iberia_ncep_psl)
#' plotClimatology(climatology(iberia_ncep_psl),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL

#' @title Grid containing NCEP reanalysis data of air temperature at 850mb for the Iberian Peninsula.
#' @description The data correspond to the wintertime (DJF) period 1991-2010, and it consists of daily mean data 
#' @format A grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @name iberia_ncep_ta850
#' @examples
#' data(iberia_ncep_ta850)
#' plotClimatology(climatology(iberia_ncep_ta850),
#'                 backdrop.theme = "countries", scales = list(draw = TRUE))
NULL

#' @title Multimember grid containing a seasonal forecast of maximum surface temperature for Europe
#' @description CFSv2 forecast of maximum daily temperature for July 2001 over Europe. Lead-month 2, first 9 members.
#' @format A multimember grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @references 
#' Saha, S. \emph{et al.}, 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @name tasmax_forecast
#' @examples
#' data(tasmax_forecast)
#' plotClimatology(climatology(tasmax_forecast), backdrop.theme = "coastline")
NULL

#' @title Multimember grid containing a seasonal forecast of minimum surface temperature for Europe
#' @description CFSv2 forecast of minimum daily temperature for July 2001 over Europe. Lead-month 2, first 9 members.
#' @format A multimember grid
#' @source \url{http://www.meteo.unican.es/ecoms-udg}
#' @references 
#' Saha, S. \emph{et al.}, 2013. The NCEP Climate Forecast System Version 2. J Clim 130925135638001. doi:10.1175/JCLI-D-12-00823.1
#' @name tasmin_forecast
#' @examples
#' data(tasmin_forecast)
#' plotClimatology(climatology(tasmin_forecast), backdrop.theme = "coastline")
NULL





#' @title Station daily precipitation dataset in Iberia
#' @description Station data from the VALUE_ECA_86_v2 dataset containing daily precipitation for stations in the Iberian Peninsula.
#' @format Station data
#' @name VALUE_Iberia_tp
#' @docType data
#' @source  Subset of VALUE station data. Full dataset is accessible 
#' for download in \url{http://meteo.unican.es/work/downscaler/data/VALUE_ECA_86_v2.tar.gz}.
NULL

#' @title Station mean temperature dataset
#' @description Station data from the VALUE_ECA_86_v2 dataset containing daily mean temperature for for stations in the Iberian Peninsula.
#' @format Station data
#' @name VALUE_Iberia_tas
#' @docType data
#' @source  Subset of VALUE station data. Full dataset is accessible 
#' for download in \url{http://meteo.unican.es/work/downscaler/data/VALUE_ECA_86_v2.tar.gz}.
NULL


#' @title Station daily precipitation data
#' @description Station data from the VALUE_ECA_86_v2 dataset containing daily precipitation for the Igueldo-SanSebastian station. 
#' @format Station data
#' @name VALUE_Igueldo_tp
#' @docType data
#' @source  Subset of VALUE station data. Full dataset is accessible 
#' for download in \url{http://meteo.unican.es/work/downscaler/data/VALUE_ECA_86_v2.tar.gz}.
NULL

#' @title Grid containing NCEP reanalysis data of precipitation for the Iberian Peninsula.
#' @description NCEP_Iberia_tp is a grid object returned by loadECOMS from package loadeR.ECOMS
#' @format A grid
#' @name NCEP_Iberia_tp
#' @docType data
#' @keywords NCEP reanalysis
#' @source  subset of NCEP reanalysis data, which is accessible through the \strong{ECOMS User Data Gateway (ECOMS-UDG)} 
NULL