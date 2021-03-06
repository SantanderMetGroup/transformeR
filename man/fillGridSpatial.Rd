% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fillGrid.R
\name{fillGridSpatial}
\alias{fillGridSpatial}
\title{Fill missing dates}
\usage{
fillGridSpatial(grid, lonLim = c(-180, 180), latLim = c(-90, 90))
}
\arguments{
\item{grid}{grid or station data}

\item{lonLim}{Optional. A vector with the minimum and maximum longitude boundaries to be filled with NAs. Default to lonLim = c(-180,180).}

\item{latLim}{Optional. A vector with the minimum and maximum latitude boundaries to be filled with NAs. Default to latLim = c(-90,90).}
}
\value{
A grid filled with NAs in the previously missing date positions
}
\description{
fill with NA missing dates in grids and station datasets
}
\author{
J. Baño-Medina
}
