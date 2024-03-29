% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intersectGrid.R
\name{intersectGrid}
\alias{intersectGrid}
\title{Intersection of multiple grids}
\usage{
intersectGrid(
  ...,
  type = c("temporal", "spatial", "members"),
  which.return = 1
)
}
\arguments{
\item{...}{Input grids}

\item{type}{Character. Options are "temporal" (default) or "spatial".}

\item{which.return}{Integer of the index to specify which grids in "..." are to be returned.}
}
\value{
The grids indicated in \code{which.return}, encompassing the overlapping time period.
}
\description{
Takes multiple input grids and crops the overlapping part
}
\details{
If type = "members" the function directly loads the resulting objects to the global
environment and argument which.return is ignored (see examples).
}
\examples{
\donttest{
require(climate4R.datasets) 
data("EOBS_Iberia_tas")
a <- subsetGrid(EOBS_Iberia_tas, lonLim = c(-8,-1), latLim = c(37, 40))
b <- subsetGrid(EOBS_Iberia_tas, lonLim = c(-4,3), latLim = c(39, 43))
z <- intersectGrid(a, b, type = "spatial", which.return = 1)
 data("CFS_Iberia_tas")
 data("CFS_Iberia_pr")
 intersectGrid(CFS_Iberia_tas, CFS_Iberia_pr, type = "members")
}
}
\seealso{
\code{\link{subsetGrid}}

Other subsetting: 
\code{\link{getTemporalIntersection}()},
\code{\link{intersectGrid.spatial}()},
\code{\link{intersectGrid.time}()},
\code{\link{limitArea}()},
\code{\link{subsetCluster}()},
\code{\link{subsetDimension}()},
\code{\link{subsetGrid}()},
\code{\link{subsetMembers}()},
\code{\link{subsetRuntime}()},
\code{\link{subsetSeason}()},
\code{\link{subsetSpatial}()},
\code{\link{subsetStation}()},
\code{\link{subsetVar}()},
\code{\link{subsetYears}()}
}
\author{
M Iturbide
}
\concept{subsetting}
