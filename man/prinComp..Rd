% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prinComp.R
\name{prinComp.}
\alias{prinComp.}
\title{Principal Component Analysis}
\usage{
prinComp.(Xsc.list, n.eofs, v.exp, keep.orig, rot)
}
\arguments{
\item{Xsc.list}{A nested list of (scaled and centered) input variables (as returned by \code{\link{prinComp.scale}})}

\item{n.eofs}{n.eofs vector or NULL}

\item{v.exp}{explained variance vector or NULL}

\item{rot}{logical value indicating whether VARIMAX-Rotation should be performed.}
}
\value{
A list
}
\description{
Internal of prinComp, performing the PCA
}
\author{
J Bedia, M de Felice, A Casanueva
}
\keyword{internal}
