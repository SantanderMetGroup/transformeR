% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prinComp.R
\name{prinComp.scale}
\alias{prinComp.scale}
\title{Local/Global grid scaling}
\usage{
prinComp.scale(var.list, scaling)
}
\arguments{
\item{var.list}{A named nested list (variables-members) of fields in the form of 2D matrices (after \code{array3Dto2Dmat})}

\item{scaling}{Character vector of scaling types (same length as \code{var.list})}
}
\value{
A named list of the same structure as the input \code{var.list} with the scaled (and centered) fields (the
names of the elements of the list are the variable names).
}
\description{
Scale a grid prior to PCA analysis (internal of \code{prinComp})
}
\details{
The \code{"gridbox"} scaling performs the scaling independently for each grid/station point (the mean and sd is calculated
sepparately for each point). On the contrary, the \code{"field"} approach substracts the same global mean and divides by the global sd
considering the whole field.
}
\author{
J Bedia
}
\keyword{internal}
