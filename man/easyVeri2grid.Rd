% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/easyVeri2grid.R
\name{easyVeri2grid}
\alias{easyVeri2grid}
\title{easyVerification matrix to climatological grid conversion}
\usage{
easyVeri2grid(easyVeri.mat, obs.grid, verifun = NULL)
}
\arguments{
\item{easyVeri.mat}{A matrix containing the verification measure,
as returned by \code{\link[easyVerification]{veriApply}}}

\item{obs.grid}{The grid containing the verifying reference used in the call to \code{veryApply}
producing the score matrix.}

\item{verifun}{Optional. Character string indicating the value of the \code{verifun} argument. Just for 
a better traceability and metadata completeness.}
}
\value{
A climatological grid.
}
\description{
Convert a xyz-type verification matrix as returned by \code{veriApply} to a climatological grid
}
\seealso{
\code{\link{climatology}}, \code{\link[visualizeR]{spatialPlot}}.

Other easyVerification.bridges: 
\code{\link{easyVeri.signif}()}
}
\author{
J. Bedia
}
\concept{easyVerification.bridges}
