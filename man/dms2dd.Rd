% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo.R
\name{dms2dd}
\alias{dms2dd}
\title{Convert numeric coordinate vectors in degrees, minutes, and seconds to decimal degrees}
\usage{
dms2dd(x)
}
\arguments{
\item{x}{numeric vector of length 3 corresponding to degrees, minutes, and seconds}
}
\description{
Convert numeric coordinate vectors in degrees, minutes, and seconds to decimal degrees
}
\examples{
dt <- rbind(c(25, 12, 53.66), c(-80, 32, 00.61))
apply(dt, 1, function(x) dms2dd(x))
}
