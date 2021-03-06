% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pix2deg.r
\name{degToPix}
\alias{degToPix}
\title{Convert degrees to pixels for machine 'machine'}
\usage{
degToPix(xy, machine = "compass")
}
\arguments{
\item{xy}{a 2 element vector c(x,y) where x and y are in degrees on a visual field with the usual conventions}

\item{machine}{"compass" or ...?}
}
\value{
xy converted to pixels (top-left is (0,0)) for the machine or \code{NA} if machine is unknown
}
\description{
Convert degrees to pixels for machine 'machine'
}
\examples{
degToPix(c(0, 0), machine="compass")  # c(960, 960) pixels
degToPix(c(-15, 2)) # c(495, 898) pixels

}
