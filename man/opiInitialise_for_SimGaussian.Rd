% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SimGaussian.r
\name{opiInitialise_for_SimGaussian}
\alias{opiInitialise_for_SimGaussian}
\title{opiInitialize_for_SimGaussian}
\arguments{
\item{sd}{Standard deviation of Cumulative Gaussian.}

\item{maxStim}{The maximum stimuls value (0 dB) in cd/\eqn{\mbox{m}^2}{m^2}.}

\item{...}{Any other parameters you like, they are ignored.}
}
\value{
A list with elements:
\itemize{
\item \code{err} NULL if initialised, a message otherwise
}
}
\description{
Simulates responses using a Frequency of Seeing (FoS) curve.

The FoS is modelled as a cumulative Gaussian function with standard deviation
equal to \code{sd} as provided and the mean as the true threshold given as \code{tt}
\link{opiPresent}.
All values are in dB relative to \code{maxStim}.

This is for internal use only, use \code{\link[=opiInitialize]{opiInitialize()}}.
}
\examples{
    # Set up a simple simulation for white-on-white perimetry
chooseOpi("SimGaussian")
res <- opiInitialize(sd = 2.5)
if (!is.null(res$err))
  stop(paste("opiInitialize() failed:", res$msg))

}
