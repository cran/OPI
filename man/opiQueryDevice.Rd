\name{opiQueryDevice}
\alias{opiQueryDevice}
\title{
Query device using OPI.
}
\description{
Generic function for getting details of the chosen OPI implementation that is set with
\code{chooseOpi()}.
}
\usage{
opiQueryDevice(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{\dots}{Implementation specific parameters. See details.}
}
%\details{
%}

\value{
Returns implementation dependant data.
}
\references{
http://perimetry.org/OPI
}
\author{
Andrew Tuprin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{chooseOpi}}
}

\examples{
chooseOpi("SimGaussian")
if (!is.null(opiInitialize(sd=2)))
    stop("opiInitialize failed")
print(opiQueryDevice())
}

\keyword{ misc }
