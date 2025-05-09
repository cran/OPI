% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImoVifa.r
\name{opiClose_for_ImoVifa}
\alias{opiClose_for_ImoVifa}
\title{Implementation of opiClose for the ImoVifa machine.}
\value{
A list containing:
\itemize{
\item \code{err} \code{NULL} if there was no error, a string message if there is an error.
}
}
\description{
This is for internal use only. Use \code{\link[=opiClose]{opiClose()}} after
\code{chooseOPI("ImoVifa")} to call this function.
}
\examples{
\dontrun{
chooseOpi("ImoVifa")
opiInitialise(list(port = 50001, ip = "localhost"))
opiSetup(list(eye = "BOTH"))
result <- opiClose()
}

}
\seealso{
\code{\link[=opiClose]{opiClose()}}
}
