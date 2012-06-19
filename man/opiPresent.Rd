\name{opiPresent}
\alias{opiPresent}
\title{
Use OPI to present stimulus.
}
\description{
Generic function for presentation of stimulus stim. 
Depending on your choice of OPI implementation set using 
\code{chooseOpi()}, 
different parameters are available for \code{opiPresent}.
}
\usage{
opiPresent(stim, nextStim=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stim}{
    A list of class \code{opiStaticStimulus}, \code{opiKineticStimulus}, or
        \code{opiTemporalStimulus}. 
  }
  \item{nextStim}{
    As for \code{stim}, but the next presentation to be made. 
    This might be useful
    on some machines, particularly projector based systems, where preparations
    for the next presentation can be made while waiting for a response to the
    current.
  }
  \item{\dots}{
    Parameters specific to your chosen opi implementation.
  }
}
\details{
\code{opiPresent} 
is blocking in that it will not return until either a response
is obtained, or at least the responseWindow milliseconds has expired. (Note
that more time might have expired.) Specifying \code{nextStim} 
allows the implementing
machine to use the time waiting for a 
response to \code{stim} to make preparations
for the next stimuli. (For example retargeting the projector or moving
aperture and/or filter wheels.) 
There is no guarantee that the next call to
\code{opiPresent} will have \code{nextStim} as the first argument; this 
should be checked by the machine specific implementations.

\subsection{SimHenson}{

\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}

If the chosen OPI implementation is \code{SimHenson}, then
the response to a stimuli is determined by sampling from a
Frequency-of-Seeing (FoS) curve (also known as the psychometric
function) with formula \deqn{\mbox{fpr}+(1-\mbox{fpr}-\mbox{fnr})(1-\mbox{pnorm}(x, \mbox{tt},
\mbox{pxVar})),}{\code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, pxVar))},}
where \eqn{x}{\code{x}} is the stimulus value in Humphrey dB, and pxVar is
\deqn{\min\left(\mbox{simH.global.cap}, e^{A\times\mbox{tt}+B}\right).}{\code{min(simH.cap, exp(A*tt + B))}.} 
The ceiling \code{simH.global.cap}
is set with the call to \code{opiInitialize}, and \code{A} and \code{B}
are from Table 1 in Henson et al (2000). Which values are used is
determined by \code{simH.type} which is also set in the call to
\code{opiInitialize}.
}


\subsection{SimGaussian}{

\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}

If the chosen OPI implementation is \code{SimGaussian}, then
the response to a stimuli as determined by sampling from a
Frequency-of-Seeing (FoS) curve (also known as the psychometric
function) with formula \code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, simG.global.sd))},
where \code{x} is the stimulus value in Humphrey dB, and \code{simG.global.sd} is
set with \code{opiInitialize}.
}

}
\value{
A list containing
\item{err }{\code{NULL} if no error occurred, otherwise a machine specific error
message.
This should include errors when the specified size cannot be achieved by
the device (for example, in a projection system with an aperture wheel of
predefined sizes.) If \code{stim} is \code{NULL}, then \code{err} contains the status of the
machine.}
\item{seen }{\code{TRUE} if a response was detected in the allowed
\code{responseWindow}, \code{FALSE} otherwise.}
\item{time}{The time in milliseconds from the onset (or offset, machine
specific) of the presentation
until the response from the subject if \code{seen} is \code{TRUE}. 
If \code{seen} is \code{FALSE}, this
value is undefined.
}
}
\references{
http://perimetry.org/OPI

David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and
Alec Ansons.  
Response Variability in the Visual Field: Comparison of Optic Neuritis,
Glaucoma, Ocular Hypertension, and Normal Eyes.
Investigative Ophthalmology & Visual Science, February 2000, Vol. 41(2).
}
\author{
Andrew Tuprin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{opiStaticStimulus}}, 
\code{\link{opiKineticStimulus}}, 
\code{\link{opiTemporalStimulus}}, 
\code{\link{chooseOpi}}
}

\examples{
    # Stimulus is Size III white-on-white as in the HFA
makeStim <- function(db, n) { 
    s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
             duration=200, responseWindow=1500)
    class(s) <- "opiStaticStimulus"

    return(s)
}
chooseOpi("SimHenson")
if (!is.null(opiInitialize("C", 6)))
    stop("opiInitialize failed")

result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)

if (!is.null(opiClose()))
    warning("opiClose() failed")
}

\keyword{ misc }
