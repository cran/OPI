\name{opiInitialize}
\alias{opiInitialize}
\title{
Initialize OPI.
}
\description{
Generic function for initialization of the chosen OPI implementation that is set with
\code{chooseOpi()}.
}
\usage{
opiInitialize(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{\dots}{Implementation specific parameters. See details.}
}
\details{
\subsection{SimHenson}{
    \code{opiInitialize(type="C", cap=6)}

    If the chosen OPI implementation is \code{SimHenson}, then
  \code{type} can be one of:
    \code{"N"}, for normal patients; 
    \code{"G"}, for POAG patients; and
    \code{"C"}, for a combination. See Table 1 in Henson et al (2000).
    \code{cap} is the maximum
    standard deviation value that the simulator will use for
    the slope/spread of the psychometric function.
}
\subsection{SimGaussian}{
    \code{opiInitialize(sd)}

    If the chosen OPI implementation is \code{SimGaussian}, then \code{sd}
    is the standard deviation value that the simulator will use for 
    the slope/spread of the psychometric function.
}
\subsection{Octopus900}{
    \code{opiInitialize(eyeSuiteJarLocation=NA, eyeSuiteSettingsLocation=NA, eye=NA)}

    If the chosen OPI implementation is \code{Octopus900}, then you must 
    specify two directory/folder names and the eye to be tested.

    \code{eyeSuiteJarLocation} is the folder name containing the EyeSuite jar files,
    and should include the trainling slash.

    \code{eyeSuiteSettingsLocation} is the folder name containing the EyeSuite setting files,
    and should include the trainling slash.

    \code{eye} must be either "left" or "right".
}
}

\value{
Returns NULL if initialization succeeded, 
otherwise an implementation dependant error.

\subsection{Octopus900}{
Returns NULL if successful, 1 if Octopus900 is already initialised by a previous call to 
\code{opiInitialize}, and 2 if some error occured that prevented ininitialisation.

The default background and stimulus setup is to white-on-white perimetry. Use \code{opiSetBackground}
to change the background and stimulus colors.
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
\code{\link{chooseOpi}},
\code{\link{opiSetBackground}},
\code{\link{opiClose}},
\code{\link{opiPresent}}
}

\examples{
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
    stop("opiInitialize failed")

chooseOpi("SimGaussian")
if (!is.null(opiInitialize(sd=2)))
    stop("opiInitialize failed")

\dontrun{
chooseOpi("Octopus900")
if (!is.null(opiInitialize(
        "C:/EyeSuite/", 
        "C:/Documents and Settings/All Users/HAAG-STREIT/EyeSuite/",
        "left")))
    stop("opiInitialize failed")
}
}

\keyword{ misc }
