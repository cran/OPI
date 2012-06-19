\name{chooseOpi}
\alias{chooseOpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Choose an implementation of the OPI.
}
\description{
Chooses an implementation of the OPI to use.
}
\usage{
chooseOpi(opiImplementation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{opiImplementation}{A character string that is one of the following.
    \itemize{
    \item\code{"SimHenson"} for a simulator that uses a cummulative gaussian 
                psychometric function with standard deviation 
                according to Henson et al (2000) where variability 
                increases as true threshold (Humphrey dB) value 
                decreases. 
    \item\code{"SimGaussian"} for a simulator that uses a cummulative gaussian 
                psychometric function with 
                standard deviation supplied in opiInitialize(). 
    \item\code{"Octopus900"} for interfacing with the Octopus 900. 
    \item\code{"HEP"}        for interfacing with the Heidelberg Edge Perimeter.
    \item\code{NULL}         print a list of available OPI implementations.
    }
}
}
\value{ Returns TRUE if successful, FALSE otherwise.}

\references{
David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and
Alec Ansons.  
Response Variability in the Visual Field: Comparison of Optic Neuritis,
Glaucoma, Ocular Hypertension, and Normal Eyes.
Investigative Ophthalmology & Visual Science, February 2000, Vol. 41(2).

http://perimetry.org/OPI

}

\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}

%\note{ }

\examples{
    if(!chooseOpi("SimHenson"))
        warnings()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
