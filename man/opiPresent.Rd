% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opi.r, R/compassClient.r, R/displayClient.r,
%   R/daydreamClient.r, R/imoClient.r, R/kowaAP7000Client.r, R/octopus600.r,
%   R/octopus900Client.r, R/phoneHMD.r, R/simG.r, R/simH.r, R/simH_RT.r,
%   R/simNo.r, R/simYes.r
\name{opiPresent}
\alias{opiPresent}
\alias{compass.opiPresent}
\alias{display.opiPresent}
\alias{daydream.opiPresent}
\alias{imo.opiPresent}
\alias{kowaAP7000.opiPresent}
\alias{octo600.opiPresent}
\alias{octo900.opiPresentF310}
\alias{PhoneHMD.opiPresent}
\alias{simG.opiPresent}
\alias{simH.opiPresent}
\alias{simH_RT.opiPresent}
\alias{simNo.opiPresent}
\alias{simYes.opiPresent}
\title{Use OPI to present stimulus}
\usage{
opiPresent(stim, nextStim = NULL, ...)

compass.opiPresent(stim, nextStim = NULL)

display.opiPresent(stim, nextStim = NULL)

daydream.opiPresent(stim, nextStim = NULL)

imo.opiPresent(stim, nextStim = NULL)

kowaAP7000.opiPresent(stim, nextStim = NULL)

octo600.opiPresent(stim, nextStim = NULL)

octo900.opiPresentF310(stim, nextStim = NULL)

PhoneHMD.opiPresent(stim, nextStim = NULL)

simG.opiPresent(stim, nextStim = NULL, fpr = 0.03, fnr = 0.01, tt = 30)

simH.opiPresent(
  stim,
  nextStim = NULL,
  fpr = 0.03,
  fnr = 0.01,
  tt = 30,
  criteria = 0.97,
  rt_shape = 5.3,
  rt_rate = 1.4,
  rt_scale = 0.1
)

simH_RT.opiPresent(
  stim,
  nextStim = NULL,
  fpr = 0.03,
  fnr = 0.01,
  tt = 30,
  notSeenToSeen = TRUE
)

simNo.opiPresent(stim, nextStim = NULL)

simYes.opiPresent(stim, nextStim = NULL)
}
\arguments{
\item{stim}{a list of class \code{\link{opiStaticStimulus}},
\code{\link{opiKineticStimulus}}, or \code{\link{opiTemporalStimulus}} to be presented.}

\item{nextStim}{unused - included for compliance with OPI standard.}

\item{...}{Parameters specific to your chosen opi implementation}

\item{fpr}{false positive rate for OPI implementation "SimHenson"}

\item{fnr}{false negative rate for OPI implementation "SimHenson"}

\item{tt}{SOMETHING for OPI implementation "SimHenson"}

\item{criteria}{CRITERIA for OPI implementation "SimHenson"}

\item{rt_shape}{response time shape parameter for OPI implementation "SimHenson"}

\item{rt_rate}{response time rate parameter  for OPI implementation "SimHenson"}

\item{rt_scale}{response time scale parameter for OPI implementation "SimHenson"}

\item{notSeenToSeen}{SOMETHING for OPI implementation "SimHensonRT"}
}
\value{
A list containing
\itemize{
\item \code{err} \code{NULL} if no error occurred, otherwise a machine-specific error message.
This should include errors when the specified size cannot be achieved by
the device (for example, in a projection system with an aperture wheel of
predefined sizes.) If \code{stim} is \code{NULL}, then \code{err} contains
the status of the machine.
\item \code{seen} \code{TRUE} if a response was detected in the allowed
\code{responseWindow}, \code{FALSE} otherwise. (Note, see
Octopus900F310 above).
\item \code{time} The time in milliseconds from the onset (or offset,
machine-specific) of the presentation until the response from the subject
if \code{seen} is \code{TRUE}. If \code{seen} is \code{FALSE}, this value is undefined.
For kinetic perimetry on the O900, this value is unknown... (what does this mean!?)
}
\subsection{O600}{

\code{answer} only returned for \code{Octopus600}. Can be the following values:
\itemize{
\item 0 = stimulus not seen;
\item 1 = stimulus seen;
\item 132 = Response button was pressed before stimulus presentation (Patient needs a break - hold on examination);
\item 36 = Eye is closed before stimulus presentation;
\item 68 = Fixation lost before stimulus presentation (pupil center is out of green window in video image);
\item 260 = Forehead rest lost before stimulus presentation;
\item 516 = Fast Eye movements before stimulus presentation;
\item 258 = Forehead rest lost during stimulus presentation;
\item 66 = Fixation lost during stimulus presentation (pupil center is out of green window in video image);
\item 34 = Eye was closed during stimulus presentation;
\item 18 = Patient answer was too early (<=100ms after stimulus presentation) - lucky punch;
\item 514 = Fast Eye movements during stimulus presentation
}
}

\subsection{Kowa AP7000}{
\itemize{
\item \code{pupilX} Only returned for KowaAP7000 (in pixels) and an
opiStaticStimulus or O900 (in degrees) and static/kinetic if gazeFeed==1.
x-coordinate of centre of pupil during presentation.
\item \code{pupilY} Only returned for KowaAP7000 (in pixels) and an
opiStaticStimulus or O900 (in degrees) and static/kinetic if gazeFeed==1.
y-coordinate of centre of pupil during presentation.
\item \code{purkinjeX} Only returned for KowaAP7000 and an opiStaticStimulus.  x-coordinate of centre of Purkinje Image in pixels during presentation.
\item \code{purkinjeY} Only returned for KowaAP7000 and an opiStaticStimulus.  y-coordinate of centre of Purkinje Image in pixels during presentation.
}
}

\subsection{Kowa AP7000 and Octopus O900}{
\itemize{
\item \code{x} Only returned for KowaAP7000 or Octopus900 and an opiKineticStimulus. x coordinate of stimuli when button is pressed.
\item \code{y} Only returned for KowaAP7000 or Octopus900 and an opiKineticStimulus. y coordinate of stimuli when button is pressed.
}
}

\subsection{Compass}{
\itemize{
\item \code{time_rec} Time since epoch that the opiPresent command was received by the Compass in ms.
\item \code{time_hw} Hardware time of button press or response window expired (integer ms).
To get the hardware time that a presentation began, subtract
responseWindow from \code{th} (for aligning with fixation data returned
by \code{opiClose()}.
\item \code{time_resp} Time since epoch that the response was received or response window expired (in ms).
\item \code{num_track_events} The number of tracking events associated with this presentation.
\item \code{num_motor_fails} The number of time the motor could not keep pace with eye movements.
\item \code{pupil_diam} The diameter of the pupil on millimeters on presentation.
\item \code{loc_x} The x location in pixels of the presentation on the retinal image returned by \code{opiInitialize}.
\item \code{loc_y} The y location in pixels of the presentation on the retinal image returned by \code{opiInitialize}.
}
}

\subsection{Compass}{

A list containing
\itemize{
\item \code{err} 0 all clear, >= 1 some error codes (eg cannot track, etc) (integer)
\item \code{seen} \code{FALSE} for not seen, \code{TRUE} for seen (button pressed in response window)
\item \code{time} response time in ms (integer) since stimulus onset, -1 for not seen
\item \code{time_rec} time since epoch when command was received at Compass (integer ms)
\item \code{time_pres} time since epoch that stimulus was presented (integer ms)
\item \code{num_track_events} number of tracking events that occurred during presentation (integer)
\item \code{num_motor_fails} number of times motor could not follow fixation movement during presentation (integer)
\item \code{pupil_diam} pupil diameter in mm (float)
\item \code{loc_x} pixels integer, location in image of presentation (integer)
\item \code{loc_y} pixels integer, location in image of presentation (integer)
}
}
}
\description{
Generic function for presentation of stimulus stim. Depending on
your choice of OPI implementation set using \code{chooseOpi()}, different
parameters are available for \code{opiPresent}
}
\details{
\code{\link{opiPresent}} is blocking in that it will not return
until either a response is obtained, or at least the responseWindow
milliseconds has expired. (Note that more time might have expired.)
Specifying \code{nextStim} allows the implementing machine to use the time
waiting for a response to \code{stim} to make preparations for the next
stimuli. (For example retargeting the projector or moving aperture and/or
filter wheels.) There is no guarantee that the next call to
\code{\link{opiPresent}} will have \code{nextStim} as the first argument;
this could be checked by the machine specific implementations (but currently
is not, I think).

Also note that to allow for different parameters depending on the
implementation chosen with \code{chooseOpi}, every parameter MUST be named in
a call to \code{\link{opiPresent}}.

\subsection{Display}{
Present a circle of radius \code{stim$size} and color \code{stim$color}
at \code{(stim$x, stim$y)} for \code{stim$duration} ms and wait for a keyboard
or mouse response for \code{stim$responseWindow} ms.

\code{stim$size}, \code{sitm$x} and \code{stim$y} are in the same units
as \code{xlim} and \code{ylim} as specified in \code{\link{opiInitialise}}.

If the chosen OPI implementation is \code{Display}, then \code{nextStim}
is ignored.

Duration and response window are rounded to the nearest 5 ms.

Currently only implemented for \code{opiStaticStimulus}.
}

\subsection{Daydream}{
If the chosen OPI implementation is \code{Daydream}, then \code{nextStim}
is ignored.

Note that the dB level is rounded to the nearest cd/\eqn{\mbox{m}^2}{m^2}
that is in the \code{lut} specified in \code{opiInitialise}.

Currently uses the most simple algorithm for drawing a 'circle'
(ie not Bresenham's).

Currently only implemented for \code{opiStaticStimulus}.
}

\subsection{imo}{
DETAILS HERE
}

\subsection{KowaAP7000}{
\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{KowaAP7000}, then \code{nextStim}
is ignored.
}
}
\section{Compass}{
\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{Compass}, then \code{nextStim}
is ignored. Note that the dB level is rounded to the nearest integer.

If tracking is on, then this will block until the tracking is obtained,
and the stimulus presented.
}

\section{Octopus600}{
\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{Octopus600}, then nextStim is
ignored. If \code{eyeControl} is non-zero, as set in \code{opiInitialize},
answer codes describing patient state may arise (see \code{answer} field
in the Value section).
}

\section{Octopus900F310}{
\code{opiPresent(stim, nextStim=NULL)}

This functions as for the Octopus900, but responses are taken from the F310
Controller.

If the L button is pressed, \code{seen} is set to 1.

If the R button is pressed, \code{seen} is set to 2.

If no button is pressed within \code{responseWindow}, then \code{seen} is set to 0.
}

\section{PhoneHMD}{
If the chosen OPI implementation is \code{PhoneHMD}, then \code{nextStim}
is ignored. PhonVR
}

\section{SimGaussian}{
\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}

If the chosen OPI implementation is \code{SimGaussian}, then the response
to a stimuli is determined by sampling from a Frequency-of-Seeing (FoS)
curve (also known as the psychometric function) with formula
\code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, simG.global.sd))}, where \code{x}
is the stimulus value in Humphrey dB, and \code{simG.global.sd} is
set with \code{opiInitialize}.
}

\section{SimHenson}{
\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}

If the chosen OPI implementation is \code{SimHenson}, then the response to a
stimuli is determined by sampling from a Frequency-of-Seeing (FoS) curve
(also known as the psychometric function) with formula
\deqn{\mbox{fpr}+(1-\mbox{fpr}-\mbox{fnr})(1-\mbox{pnorm}(x, \mbox{tt}},
where \eqn{x}{\code{x}} is the stimulus value in Humphrey dB, and pxVar is
\deqn{\min\left(\mbox{simH.global.cap}, e^{A\times\mbox{tt}+B}\right).}{\code{min(simH.cap, exp(A*tt + B))}.}
The ceiling \code{simH.global.cap} is set with the call to
\code{opiInitialize}, and \code{A} and \code{B} are from Table 1 in Henson
et al (2000). Which values are used is determined by \code{simH.type} which
is also set in the call to \code{opiInitialize}.

Note that if the stimulus value is less than zero, then the Henson formula
is not used. The probability of seeing is \code{fpr}.

\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=NULL,
                   criteria=0.95, rt_shape=5.3, rt_rate=1.4, rt_scale=0.1)}

For determinng seen/not-seen for kinetic, the first location (to a fidelity
of 0.01 degrees) on the path (it only works for single paths now) where the
probability of seeing is equal to \code{criteria} is found. If no such
location exists, then the stimuli is not seen. The probability of seeing at
each location is determined using a frequency-of-seeing curve defined as a
cumulative Gaussian with parameters controlled by \code{tt} and
\code{opiInitialize}. At each location along the path, the mean of the FoS
is taken from the \code{tt} function, which takes a distance-along-path
(in degrees) as an argument, and returns a dB value which is the static
threshold at that distance along the path.

Function \code{tt} can return NA for not thresholds that are always not
seen. At each location along the path, the standard deviation of the FoS
is sampled from a Gaussion with mean taken from the formula of Henson et
al (2000), as parametrised by \code{opiInitialize}, and standard deviation
0.25.

The location of a false positive response (for the total kinetic path) is
sampled uniformly from the start of the path to the 'seeing' location, or
the entire path if the stimuli is not seen.

Note that the false positive rate \code{fpr} and the false negative rate
\code{fnr} are specified for the whole path, and not for the individual
static responses along the way.

The actual location returned for a seen response is the location where the
probability of seeing equals \code{criteria}, plus a response time sampled
from a Gamma distribution parameterised by \code{rt_shape} and \code{rt_rate}
and multiplied by \code{rt_scale}.That is:
\code{rgamma(1, shape=rt_shape, rate=rt_rate) / rt_scale}.
}

\section{SimHensonRT}{
\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, dist=stim$level - tt)}

For static stimuli, this function is the same as for \code{SimHenson}, but
reaction times are determined by sampling from \code{rtData} as passed to
\code{opiInitialize}.  The \code{dist} parameter is the distance of the
stimulus level from the true threshold, and should be in the same units as
the \code{Dist} column of \code{rtData}. The default is just the straight
difference between the stimulus level and the true threshold, but you might
want it scaled somehow to match \code{rtData}.
}

\section{SimNo}{
\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{SimNo}, then the response to a
stimuli is always no, hence \code{\link{opiPresent}} always returns
\code{err=NULL}, \code{seen=FALSE}, and \code{time=0}.
}

\section{SimYes}{
\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{SimYes}, then the response to a
stimuli is always yes, hence \code{\link{opiPresent}} always returns
\code{err=NULL}, \code{seen=TRUE}, and \code{time=0}.
}

\examples{
\dontrun{
# Display a spot
 chooseOpi("Display")
 if(!is.null(opiInitialize(width = 1680, height = 1050, ppi = 128, viewdist = 25)))
   stop("opiInitialize failed")
 opiSetBackground(lum = 50, color = "white", fixation = "Cross")

 makeStim <- function(db) {
   s <- list(x = 9, y = 9, level = dbTocd(db, 400), size = 1.72, color = "white",
             duration = 1000, responseWindow = 1000)
   class(s) <- "opiStaticStimulus"
   return(s)
 }
 result <- opiPresent(makeStim(0))

 opiClose()
}
# Stimulus is Size III white-on-white as in the HFA
makeStim <- function(db, n) {
  s <- list(x=9, y=9, level=dbTocd(db, 10000/pi), size=0.43, color="white",
            duration=200, responseWindow=1500)
  class(s) <- "opiStaticStimulus"
  return(s)
}

chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
  stop("opiInitialize failed")

result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)

# Will not work as 'stim' is not named
#result <- opiPresent(makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)

if (!is.null(opiClose()))
  warning("opiClose() failed")
# Same but with simulated reaction times
chooseOpi("SimHensonRT")
data(RtSigmaUnits)
if (!is.null(opiInitialize(type="C", cap=6, rtData=RtSigmaUnits)))
  stop("opiInitialize failed")

dist <- (10 - 30)/min(exp(-0.098 * 30 + 3.62), 6)
result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01, dist=dist)

if (!is.null(opiClose()))
  warning("opiClose() failed")
}
\references{
David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and
Alec Ansons. Response Variability in the Visual Field: Comparison of Optic
Neuritis, Glaucoma, Ocular Hypertension, and Normal Eyes. Investigative
Ophthalmology & Visual Science, February 2000, Vol. 41(2).
}
\seealso{
\code{\link{opiStaticStimulus}}, \code{\link{opiKineticStimulus}},
\code{\link{opiTemporalStimulus}}, \code{\link{chooseOpi}},
\code{\link{opiInitialize}}
}
