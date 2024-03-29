% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opi.r, R/compassClient.r, R/displayClient.r,
%   R/daydreamClient.r, R/imoClient.r, R/kowaAP7000Client.r, R/octopus600.r,
%   R/octopus900Client.r, R/phoneHMD.r, R/simG.r, R/simH.r, R/simH_RT.r
\name{opiInitialize}
\alias{opiInitialize}
\alias{opiInitialise}
\alias{compass.opiInitialize}
\alias{display.opiInitialize}
\alias{daydream.opiInitialize}
\alias{imo.opiInitialize}
\alias{kowaAP7000.opiInitialize}
\alias{octo600.opiInitialize}
\alias{octo900.opiInitialize}
\alias{PhoneHMD.opiInitialize}
\alias{simG.opiInitialize}
\alias{simH.opiInitialize}
\alias{simH_RT.opiInitialize}
\title{Initialize OPI}
\usage{
opiInitialize(...)

opiInitialise(...)

compass.opiInitialize(ip = "192.168.1.2", port = 44965)

display.opiInitialize(
  width,
  height,
  ppi,
  viewdist,
  lut = seq(0, 400, length.out = 256)
)

daydream.opiInitialize(
  ip = "127.0.0.1",
  port = 50008,
  lut = seq(0, 400, length.out = 256),
  fovy = 90
)

imo.opiInitialize(
  ip = "localhost",
  port = 1234,
  ppd = 16,
  tracking = FALSE,
  tracktol = 2
)

kowaAP7000.opiInitialize(ip = "192.168.1.2", port = 44965)

octo600.opiInitialize(
  ipAddress = "",
  eye = "",
  pupilTracking = FALSE,
  pulsar = FALSE,
  eyeControl = 0
)

octo900.opiInitialize(
  serverPort = 50001,
  eyeSuiteSettingsLocation = "C:/ProgramData/Haag-Streit/EyeSuite/",
  eye = "",
  gazeFeed = "",
  bigWheel = FALSE,
  pres_buzzer = 0,
  resp_buzzer = 0,
  zero_dB_is_10000_asb = TRUE
)

PhoneHMD.opiInitialize(ip, port = 50008, lut = seq(0, 400, length.out = 256))

simG.opiInitialize(sd = 2, display = NA, maxStim = 10000/pi)

simH.opiInitialize(
  type = "C",
  A = -0.081,
  B = 3.27,
  cap = 6,
  display = NA,
  maxStim = 10000/pi
)

simH_RT.opiInitialize(
  type = "C",
  cap = 6,
  A = -0.081,
  B = 3.27,
  display = NA,
  maxStim = 10000/pi,
  rtData = NULL,
  rtFP = 1:1600
)
}
\arguments{
\item{...}{Implementation specific parameters. See details.}

\item{ip}{IP address on which server is listening for PhoneHMD}

\item{port}{Port number on which server is listening for PhoneHMD. Default is 50008}

\item{width}{Width of the screen in pixels}

\item{height}{Height of the screen in pixels}

\item{ppi}{Pixels per inch of the display}

\item{viewdist}{Viewing distance in cm}

\item{lut}{Look up table mapping pixel values to cd/m2}

\item{fovy}{Field of view in degrees in the y-axis. It is different depending on the device.
For Daydream view, it is 90 degrees, for, Daydream view 2 is 100 degrees. Default is 90.}

\item{ppd}{pixels size as in pixels per degree}

\item{tracking}{tracking on or off}

\item{tracktol}{tolerance during tracking in degrees of visual angle}

\item{ipAddress}{IP address of Octopus 600 machine}

\item{eye}{eye; "right" or "left" for "Octopus900", "Octopus600"}

\item{pupilTracking}{pupil tracking}

\item{pulsar}{DETAILS}

\item{eyeControl}{DETAILS}

\item{serverPort}{port number on which server is listening for "Octopus900"}

\item{eyeSuiteSettingsLocation}{dir name containing EyeSuite settings for "Octopus900"}

\item{gazeFeed}{NA or a folder name for "Octopus900"}

\item{bigWheel}{FALSE (standard machine), TRUE for modified apeture wheel for "Octopus900"}

\item{pres_buzzer}{0 (no buzzer),1, 2, 3 (max volume) for "Octopus900"}

\item{resp_buzzer}{0 (no buzzer),1, 2, 3 (max volume) for "Octopus900"}

\item{zero_dB_is_10000_asb}{Is 0 dB 10000 apostilb (TRUE) or or 4000 (FALSE) for "Octopus900"}

\item{sd}{standard deviation for the Gaussian}

\item{display}{Dimensions of plot area (-x,+x,-y,+y) to display stim. No display if NULL.
For "SimHenson", "SimHensonRT", "SimGaussian", "SimNo", "SimYes"}

\item{maxStim}{Maximum stimulus value in cd/m^2 used for db <-> cd/m^2 conversions for
"SimHenson", "SimHensonRT", "SimGaussian"}

\item{type}{N|G|C for the three Henson params for "SimHenson", "SimHensonRT". Type 'X'
to specify your own A and B values (eg different dB scale)}

\item{A}{parameter A for "SimHenson", "SimHensonRT"}

\item{B}{parameter B for "SimHenson", "SimHensonRT"}

\item{cap}{dB value for capping stdev form Henson formula for "SimHenson", "SimHensonRT"}

\item{rtData}{data.frame with colnames == "Rt", "Dist", "Person" for "SimHensonRT"}

\item{rtFP}{response time for false positives ??? for "SimHensonRT"}
}
\value{
Returns NULL if initialization succeeded, otherwise an
implementation-dependent error.

\subsection{Compass}{

Returns a list with elements:
* \code{err} NULL if successful, not otherwise.
* \code{prl} a pair giving the (x,y) in degrees of the Preferred Retinal Locus detected in the initial alignment.
* \code{onh} a pair giving the (x,y) in degrees of the ONH as selected by the user.
* \code{image} raw bytes being the JPEG compressed infra-red image acquired during alignment encoded using
\code{openssl::base64_encode()}.
}

\subsection{Display}{
Always returns NULL.
}

\subsection{Daydream}{

Always returns NULL.
}

\subsection{imo}{

Always returns NULL. Will \code{stop} if there is an error.
}

\subsection{Kowa AP-7000}{

Always returns NULL.
}

\subsection{Octopus600}{

Returns NULL if successful, or an Octopus 600 error code. The default
background and stimulus setup is to white-on-white perimetry.
}

\subsection{Octopus900}{

Returns NULL if successful, 1 if Octopus900 is already initialised by a
previous call to \code{opiInitialize}, and 2 if some error occurred that
prevented initialisation. The default background and stimulus setup is
to white-on-white perimetry. Use \code{opiSetBackground} to change the
background and stimulus colors.
}

\subsection{PhoneHMD}{

Returns NULL if connection is made, otherwise, it returns a text with the error.
}
}
\description{
Generic function for initialization of the chosen OPI
implementation that is set with \code{chooseOpi()}
}
\details{
\subsection{Compass}{

\code{opiInitialize(ip, port)}

If the chosen OPI implementation is \code{Compass}, then you must specify
the IP address and port of the Compass server.
\itemize{
\item \code{ip} is the IP address of the Compass server as a string.
\item \code{port} is the TCP/IP port of the Compass server as a number.
Warning: this returns a list, not a single error code.
}
}

\subsection{Display}{
\code{opiInitialize((width, height, ppi, viewdist, lut = .OpiEnv$Display$LUT)
  )}

If the chosen OPI implementation is \code{Display}, then you can specify
the limits of the plot area and the background color of the plot area.
Note that this assumes \code{link{X11()}} is available on the platform.

We need to know the physical dimensions of the screen and the window
generated in order to calculate stimulus position and size in degrees
of visual angle. The physical dimensions in inches are calculated from
width, height, and ppi. The pixel size pix per degree is then obtained
using viewdist. A gamma function for the screen should be obtained and
its lut passed to convert from luminance in cd/m2 to 8-bit pixel value
(256 levels).

}
}
\section{Daydream}{
\code{opiInitialize(ip="127.0.0.1", port=50008, lut= seq(0, 400, length.out = 256), fovy = 90)}

If the chosen OPI implementation is \code{Daydream}, then you must specify
the IP address of the Android phone that is in the Daydream, and the port on
which the server running on the phone is listening.
\itemize{
\item \code{ip} is the IP address of the Daydream server as a string
\item \code{port} is the TCP/IP port of the Daydream server as a number
\item \code{lut} is a vector of 256 luminance values, with \code{lut[i]} being the cd/\eqn{\mbox{m}^2}{m^2} value for grey level i. Default is \code{seq(0, 4000, length.out = 256)}
\item \code{fovy} Field of view in degrees in the y-axis. It is different depending on the device. For Daydream view, it is 90 degrees, for, Daydream view 2 is 100 degrees. Default is 90.
}
}

\section{imo}{
\code{opiInitialize(ip, port, ppd = 16, tracking = FALSE, tracktol = 2)}

If the chosen OPI implementation is \code{imo}, then you must specify the IP
address and port of the imo server.

\if{html}{\out{<div class="sourceCode">}}\preformatted{* \code{ip} is the IP address of the imo server as a string.
* \code{port} is the TCP/IP port of the imo server as a number.
* \code{ppd} Pixel size in pixels per degree. Default is 16 ppd.
* \code{tracking} Whether to use tracking during stimulus presentation. Default is FALSE.
* \code{tracktol} Tolerance during tracking in degrees of visual angle.  The system does not show any stimulus if eye is not within \code{tracktol} degrees of visual angle from fixation point. Default is 2 degrees.
}\if{html}{\out{</div>}}
}

\section{KowaAP7000}{
\code{opiInitialize(ip, port)}

If the chosen OPI implementation is \code{KowaAP7000}, then you must specify
the IP address and port of the AP-7000 server.
\itemize{
\item \code{ipAddress} is the IP address of the AP-7000 server as a string.
\item \code{port} is the TCP/IP port of the AP-7000 server as a number.
}
}

\section{Octopus600}{
\code{opiInitialize(ipAddress, eye, pupilTracking=FALSE, pulsar=FALSE, eyeControl=0)}

If the chosen OPI implementation is \code{Octopus600}, then you must specify
the IP address of the Octopus 600 and the eye to test.

\code{ipAddress} is the IP address of the Octopus 600 as a string.

\code{eye} must be either "left" or "right".

\code{pupilTracking} is TRUE to turn on IR illumination and set pupil
black level (which happens at the first stimulus presentation).

\code{pulsar} is TRUE for pulsar stimulus, FALSE for size III white-on-white.

\code{eyeControl}
* 0 is off
* 1 is eye blink
* 2 is eye blink, forehead rest, fixation control
* 3 is eye blink, forehead rest, fixation control, fast eye movements
}

\section{Octopus900}{
\code{opiInitialize(serverPort=50001,eyeSuiteSettingsLocation, eye, gazeFeed=NA, bigWheel=FALSE,pres_buzzer=0, resp_buzzer=0, zero_dB_is_10000_asb=TRUE)}

If the chosen OPI implementation is \code{Octopus900}, then you must specify
a directory and the eye to be tested.

\code{serverPort} is the TCP/IP port on which the server is listening (on
localhost).

\code{eyeSuiteSettingsLocation} is the folder name containing the EyeSuite
setting files, and should include the trailing slash.

\code{eye} must be either "left" or "right".

\code{gazeFeed} is the name of an existing folder into which the video frames
of eye tracker are recorded. Set to \code{NA} for no recording.

\code{bigWheel} is \code{FALSE} for a standard Octopus 900 machine. Some
research machines are fitted with an alternate aperture wheel that has 24
sizes, which are accessed with \code{bigWheel} is \code{TRUE}. The mapping
from size to 'hole on wheel' is hard coded; see code for details.

If \code{pres_buzzer} is greater than zero, a buzzer will sound with each
stimuli presented.

If \code{resp_buzzer} is greater than zero, a buzzer will sound with each
button press (response). The volume can be one of 0 (no buzzer), 1, 2, or 3
(max volume). If both buzzers are more than zero, the maximum of the two will
be used as the volume.

If \code{zero_dB_is_10000_asb} is \code{TRUE} then 0 dB is taken as 10000
apostilbs, otherwise 0 dB is taken as 4000 apostilbs.
}

\section{PhoneHMD}{
\code{opiInitialize(serverPort, port = 50008, lut = seq(0, 400, length.out = 256))}
If the chosen OPI implementation is \code{PhoneHMD}, then you must specify
the IP address of the Android PhoneHMD that is in the PhoneHMD, and the port on
which the server running on the PhoneHMD is listening.
\itemize{
\item \code{ip} is the IP address of the PhoneHMD server as a string
\item \code{port} is the TCP/IP port of the PhoneHMD server as a number
\item \code{lut} is a vector of 256 luminance values, with \code{lut[i]} being the
cd/\eqn{\mbox{m}^2}{m^2} value for grey level i. Default is
\code{seq(0, 4000, length.out = 256)}
}
}

\section{SimGaussian}{
\code{opiInitialize(sd, display=NA, maxStim=10000/pi)}

If the chosen OPI implementation is \code{SimGaussian}, then \code{sd} is the
standard deviation value that the simulator will use for the slope/spread of
the psychometric function.

\code{display} and \code{maxStim} is as for SimHenson.
}

\section{SimHenson}{
\code{opiInitialize(type="C", A=NA, B=NA, cap=6, maxStim=10000/pi)}

If the chosen OPI implementation is \code{SimHenson}, then \code{type}
can be one of: \code{"N"}, for normal patients; \code{"G"}, for POAG
patients; and \code{"C"}, for a combination. See Table 1 in Henson et al
(2000).

If \code{type} is \code{"X"} then \code{A} and \code{B} should be
specified and are used in place of one of the three A/B combinations as in
Henson et al (2000). \code{cap} is the maximum standard deviation value that
the simulator will use for the slope/spread of the psychometric function.

If \code{display} is a vector of four numbers \code{c(xlow, xhi, ylow, yhi)},
then a plot area is created of dimension \code{xlim=range(xlow, xhi)} and
\code{ylim=range(ylow, yhi)} and each call to \code{opiPresent} will display
a point on the area. The color of the plot area can be set with
\code{opiSetBackground}, and the color of the displayed point is determined
by the stimulus passed to \code{opiPresent}.

\code{maxStim} is the maximum stimuls value in cd/\eqn{\mbox{m}^2}{m^2}.
This is used in converting cd/\eqn{\mbox{m}^2}{m^2} to dB values, and
vice versa.
}

\section{SimHensonRT}{
\code{opiInitialize(type="C", A=NA, B=NA, cap=6, display=NA, maxStim=10000/pi, rtData, rtFP=1:1600)}

If the chosen OPI implementation is \code{SimHensonRT}, then the first six
parameters are as in \code{SimHenson}, and \code{rtData} is a data frame
with at least 2 columns: \code{"Rt"}, reponse time; and \code{"Dist"},
signifying that distance between assumed threshold and stimulus value in
your units.

This package contains \code{RtSigmaUnits} or \code{RtDbUnits} that can be
loaded with the commands \code{data(RtSigmaUnits)} or \code{data(RtDbUnits)},
and are suitable to pass as values for \code{rtData}.

\code{rtFp} gives the vector of values in milliseconds from which a response
time for a false positive response is randomly sampled.
}

\examples{
\dontrun{
  # Set up the Compass
  chooseOpi("Compass")
  result <- opiInitialize(ip="192.168.1.7", port=44965)
  if (is.null(result$err))
    print(result$prl)
}
\dontrun{
  # Set up a Display and wait for a key press in it.
  chooseOpi("Display")
  if (!is.null(opiInitialize(width = 1680, height = 1050, ppi = 128, viewdist = 25)))
    stop("opiInitialize failed")

  opiSetBackground(lum = 100, color = "white", fixation = "Circle")

  opiClose()
}
\dontrun{
  # Set up the imo
  chooseOpi("imo")
  opiInitialize(ip = "192.168.1.7", port = 1234)
}
\dontrun{
  # Set up the Kowa AP-7000
  chooseOpi("KowaAP7000")
  opiInitialize(ip="192.168.1.7", port=44965)
}
\dontrun{
  # Set up the Octopus 900
  chooseOpi("Octopus900")
  if (!is.null(opiInitialize(
       eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",
       eye="left")))
    stop("opiInitialize failed")
}
# Set up a simulation using a psychometric function that is
# a cumulative gaussian of standard deviation 2
chooseOpi("SimGaussian")
if (!is.null(opiInitialize(sd=2)))
  stop("opiInitialize failed")
# Set up a simple simulation for white-on-white perimetry
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
  stop("opiInitialize failed")

# Set up a simple simulation for white-on-white perimetry
# and display the stimuli in a plot region
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
  stop("opiInitialize failed")
# Set up a simple simulation for white-on-white perimetry
# and display the stimuli in a plot region and simulate response times
chooseOpi("SimHensonRT")
data(RtSigmaUnits)
oi <- opiInitialize(type="C", cap=6, display=NA, rtData=RtSigmaUnits, rtFP=1:100)
if (!is.null(oi))
  stop("opiInitialize failed")
}
\references{
David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and Alec
Ansons. Response Variability in the Visual Field: Comparison of Optic Neuritis,
Glaucoma, Ocular Hypertension, and Normal Eyes. Investigative Ophthalmology &
Visual Science, February 2000, Vol. 41(2).
}
\seealso{
\code{\link{chooseOpi}}, \code{\link{opiSetBackground}},
\code{\link{opiClose}}, \code{\link{opiPresent}}
}
