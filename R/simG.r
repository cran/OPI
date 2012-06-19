#
# An implementation of the OPI that simulates responses using 
# Cummulative Gaussian variability with supplied standard deviation.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: June 2012
#
# Copyright 2012 Andrew Turpin and Jonathan Denniss
# This program is part of the OPI (http://perimetry.org/OPI).
# OPI is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

simG.opiClose         <- function() { return(NULL) }
simG.opiSetBackground <- function() { return(NULL) }
simG.opiQueryDevice   <- function() { return (list(type="SimGaussian")) }

################################################################################
# Input
#   sd standard deviation for the Gaussian
#
# Return NULL if succesful, string error message otherwise  
################################################################################
simG.opiInitialize <- function(sd) {
    if (!is.numeric(sd) || (sd < 0)) {
        msg <- paste("Invalid standard deviation in opiInitialize for SimGaussian:",sd)
        warning(msg)
        return(msg)
    }

    assign("simG.global.sd", sd, envir = .GlobalEnv)
    return(NULL)
}

################################################################################
simG.global.sd <- NA

simG.opiPresent <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) { UseMethod("simG.opiPresent") }
setGeneric("simG.opiPresent")

simG.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) {
    prSeeing <- fpr + (1-fpr-fnr)*(1-pnorm(cdTodb(stim$level), mean=tt, sd=.GlobalEnv$simG.global.sd))

    return ( list(
        err = NULL,
        seen= runif(1) < prSeeing,
        time= 0
    ))
}#

########################################## TO DO
simG.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simG temporal persenter yet")
}

simG.opiPresent.opiKineticStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simG kinetic persenter yet")
}
