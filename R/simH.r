#
# An implementation of the OPI that simulates responses using 
# Henson et al (2000) variability.
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

simH.opiClose         <- function() { return(NULL) }
simH.opiSetBackground <- function() { return(NULL) }
simH.opiQueryDevice   <- function() { return (list(type="SimHenson")) }

simH.global.type <- NA
simH.global.cap  <- NA

################################################################################
# Input
#   type N|G|C for the three Henson params
#   cap  dB value for capping stdev form Henson formula
#
# Return NULL if succesful, string error message otherwise  
################################################################################
simH.opiInitialize <- function(type="C", cap=6) {
    if (!is.element(type,c("N","G","C"))) {
        msg <- paste("Bad 'type' specified for SimHenson in opiInitialize():",type)
        warning(msg)
        return(msg)
    }

    assign("simH.global.type", type, envir = .GlobalEnv)
    assign("simH.global.cap",  cap , envir = .GlobalEnv)

    return(NULL)
}

################################################################################
#
################################################################################
simH.opiPresent <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) { UseMethod("simH.opiPresent") }
setGeneric("simH.opiPresent")

#
# Helper function that allows different coefficients from Table 1 of Henson 2000.
#
simH.present <- function(db, cap=6, fpr=0.03, fnr=0.01, tt=30, A, B) {
    pxVar <- min(cap, exp(A*db + B)) # variability of patient, henson formula 

    prSeeing <- fpr + (1-fpr-fnr)*(1-pnorm(db, mean=tt, sd=pxVar))    

    return ( list(
        err = NULL,
        seen= runif(1) < prSeeing,
        time= 0
    ))
}#

#
# stim is list of type opiStaticStimulus
#
simH.opiPresent.opiStaticStimulus <- function(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30) {
    if (!exists(.GlobalEnv$simH.global.type)) {
        return ( list(
            err = "opiInitialize(type,cap) was not called before opiPresent()",
            seen= NA,
            time= NA 
        ))
    }

    if (.GlobalEnv$simH.global.type == "N") {
        return(simH.present(cdTodb(stim$level), .GlobalEnv$simH.global.cap, fpr, fnr, tt, -0.066, 2.81))
    } else if (.GlobalEnv$simH.global.type == "G") {
        return(simH.present(cdTodb(stim$level), .GlobalEnv$simH.global.cap, fpr, fnr, tt, -0.098, 3.62))
    } else if (.GlobalEnv$simH.global.type == "C") {
        return(simH.present(cdTodb(stim$level), .GlobalEnv$simH.global.cap, fpr, fnr, tt, -0.081, 3.27))
    } else {
        return ( list(
            err = "Unknown error in opiPresent() for SimHenson",
            seen= NA,
            time= NA 
        ))
    }
}

########################################## TO DO !
simH.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH temporal persenter yet")
}

simH.opiPresent.opiKineticStimulus <- function(stim, nextStim=NULL, ...) {
    stop("ERROR: haven't written simH kinetic persenter yet")
}
