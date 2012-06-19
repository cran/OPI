#
# Choose which OPI implementation to run
#
# This would all have been nicer in an OO style, with each implementation
# being a subclass of an opi class, but I don't think it can be in R.
# The OPI standard doesn't want users employing exactly the same function 
# no matter what the underlying implementation, and so there cannot be 
# extra parameters to create method signatures for different classes.
# Similarly, some implementations use exactly the same method signatures,
# again which will confuse R, I think. Anyway, if I am wrong, sorry about that.
# What I've done (use a list of implementations and then use a global
# integer to index them) works and makes sense to the non-OO person.
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

################################################################################
# A list of available OPI implementations for chooseOpi to choose from, and 
# the opi* functions to index using opi.global.chooser.
################################################################################
opi.implementations <- list(
    list(
        name="Octopus900",
        opiInitialize    = "octo900.opiInitialize",
        opiClose         = "octo900.opiClose",
        opiSetBackground = "octo900.opiSetBackground",
        opiQueryDevice   = "octo900.opiQueryDevice",
        opiPresent       = "octo900.opiPresent"
    ),
    list(
        name="SimHenson",
        opiInitialize    = "simH.opiInitialize",
        opiClose         = "simH.opiClose",
        opiSetBackground = "simH.opiSetBackground",
        opiQueryDevice   = "simH.opiQueryDevice",
        opiPresent       = "simH.opiPresent"
    ),
    list(
        name="SimGaussian",
        opiInitialize    = "simG.opiInitialize",
        opiClose         = "simG.opiClose",
        opiSetBackground = "simG.opiSetBackground",
        opiQueryDevice   = "simG.opiQueryDevice",
        opiPresent       = "simG.opiPresent"
    )
)

################################################################################
# Input parameters
#   opiImplementation  Either "Octopus900", "HEP", "SimHenson", "SimGaussian"
#                      If NULL, prints a list of possible values. Returns TRUE.
# Side effect
#   Sets opi.global.chooser
#
# Returns
#   TRUE     If successful
#   FALSE    Otherwise
################################################################################
chooseOpi <- function(opiImplementation) { 
    possible <- unlist(lapply(opi.implementations, "[", "name"))

        #
        # If NULL, print the list of possible
        #
    if (is.null(opiImplementation)) {
        print(possible)
        return(TRUE)
    }

        #
        # Warn about the one unimplemented one
        #
    if (opiImplementation == "HEP") {
        # require(rHEP)
        warning("Have not implemented chooseOPI(HEP)")
        return(FALSE)
    } 

        #
        # Check OPIOctopus900 package exists
        #
    if ((opiImplementation == "Octopus900") && !require(OPIOctopus900)) {
        cat("***********************************************************************\n")
        cat("* You cannot choose the Octopus900 OPI without installing the package *\n")
        cat("* OPIOctopus900, which is available with permission from HAAG-STREIT. *\n")
        cat("***********************************************************************\n")
        stop("Get the Octopus900 package")
    }

        #
        # Find the index in opi.implementations
        #
    i <- which(opiImplementation == possible)
    if (length(i) == 0) {
        assign("opi.global.chooser", NA, envir = .GlobalEnv)
        warning(paste("chooseOpi() cannot find opiImplementation",opiImplementation))
        return(FALSE)
    } else {
        assign("opi.global.chooser", i, envir = .GlobalEnv)
        return(TRUE)
    }
}
