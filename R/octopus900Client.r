#
# OPI for Octopus 900 
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
# This version creates a socket to O900Server.java and sends/receives 
# commands. It requires the server to be running, and the server 
# to know where the H-S jar files are (ie in the CLASSPATH). 
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Author: David Lawson    (XXX)
# Date: July 2014
#
# Copyright 2012 Andrew Turpin and David Lawson
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


###################################################################
# .OpiEnv$O900$... are lots of colors and Fixation constants set in setupBackgroundConstants()
###################################################################
if (exists(".OpiEnv") && !exists("O900", where=.OpiEnv))
    assign("O900", new.env(25), envir=.OpiEnv)

###########################################################################
# Get values for fixation, color and bg intensity constants
# from EyeSuite classes, and set globals
#       .OpiEnv$O900$* 
# to the values of those constants.
# INPUT: None.
# OUTPUT: None.
# SIDE EFFECTS: sets .OpiEnv$O900$* if possible.
#               store the names and values in .OpiEnv$O900$constList 
###########################################################################
setupBackgroundConstants <- function() {

    constList <- NULL

    getC <- function(cName) {
        writeLines(paste("OPI_GET_CONSTANT", cName), .OpiEnv$O900$socket)
        res <- readLines(.OpiEnv$O900$socket, n=1)
        if (res == "OZ900Fail") {
            warning(paste("Cannot set",cName,"constant for the O900."))
        } else {
            assign(cName, as.double(res), envir = .OpiEnv$O900) 
            assign("constList", c(.OpiEnv$O900$constList, list(list(cName, as.double(res)))), 
                    envir = .OpiEnv$O900)
        }
    }

    getC("FIX_CENTRE")
    getC("FIX_CROSS")
    getC("FIX_RING")
    getC("BG_OFF")
    getC("BG_1")        # 1.27 cd/m2 == 127 passed to MsgInitializePerimUnit
    getC("BG_10")       # 10 cd/m2 == 1000
    getC("BG_100")      # 100 cd/m2 == 10000

    assign("FIX_CENTER", .OpiEnv$O900$FIX_CENTRE, envir = .OpiEnv$O900) # help Americans

        # get the color fields from OCTO900
    getC("STIM_WHITE")
    getC("STIM_BLUE")
    getC("STIM_RED")
    getC("BG_WHITE")
    getC("BG_YELLOW")
    getC("MET_COL_WW")
    getC("MET_COL_BY")
    getC("MET_COL_RW")
    getC("MET_COL_BLUE_WHITE")
    getC("MET_COL_RED_YELLOW")
    getC("MET_COL_WHITE_YELLOW")
    getC("MET_COL_USER")

    assign("MET_COL_BW", .OpiEnv$O900$MET_COL_BLUE_WHITE,   envir = .OpiEnv$O900)
    assign("MET_COL_RY", .OpiEnv$O900$MET_COL_RED_YELLOW,   envir = .OpiEnv$O900)
    assign("MET_COL_WY", .OpiEnv$O900$MET_COL_WHITE_YELLOW, envir = .OpiEnv$O900)
}

#######################################################################
# INPUT: 
#   serverPort               = port number on which server is listening
#   eyeSuiteSettingsLocation = dir name containing EyeSuite settings
#   eye                      = "right" or "left"
#   gazeFeed                 = NA or a folder name
#   bigWheel                 = FALSE (standard machine), TRUE for modified apeture wheel
#   pres_buzzer              = 0 (no buzzer),1,2, 3 (max volume)
#   resp_buzzer              = 0 (no buzzer),1,2, 3 (max volume)
#
#   Both input dirs should INCLUDE THE TRAILING SLASH.
#
# @return NULL if succeed
# @return 1 if already initialised
# @return 2 if failed to make ready
#
#######################################################################
octo900.opiInitialize <- function(serverPort=50001,eyeSuiteSettingsLocation=NA, 
                                  eye=NA, gazeFeed=NA, bigWheel=FALSE, 
                                  pres_buzzer=0, resp_buzzer=0,
                                 zero_dB_is_10000_asb=TRUE) {
    if (!bigWheel) {
        assign("GOLDMANN", c(6.5, 13, 26, 52, 104) / 60, envir=.OpiEnv$O900)
    } else {
        mm <- c(0.125,0.25,0.5,1,1.41,2,2.83,4,5.66,8,11.3,16,22.6,32,64,128,256)
        ind <- c(32,28,31,26,30,29,27,24,25,23,21,22,39,38,20,37,36)
        GOLDMANN <- rep(NA,39)
        GOLDMANN[ind] <- (sqrt(mm/pi)*180/pi/149.1954)
        assign("GOLDMANN", GOLDMANN, envir=.OpiEnv$O900)
    }

    if (zero_dB_is_10000_asb)
            assign("zero_db_in_asb", 10000, envir=.OpiEnv$O900)
    else
            assign("zero_db_in_asb",  4000, envir=.OpiEnv$O900)

    if (is.na(pres_buzzer) || pres_buzzer < 0) pres_buzzer <- 0
    if (is.na(resp_buzzer) || resp_buzzer < 0) resp_buzzer <- 0
    if (pres_buzzer > 3) pres_buzzer <- 3
    if (resp_buzzer > 3) resp_buzzer <- 3

    cat("Looking for server... ")
    suppressWarnings(tryCatch(    
        v <- socketConnection(host = "localhost", serverPort,
                      blocking = TRUE, open = "w+b",
                      timeout = 10)
        , error=function(e) { 
            stop(paste(" cannot find a server on port",serverPort))
        }
    ))
    close(v)
    
    print("found server :)")

    if (is.na(eyeSuiteSettingsLocation))
        stop("You must specify the EyeSuite settings folder in your call to opiInitialize")
    if (is.na(eye))
        stop("You must specify which eye ('left' or 'right') in your call to opiInitialize")
    if (eye != "left" && eye != "right")
        stop("The eye argument of opiInitialize must be 'left' or 'right'")

    socket <- tryCatch(
        socketConnection(host="localhost", serverPort, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to Octopus 900 on port", serverPort))
    )
    assign("socket", socket, envir = .OpiEnv$O900)
    msg <- paste0("OPI_INITIALIZE \"",eyeSuiteSettingsLocation,"\"\ ",eye, " ", pres_buzzer, " ", resp_buzzer, " ", as.integer(zero_dB_is_10000_asb))
    msg <- paste0(msg, " ", ifelse(is.na(gazeFeed) || is.null(gazeFeed), "NA", gazeFeed))
    writeLines(msg, socket)
    res <- readLines(socket, n=1)
    
    setupBackgroundConstants()

    if (res == "0")
        return(NULL)
    else
        return(res)
}

###########################################################################
# INPUT: 
#   As per OPI spec
#   stim$color must be same as that initialised by opiSetBackground or opiInitialize
#   If F310 is FALSE, response is taken from internal button 
#        (sends OPI_PRESENT_STATIC to server)
#   If F310 is TRUE , response is taken from external controller
#        (sends OPI_PRESENT_STATIC_F310 to server)
#
# Return a list of 
#    err  = string message
#    seen = 1 if seen, 0 otherwise
#    time = reaction time
# 
# If stim is null, always return 0 status.
###########################################################################
octo900.presentStatic <- function(stim, nextStim, F310=FALSE) {
    if (is.null(stim)) 
        return(list(err=0))

    if (is.null(stim$x)) stop(paste("opiPresent: no x value given in static stim"))
    if (is.null(stim$y)) stop(paste("opiPresent: no x value given in static stim"))
    if (is.null(stim$level)) stop(paste("opiPresent: no level value given in static stim"))
    if (is.null(stim$size)) {
        warning("opiPresent: no stim size specified. Assuming Goldmann III = 26/60 degrees.")
        stim$size <- 26/60
    }
    if (is.null(stim$duration)) {
        warning("opiPresent: no stim duration specified. Assuming 200ms.")
        stim$duration <- 200
    }
    if (is.null(stim$responseWindow)) {
        warning("opiPresent: no stim responseWindow specified. Assuming 1500ms.")
        stim$responseWindow <- 1500
    }
    if (is.null(stim$color)) stim$color <- .OpiEnv$O900$STIM_WHITE

    if(min(abs(.OpiEnv$O900$GOLDMANN - stim$size), na.rm=TRUE) > 0.001)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    if (F310)
        msg <- "OPI_PRESENT_STATIC_F310 "
    else
        msg <- "OPI_PRESENT_STATIC "
    
    msg <- paste(msg, stim$x * 10.0, stim$y * 10.0, cdTodb(stim$level, .OpiEnv$O900$zero_db_in_asb/pi) * 10.0)
    msg <- paste(msg, (which.min(abs(.OpiEnv$O900$GOLDMANN - stim$size))))
    msg <- paste(msg, stim$duration)
    msg <- paste(msg, stim$responseWindow)
    msg <- paste(msg, stim$color)
    if (!is.null(nextStim)) {
        msg <- paste(msg, nextStim$x * 10.0, nextStim$y * 10.0)
    } else {
        msg <- paste(msg, stim$x * 10.0, stim$y * 10.0)
    }

    #print(msg)
    writeLines(msg, .OpiEnv$O900$socket)
    #Sys.sleep(1)
    res <- readLines(.OpiEnv$O900$socket, n=1)
    s <- strsplit(res, "|||", fixed=TRUE)[[1]]
    if (s[1] == "0") {
      err <- NULL
    } else {
      err <- s[1]
    }

    return(list(
      err=err,
      seen=as.numeric(s[2]),
      time=as.numeric(s[3]),
      pupilX=ifelse(length(s) > 3, as.numeric(s[4]), NA),
      pupilY=ifelse(length(s) > 4, as.numeric(s[5]), NA)
    ))
}

###########################################################################
# Set up generic calls based on type of stim
###########################################################################
octo900.opiPresent <- function(stim, nextStim=NULL) { UseMethod("octo900.opiPresent") }
setGeneric("octo900.opiPresent")

octo900.opiPresentF310 <- function(stim, nextStim=NULL) { UseMethod("octo900.opiPresentF310") }
setGeneric("octo900.opiPresentF310")

octo900.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    return(octo900.presentStatic(stim, nextStim, FALSE))
}

octo900.opiPresentF310.opiStaticStimulus <- function(stim, nextStim) {
    return(octo900.presentStatic(stim, nextStim, TRUE))
}

 
###########################################################################
# INPUT: 
#   As per OPI spec
#   stim$color must be same as that initialised by opiSetBackground or opiInitialize
#
# Return a list of 
#    err  = string message
#    seen = 1 if seen, 0 otherwise
#    time = reaction time
#
# If stim is null, always return 0 status.
###########################################################################
octo900.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    if (is.null(stim)) 
        return(list(err=0))

    if (is.null(stim$x)) stop(paste("opiPresent: no x value given in temporal stim"))
    if (is.null(stim$y)) stop(paste("opiPresent: no x value given in temporal stim"))
    if (is.null(stim$rate)) stop(paste("opiPresent: no rate value given in temporal stim"))
    if (is.null(stim$size)) {
        warning("opiPresent: no stim size specified. Assuming Goldmann III = 26/60 degrees.")
        stim$size <- 26/60
    }
    if (is.null(stim$duration)) {
        warning("opiPresent: no stim duration specified. Assuming 200ms.")
        stim$duration <- 200
    }
    if (is.null(stim$responseWindow)) {
        warning("opiPresent: no stim responseWindow specified. Assuming 1500ms.")
        stim$responseWindow <- 1500
    }

    if(min(abs(.OpiEnv$O900$GOLDMANN - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    msg <- "OPI_PRESENT_TEMPORAL "
    msg <- paste(c(msg, stim$x * 10.0, stim$y * 10.0, stim$rate), collapse=" ")
    msg <- paste(msg, (which.min(abs(.OpiEnv$O900$GOLDMANN - stim$size))))
    msg <- paste(msg, stim$duration)
    msg <- paste(msg, stim$responseWindow)
    if (!is.null(nextStim)) {
        msg <- paste(msg, nextStim$x * 10.0, nextStim$y * 10.0)
    } else {
        msg <- paste(msg, stim$x * 10.0, stim$y * 10.0)
    }

    writeLines(msg, .OpiEnv$O900$socket)
    res <- readLines(.OpiEnv$O900$socket, n=1)
    s <- strsplit(res, "|||", fixed=TRUE)[[1]]

    if (s[1] == "0") {
      err <- NULL
    } else {
      err <- s[1]
    }

    return(list(
        err =err, 
        seen=as.numeric(s[2]),
        time=as.numeric(s[3])
    ))

}#opiPresent.opiTemporalStimulus()

########################################## 
# Present kinetic stim, return values 
#
# If stim is null, always return 0 status.
########################################## 
octo900.opiPresent.opiKineticStimulus <- function(stim, ...) {
    if (is.null(stim)) 
        return(list(err=0))

    if (is.null(stim$path)) stop(paste("opiPresent: no path values given in kinetic stim"))
    if (is.null(stim$sizes)) stop(paste("opiPresent: no sizes values given in kinetic stim"))
    if (is.null(stim$levels)) stop(paste("opiPresent: no levels values given in kinetic stim"))
    if (is.null(stim$speeds)) stop(paste("opiPresent: no speeds values given in kinetic stim"))

        # convert sizes to GOLDMANN
     stim$sizes <- sapply(stim$sizes, function(s) {
         i <- which.min(abs(.OpiEnv$O900$GOLDMANN - s))
         if(abs(.OpiEnv$O900$GOLDMANN[i] - s) > 0.000001) {
             warning(paste("opiPresent: Rounding stimulus size",s,"to nearest Goldmann size"))
         } 
         return(i)
     })

    msg <- "OPI_PRESENT_KINETIC "
    xs <- xy.coords(stim$path)$x
    ys <- xy.coords(stim$path)$y
    msg <- paste(c(msg, length(xs), xs, ys), collapse=" ")
    msg <- paste(c(msg, sapply(stim$levels, cdTodb, maxStim=.OpiEnv$O900$zero_db_in_asb/pi)), collapse=" ")
    msg <- paste(c(msg, stim$sizes), collapse=" ")
    
      # convert degrees/second into total time for path segment in seconds
    pathLengths <- NULL
    for(i in 2:length(xs)) {
      d <- sqrt((xs[i]-xs[i-1])^2 + (ys[i]-ys[i-1])^2)
      stim$speeds[i-1] <- d/stim$speeds[i-1]
    }
    msg <- paste(c(msg, stim$speeds), collapse=" ")  
    
    writeLines(msg, .OpiEnv$O900$socket)
    res <- readLines(.OpiEnv$O900$socket, n=1)
    s <- strsplit(res, "|||", fixed=TRUE)[[1]]

    if (s[1] == "0") {
      err <- NULL
    } else {
      err <- s[1]
    }

    return(list(
      err=err,
      seen=as.numeric(s[2]),
      time=as.numeric(s[3]),
      x=as.numeric(s[4])/1000,
      y=as.numeric(s[5])/1000,
      pupilX=ifelse(length(s) > 5, as.numeric(s[6]), NA),
      pupilY=ifelse(length(s) > 6, as.numeric(s[7]), NA)
    ))
}

###########################################################################
#
# Input paras are the OpiEnv$O900$* constants
# lum is in cd/m^2 (as per OPI spec) * 100 == .OpiEnv$O900$BG_{OFF | 1 | 10 | 100 }
# color is .OpiEnv$O900$MET_COL_{WW | BY | RW | BLUE_WHITE | RED_YELLOW | WHITE_YELLOW }
# fixation is .OpiEnv$O900$FIX_{RING | CROSS | CENTRE}
# fixIntensity is 0..100 %
#
# @return NULL is succeed.
# @return -1 if opiInitialize has not been successfully called
# @return -2 trouble setting backgound color
# @return -3 trouble setting fixation
# @return -4 all input parameters NA
###########################################################################
octo900.opiSetBackground <- function(lum=NA, color=NA, fixation=NA, fixIntensity=NA) {

    if (all(is.na(c(lum, color, fixation, fixIntensity)))) {
        warning("At least one parameter must be not NA in opiSetBackground")
        return(-4)
    }

    if (!is.na(fixation) && is.na(fixIntensity)) {
        warning("If fixation is specified, fixIntensity must also be given in opiSetBackground")
        return(-4)
    }
    if (!is.na(fixIntensity) && is.na(fixation)) {
        warning("If fixIntensity is specified, fixation must also be given in opiSetBackground")
        return(-4)
    }

    if (is.na(lum)) lum <- -1 
    if (is.na(color)) color <- -1 
    if (is.na(fixation)) fixation <- -1 
    if (is.na(fixIntensity)) fixIntensity <- -1 

    msg <- paste("OPI_SET_BACKGROUND", color, lum, fixation, fixIntensity)
    writeLines(msg, .OpiEnv$O900$socket)
    ret <- as.numeric(readLines(.OpiEnv$O900$socket, n=1))

    if (ret == "0") {
        return(NULL)
    } else {
        return(ret)
    }
}

###########################################################################
# return NULL on success (in fact, always!)
###########################################################################
octo900.opiClose <- function() {
    writeLines("OPI_CLOSE", .OpiEnv$O900$socket)
    close(.OpiEnv$O900$socket)
    return(NULL)
}

###########################################################################
# Lists defined constants
###########################################################################
octo900.opiQueryDevice <- function() {
    cat("Defined constants\n")
    cat("-----------------\n")
    lapply(.OpiEnv$O900$constList, function(x) {
      lapply(x, cat, " ")
      cat("\n")
    })

    return(list(isSim=FALSE))
}
