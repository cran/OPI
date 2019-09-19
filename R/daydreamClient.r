#
# OPI for Google Daydream
# 
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: Mar 2019 
#
# Copyright 2019 Andrew Turpin
#
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
# Modified
#

###################################################################
# .OpiEnv$DayDream$socket is the connection to the daydream
# .OpiEnv$DayDream$LUT has 256 entries. LUT[x] is cd/m^2 value for grey level x
# .OpiEnv$DayDream$degrees_to_pixels() function(x,y) in degrees to (x,y) 
# .OpiEnv$DayDream$...    a variety of constants, etc
###################################################################
if (exists(".OpiEnv") && !exists("DayDream", where=.OpiEnv)) {
    assign("DayDream", new.env(25), envir=.OpiEnv)

    .OpiEnv$DayDream$endian <- "little"

    .OpiEnv$DayDream$LUT <- NULL
    .OpiEnv$DayDream$degrees_to_pixels <- NULL

    .OpiEnv$DayDream$width <- NA        # of whole phone screen
    .OpiEnv$DayDream$height <- NA
    .OpiEnv$DayDream$single_width <- NA
    .OpiEnv$DayDream$single_height <- NA

    .OpiEnv$DayDream$background_left <- NA    # stored MONO background
    .OpiEnv$DayDream$background_right <- NA

    .OpiEnv$DayDream$SEEN     <- 1  
    .OpiEnv$DayDream$NOT_SEEN <- 0  
}

#######################################################################
#' Prepare to send commands to Daydream.
#'
#' As per the OPI standard, `opiInitialise` sets up the environment 
#' and opens a socket to Daydream.
#'
#' For this function to work correctly, parameters must 
#' be named (as in all OPI functions).
#'
#' @param ip   IP address on which server is listening.
#' @param port Port number on which server is listening.
#' @param lut  \code{lut[i]} is cd/m^2 for grey level i. \code{assert(length(lut) == 256)}
#' @param degrees_to_pixels  \code{function(x,y)} where x and y are in degrees, 
#'                           returns c(x,y) in pixels for one eye.
#'
#' @return \code{list(err = NULL)} if succeed, will stop otherwise.
#'
#' @examples
#' \dontrun{opiInitialise(ip="10.0.1.1", port=8912)}
#'
#' @rdname opiInitialise
#######################################################################
daydream.opiInitialize <- function(
        ip="127.0.0.1",
        port=50008, 
        lut = rep(1000, 256),
        degrees_to_pixels = function(x,y) return(50*c(x,y))
    ) {
    cat("Looking for phone at ", ip, "\n")
    suppressWarnings(tryCatch(    
        v <- socketConnection(host = ip, port,
                      blocking = TRUE, open = "w+b",
                      timeout = 10)
        , error=function(e) { 
            stop(paste(" cannot find a phone at", ip, "on port",port))
        }
    ))
    close(v)
    
    cat("found phone at",ip,port,":)\n")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000), 
        error=function(e) stop(paste("Cannot connect to phone at",ip,"on port", port))
    )

    assign("socket", socket, envir = .OpiEnv$DayDream)
    assign("LUT", lut, envir = .OpiEnv$DayDream)
    assign("degrees_to_pixels", degrees_to_pixels, envir = .OpiEnv$DayDream)

    writeLines("OPI_GET_RES", .OpiEnv$DayDream$socket)
    assign("width",        readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("height",       readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("single_width", readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    assign("single_height",readBin(.OpiEnv$DayDream$socket, "integer", size=4, endian=.OpiEnv$DayDream$endian), envir=.OpiEnv$DayDream)
    readBin(.OpiEnv$DayDream$socket, "integer", size=1, endian=.OpiEnv$DayDream$endian) # the \n

    print(paste("Phone res", .OpiEnv$DayDream$width, .OpiEnv$DayDream$height, .OpiEnv$DayDream$single_width, .OpiEnv$DayDream$single_height))
    return(list(err=NULL))
}

###########################################################################
# Find the closest pixel value (index into .OpiEnv$DayDream$LUT less 1)
# for cd/m^2 param cdm2
###########################################################################
find_pixel_value <- function(cdm2) {
    return (which.min(abs(.OpiEnv$DayDream$LUT - cdm2) - 1))
}

###########################################################################
# Load an image to server
#
# @param im is an array of RGB values dim=c(h,w,3)
#
# @return TRUE if succeeds, FALSE otherwise
###########################################################################
load_image <- function(im) {
    h <- dim(im)[1]
    w <- dim(im)[2]     # assert(dim(im)[3] == 3)

    writeLines(paste("OPI_IMAGE", w, h), .OpiEnv$DayDream$socket)
    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    if (res == "READY") {
        for (iy in 1:h)
            for (ix in 1:w)
                for (i in 1:3)
                    writeBin(as.integer(rep(im[iy,ix,i])), .OpiEnv$DayDream$socket, size=1, endian=.OpiEnv$DayDream$endian)
    } else {
        return(FALSE)
    }

    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    print(paste('Load image',res))
    return (res == "OK")
}

###########################################################################
# INPUT: 
#   As per OPI spec. Note eye is part of stim object
#
# Return a list of 
#    err             : (integer) 0 all clear, >= 1 some error codes (eg cannot track, etc)
#    seen            : 0 for not seen, 1 for seen (button pressed in response window)
#    time            : in ms (integer) (does this include/exclude the 200ms presentation time?) -1 for not seen.
###########################################################################
daydream.opiPresent <- function(stim, nextStim=NULL) { UseMethod("daydream.opiPresent") }
setGeneric("daydream.opiPresent")

daydream.opiPresent.opiStaticStimulus <- function(stim, nextStim) {
    if (is.null(stim)) return(list(err="The NULL stimulus not supported", seen=NA, time=NA))

    if (is.null(stim$x)) return(list(err="No x coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$y)) return(list(err="No y coordinate in stimulus", seen=NA, time=NA))
    if (is.null(stim$size)) return(list(err="No size in stimulus", seen=NA, time=NA))
    if (is.null(stim$level)) return(list(err="No level in stimulus", seen=NA, time=NA))
    if (is.null(stim$duration)) return(list(err="No duration in stimulus", seen=NA, time=NA))
    if (is.null(stim$responseWindow)) return(list(err="No responseWindow in stimulus", seen=NA, time=NA))
    if (is.null(stim$eye)) return(list(err="No eye in stimulus", seen=NA, time=NA))

        # make the stimulus
    fg <- find_pixel_value(stim$level)
    radius <- round(mean(.OpiEnv$DayDream$degrees_to_pixels(stim$size/2, stim$size/2)))
    cx <- round(.OpiEnv$DayDream$single_width / 2)
    cy <- round(.OpiEnv$DayDream$single_height / 2)
    xy <- c(cx,cy) + .OpiEnv$DayDream$degrees_to_pixels(stim$x, -stim$y) 

    bg <- ifelse(stim$eye == "L", .OpiEnv$DayDream$background_left, .OpiEnv$DayDream$background_right)
    im <- array(bg, dim=c(2*radius, 2*radius, 3))
    for (ix in -radius:radius)
        for (iy in -radius:radius)
            if (ix^2 + iy^2 <= radius)
                im[radius+1+iy, radius+1+ix, ] <- fg

    if (load_image(im)) {
        msg <- paste("OPI_MONO_PRESENT", stim$eye, xy[1], xy[2], stim$duration, stim$responseWindow, sep=" ")
        writeLines(msg, .OpiEnv$DayDream$socket)

        seen <- readBin(.OpiEnv$DayDream$socket, "integer", size=1)
        time <- readBin(.OpiEnv$DayDream$socket, "double", size=4, endian=.OpiEnv$DayDream$endian)
        readBin(.OpiEnv$DayDream$socket, "integer", size=1, endian=.OpiEnv$DayDream$endian) # the \n

        cat("seen: "); print(seen)
        cat("time: "); print(time)
        seen <- seen == '1'

        if (!seen && time == 0)
            return(list(err="Background image not set", seen=NA, time=NA))
        if (!seen && time == 1)
            return(list(err="Trouble with stim image", seen=NA, time=NA))
        if (!seen && time == 2)
            return(list(err="Location out of range for daydream", seen=NA, time=NA))
        if (!seen && time == 3)
            return(list(err="OPI present error back from daydream", seen=NA, time=NA))

        return(list(
            err  =NULL,
            seen =seen,    # assumes 1 or 0, not "true" or "false"
            time =time
        ))
    } else {
        return(list(err="OPI present could not load stimulus image", seen=NA, time=NA))
    }
}

########################################## 
# Present kinetic stim, return values 
########################################## 
daydream.opiPresent.opiKineticStimulus <- function(stim, ...) {
    warning("DayDream does not support kinetic stimuli (yet)")
    return(list(err="DayDream does not support kinetic stimuli (yet)", seen=FALSE, time=0))
}

###########################################################################
# Not supported on Daydream
###########################################################################
daydream.opiPresent.opiTemporalStimulus <- function(stim, nextStim=NULL, ...) {
    warning("DayDream does not support temporal stimuli (yet)")
    return(list(err="DayDream does not support temporal stimuli (yet)", seen=FALSE, time=0))
}#opiPresent.opiTemporalStimulus()

###########################################################################
# Used to set background for one eye.
# @param lum            cd/m^2
# @param color          Ignored for now
# @param fixation       Just 'Cross' for now
# @param fixation_color RGB triple with values in range [0,255]
# @param fixation_size  Length of cross hairs In pixels
# @param eye            "L" or "R"
#
# @return string on error
# @return NULL if everything works
###########################################################################
daydream.opiSetBackground <- function(lum=NA, color=NA, 
        fixation="Cross", 
        fixation_size=21,    # probably should be odd
        fixation_color=c(0,255,0), 
        eye="L") {
    if (is.na(lum)) {
        warning('Cannot set background to NA in opiSetBackground')
        return('Cannot set background to NA in opiSetBackground')
    }
    if (!is.na(color)) { warning('Color ignored in opiSetBackground.') }

    g <- find_pixel_value(lum)
    writeLines(paste("OPI_MONO_SET_BG", eye, g), .OpiEnv$DayDream$socket)
    res <- readLines(.OpiEnv$DayDream$socket, n=1)
    if (res != "OK")
        return(paste0("Cannot set background to ",g," in opiSetBackground"))

    if (eye == "L")
        .OpiEnv$DayDream$background_left <- g
    else
        .OpiEnv$DayDream$background_right <- g

    if (fixation == 'Cross') { 
            # set fixation to a cross
        m <- (fixation_size + 1) /2
        im <- array(find_pixel_value(lum), dim=c(fixation_size, fixation_size, 3))
        for (i in 1:fixation_size) {
            im[m, i, ] <- fixation_color
            im[i, m, ] <- fixation_color
        }
        if (!load_image(im))
            return("Trouble loading fixation image in opiSetBackground.")

        cx <- round(.OpiEnv$DayDream$single_width / 2)
        cy <- round(.OpiEnv$DayDream$single_height / 2)
        cat("Fixation at: ", eye, " ", cx, " ", cy, "\n")

        writeLines(paste("OPI_MONO_BG_ADD", eye, cx, cy), .OpiEnv$DayDream$socket)
        res <- readLines(.OpiEnv$DayDream$socket, n=1)
        if (res != "OK")
            return("Trouble adding fixation to background in opiSetBackground")
    }
    
    return(NULL)
}

##############################################################################
#### return list(err=NULL, fixations=matrix of fixations)
####       matrix has one row per fixation
####       col-1 timestamp (ms since epoch) 
####       col-2 x in degrees 
####       col-3 y in degrees 
##############################################################################
daydream.opiClose <- function() {
    writeLines("OPI_CLOSE", .OpiEnv$DayDream$socket)

    res <- readLines(.OpiEnv$DayDream$socket, n=1)

    close(.OpiEnv$DayDream$socket)

    if (res != "OK")
        return(list(err="Trouble closing daydream connection."))
    else
        return(list(err=NULL))
}

##############################################################################
#### Lists defined constants
##############################################################################
daydream.opiQueryDevice <- function() {
    vars <- ls(.OpiEnv$DayDream)
    lst <- lapply(vars, function(i) .OpiEnv$DayDream[[i]])
    names(lst) <- vars
    return(lst)
}

