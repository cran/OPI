# This file is AUTOMATICALLY GENERATED from the rgen package.
# DO NOT MANUALLY ALTER.
#
# Open Perimetry Interface implementation for Display
#
# Copyright [2022] [Andrew Turpin & Ivan Marin-Franch]
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

    # environment for this machine in R
if (exists(".opi_env") && !exists("Display", where = .opi_env))
    assign("Display", new.env(), envir = .opi_env)

#' Implementation of opiInitialise for the Display machine.
#'
#' This is for internal use only. Use [opiInitialise()] after
#' \code{chooseOPI("Display")} to call this function.
#'
#' @usage NULL
#'
#' @param address A list containing:
#'  * \code{port} TCP port of the OPI Monitor.
#'  * \code{ip} IP Address of the OPI Monitor.

#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'

#'
#' @details
#'
#' \code{port} can take on values in the range \code{[0, 65535]}.
#'
#' @examples
#' \dontrun{
#' chooseOpi("Display")
#' result <- opiInitialise(address = list(port = 50001, ip = "localhost"))
#' }
#'
#' @seealso [opiInitialise()]
#'
opiInitialise_for_Display <- function(address) {
    if (!exists("socket", where = .opi_env$Display))
        assign("socket", open_socket(address$ip, address$port), .opi_env$Display)
    else
        return(list(err = "Socket connection to Monitor already exists. Perhaps not closed properly last time? Restart Monitor and R."))

    if (is.null(.opi_env$Display$socket))
        return(list(err = sprintf("Cannot Cannot find a server at %s on port %s", address$ip, address$port)))

    if (is.null(address)) return(list(err = "Nothing to do in opiInitialise."))

    msg <- list(port = address$port, ip = address$ip)
    msg <- c(list(command = "initialize"), msg)
    msg <- msg[!unlist(lapply(msg, is.null))]
    msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
    writeLines(msg, .opi_env$Display$socket)

    res <- readLines(.opi_env$Display$socket, n = 1)
    if (length(res) == 0)
        return(list(err = "Monitor server exists but a connection was not closed properly using opiClose() last time it was used. Restart Monitor."))
    res <- jsonlite::parse_json(res)

        # flatten (error, msg[1:n]) into [err, msg[[1]], ..., msg[[n]]]
        #
    if (!"error" %in% names(res))
        return(list(err = "Server did not return a list with element 'error' in opiInitialise"))
    if (!"msg" %in% names(res))
        return(list(err = "Server did not return a list with element 'msg' in opiInitialise"))

    if (res$error) {
        opiRes <- list(err = res$msg)
    } else {
        if (is.list(res$msg))
            opiRes <- res$msg
        else
            opiRes <- list(msg = res$msg)
    }
    return(opiRes)
}

#' Implementation of opiSetup for the Display machine.
#'
#' This is for internal use only. Use [opiSetup()] after
#' \code{chooseOPI("Display")} to call this function.
#'
#' @usage NULL
#'
#' @param settings A list containing:
#'  * \code{eye} The eye for which to apply the settings.
#'  * \code{bgImageFilename} (Optional) If present, display the image in the
#'                           background for eye (scaled to fill fov, bgLum and bgCol ignored)
#'  * \code{fixShape} (Optional) Fixation target type for eye.
#'  * \code{fixLum} (Optional) Fixation target luminance for eye.
#'  * \code{fixType} (Optional) Fixation target texture for eye.
#'  * \code{fixCx} (Optional) x-coordinate of fixation target (degrees).
#'  * \code{fixCy} (Optional) y-coordinate of fixation target (degrees).
#'  * \code{fixCol} (Optional) Fixation target color for eye.
#'  * \code{bgLum} (Optional) Background luminance for eye (cd/m^2).
#'  * \code{tracking} (Optional) Whether to correct stimulus location based on eye position.
#'  * \code{bgCol} (Optional) Background color for eye (rgb).
#'  * \code{fixSx} (Optional) diameter along major axis of ellipse (degrees). 0
#'                 to hide fixation marker.
#'  * \code{fixSy} (Optional) diameter along minor axis of ellipse (degrees). If
#'                 not received, then sy = sx.
#'  * \code{fixRotation} (Optional) Angles of rotation of fixation target
#'                       (degrees). Only useful if sx != sy specified.
#'  * \code{fixImageFilename} (Optional) If fixType == IMAGE, the filename on
#'                            the local filesystem of the machine running JOVP of the image to use
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'

#'
#' @details
#'
#' \code{eye} can take on values in the set \code{{"left", "right", "both",
#'      "none"}}.
#'
#' \code{fixShape} can take on values in the set \code{{"triangle",
#'           "square", "polygon", "hollow_triangle", "hollow_square",
#'           "hollow_polygon", "cross", "maltese", "circle", "annulus",
#'           "optotype", "text", "model"}}.
#'
#' \code{fixLum} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' \code{fixType} can take on values in the set \code{{"flat",
#'          "checkerboard", "sine", "squaresine", "g1", "g2", "g3", "text", "image"}}.
#'
#' \code{fixCx} can take on values in the range \code{[-90.0, 90.0]}.
#'
#' \code{fixCy} can take on values in the range \code{[-90.0, 90.0]}.
#'
#' Elements in \code{fixCol} can take on values in the range \code{[0.0, 1.0]}.
#'
#' \code{bgLum} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' \code{tracking} can take on values in the range \code{[0, 1]}.
#'
#' Elements in \code{bgCol} can take on values in the range \code{[0.0, 1.0]}.
#'
#' \code{fixSx} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' \code{fixSy} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' \code{fixRotation} can take on values in the range \code{[0.0, 360.0]}.
#'
#' @examples
#' \dontrun{
#' chooseOpi("Display")
#' opiInitialise(list(port = 50001, ip = "localhost"))
#' result <- opiSetup(settings = list(eye = "BOTH"))
#' }
#'
#' @seealso [opiSetup()]
#'
opiSetup_for_Display <- function(settings) {
    if(!exists(".opi_env") || !exists("Display", envir = .opi_env) || !("socket" %in% names(.opi_env$Display)) || is.null(.opi_env$Display$socket))
        return(list(err = "Cannot call opiSetup without an open socket to Monitor. Did you call opiInitialise()?."))

    if (is.null(settings)) return(list(err = "Nothing to do in opiSetup."))

    msg <- list(bgImageFilename = settings$bgImageFilename, fixShape = settings$fixShape, fixLum = settings$fixLum, fixType = settings$fixType, fixCx = settings$fixCx, fixCy = settings$fixCy, fixCol = settings$fixCol, bgLum = settings$bgLum, tracking = settings$tracking, bgCol = settings$bgCol, eye = settings$eye, fixSx = settings$fixSx, fixSy = settings$fixSy, fixRotation = settings$fixRotation, fixImageFilename = settings$fixImageFilename)
    msg <- c(list(command = "setup"), msg)
    msg <- msg[!unlist(lapply(msg, is.null))]
    msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
    writeLines(msg, .opi_env$Display$socket)

    res <- readLines(.opi_env$Display$socket, n = 1)
    if (length(res) == 0)
        return(list(err = "Monitor server exists but a connection was not closed properly using opiClose() last time it was used. Restart Monitor."))
    res <- jsonlite::parse_json(res)

        # flatten (error, msg[1:n]) into [err, msg[[1]], ..., msg[[n]]]
        #
    if (!"error" %in% names(res))
        return(list(err = "Server did not return a list with element 'error' in opiSetup"))
    if (!"msg" %in% names(res))
        return(list(err = "Server did not return a list with element 'msg' in opiSetup"))

    if (res$error) {
        opiRes <- list(err = res$msg)
    } else {
        if (is.list(res$msg))
            opiRes <- res$msg
        else
            opiRes <- list(msg = res$msg)
    }
    return(opiRes)
}

#' Implementation of opiQueryDevice for the Display machine.
#'
#' This is for internal use only. Use [opiQueryDevice()] after
#' \code{chooseOPI("Display")} to call this function.
#'
#' @usage NULL
#'
#'
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'

#'
#'
#'
#' @examples
#' \dontrun{
#' chooseOpi("Display")
#' opiInitialise(list(port = 50001, ip = "localhost"))
#' opiSetup(list(eye = "BOTH"))
#' result <- opiQueryDevice()
#' }
#'
#' @seealso [opiQueryDevice()]
#'
opiQueryDevice_for_Display <- function() {
    if(!exists(".opi_env") || !exists("Display", envir = .opi_env) || !("socket" %in% names(.opi_env$Display)) || is.null(.opi_env$Display$socket))
        return(list(err = "Cannot call opiQueryDevice without an open socket to Monitor. Did you call opiInitialise()?."))

    
    msg <- list()
    msg <- c(list(command = "query"), msg)
    msg <- msg[!unlist(lapply(msg, is.null))]
    msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
    writeLines(msg, .opi_env$Display$socket)

    res <- readLines(.opi_env$Display$socket, n = 1)
    if (length(res) == 0)
        return(list(err = "Monitor server exists but a connection was not closed properly using opiClose() last time it was used. Restart Monitor."))
    res <- jsonlite::parse_json(res)

        # flatten (error, msg[1:n]) into [err, msg[[1]], ..., msg[[n]]]
        #
    if (!"error" %in% names(res))
        return(list(err = "Server did not return a list with element 'error' in opiQueryDevice"))
    if (!"msg" %in% names(res))
        return(list(err = "Server did not return a list with element 'msg' in opiQueryDevice"))

    if (res$error) {
        opiRes <- list(err = res$msg)
    } else {
        if (is.list(res$msg))
            opiRes <- res$msg
        else
            opiRes <- list(msg = res$msg)
    }
    return(opiRes)
}

#' Implementation of opiPresent for the Display machine.
#'
#' This is for internal use only. Use [opiPresent()] after
#' \code{chooseOPI("Display")} to call this function.
#'
#' @usage NULL
#'
#' @param stim A list containing:
#'  * \code{lum} List of stimuli luminances (cd/m^2).
#'  * \code{stim.length} The number of elements in this stimuli.
#'  * \code{color1} List of stimulus colors for FLAT shapes and patterns.
#'  * \code{sx} List of diameters along major axis of ellipse (degrees).
#'  * \code{sy} List of diameters along minor axis of ellipse (degrees).
#'  * \code{eye} The eye for which to apply the settings.
#'  * \code{t} List of stimuli presentation times (ms). If 0, then the next stim
#'             list element will be shown simultaneously.
#'  * \code{w} Time to wait for response including presentation time (ms).
#'  * \code{x} List of x co-ordinates of stimuli (degrees).
#'  * \code{y} List of y co-ordinates of stimuli (degrees).
#'  * \code{envSdx} (Optional) List of envelope sd in x direction in degrees.
#'                  Only useful if envType != NONE
#'  * \code{envSdy} (Optional) List of envelope sd in y direction in degrees.
#'                  Only useful if envType != NONE
#'  * \code{envRotation} (Optional) List of envelope rotations in degrees. Only useful if envType != NONE
#'  * \code{type} (Optional) Stimulus type. Values include FLAT, SINE,
#'                CHECKERBOARD, SQUARESINE, G1, G2, G3, IMAGE
#'  * \code{frequency} (Optional) List of frequencies (in cycles per degrees)
#'                     for generation of spatial patterns. Only useful if type != FLAT
#'  * \code{color2} (Optional) List of second colors for non-FLAT shapes
#'  * \code{fullFoV} (Optional) If !0 fullFoV scales image to full field of view
#'                   and sx/sy are ignored.
#'  * \code{phase} (Optional) List of phases (in degrees) for generation of
#'                 spatial patterns. Only useful if type != FLAT
#'  * \code{imageFilename} (Optional) If type == IMAGE, the filename on the
#'                         local filesystem of the machine running JOVP of the image to use
#'  * \code{shape} (Optional) Stimulus shape. Values include CROSS, TRIANGLE,
#'                 CIRCLE, SQUARE, OPTOTYPE.
#'  * \code{rotation} (Optional) List of angles of rotation of stimuli
#'                    (degrees). Only useful if sx != sy specified.
#'  * \code{texRotation} (Optional) List of angles of rotation of stimuli
#'                       (degrees). Only useful if type != FLAT
#'  * \code{defocus} (Optional) List of defocus values in Diopters for stimulus post-processing.
#'  * \code{envType} (Optional) List of envelope types to apply to the stims).
#'                   Only useful if type != FLAT
#'  * \code{contrast} (Optional) List of stimulus contrasts (from 0 to 1). Only
#'                    useful if type != FLAT.
#'  * \code{optotype} (Optional) If shape == OPTOTYPE, the letter A to Z to use
#'
#' @param ... Parameters for other opiPresent implementations that are ignored here.
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'  * \code{time} Response time from stimulus onset if button pressed (ms).
#'  * \code{seen} '1' if seen, '0' if not.

#'
#' @details
#'
#' Elements in \code{lum} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' \code{stim.length} can take on values in the range \code{[1, 2147483647]}.
#'
#' Elements in \code{color1} can take on values in the range \code{[0.0, 1.0]}.
#'
#' Elements in \code{sx} can take on values in the range \code{[0.0, 180.0]}.
#'
#' Elements in \code{sy} can take on values in the range \code{[0.0, 180.0]}.
#'
#' Elements in \code{eye} can take on values in the set
#'                 \code{{"left", "right", "both", "none"}}.
#'
#' Elements in \code{t} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' \code{w} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' Elements in \code{x} can take on values in the range \code{[-90.0, 90.0]}.
#'
#' Elements in \code{y} can take on values in the range \code{[-90.0, 90.0]}.
#'
#' Elements in \code{envSdx} can take on values in the range
#'                    \code{[-1.0E10, 1.0E10]}.
#'
#' Elements in \code{envSdy} can take on values in the range
#'                    \code{[-1.0E10, 1.0E10]}.
#'
#' Elements in \code{envRotation} can take on values in
#'                         the range \code{[-1.0E10, 1.0E10]}.
#'
#' Elements in \code{type} can take on values in the set
#'                  \code{{"flat", "checkerboard", "sine", "squaresine", "g1",
#'                  "g2", "g3", "text", "image"}}.
#'
#' Elements in \code{frequency} can take on values in the
#'                       range \code{[0.0, 300.0]}.
#'
#' Elements in \code{color2} can take on values in the range \code{[0.0, 1.0]}.
#'
#' Elements in \code{fullFoV} can take on values in the
#'                     range \code{[-1.0E10, 1.0E10]}.
#'
#' Elements in \code{phase} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' Elements in \code{shape} can take on values in the set
#'                   \code{{"triangle", "square", "polygon", "hollow_triangle",
#'                   "hollow_square", "hollow_polygon", "cross", "maltese",
#'                   "circle", "annulus", "optotype", "text", "model"}}.
#'
#' Elements in \code{rotation} can take on values in the range \code{[0.0, 360.0]}.
#'
#' Elements in \code{texRotation} can take on values in
#'                         the range \code{[0.0, 360.0]}.
#'
#' Elements in \code{defocus} can take on values in the range \code{[0.0, 1.0E10]}.
#'
#' Elements in \code{envType} can take on values in the set
#'                     \code{{"none", "square", "circle", "gaussian"}}.
#'
#' Elements in \code{contrast} can take on values in the range \code{[0.0, 1.0]}.
#'
#' Elements in \code{optotype} can take on values in the
#'                      set \code{{"a", "b", "c", "d", "e", "f", "g", "h", "i",
#'                      "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
#'                      "u", "v", "w", "x", "y", "z"}}.
#'
#' @examples
#' \dontrun{
#' chooseOpi("Display")
#' opiInitialise(list(port = 50001, ip = "localhost"))
#' opiSetup(list(eye = "BOTH"))
#' result <- opiPresent(stim = list(lum = list(300.0), stim.length = 1, color1 = list(list(1.0,
#'                   1.0, 1.0)), sx = list(1.72), sy = list(1.72),
#'                   eye = list("LEFT"), t = list(200.0), w = 1500.0, x = list(0.0), y = list(0.0)))
#' }
#'
#' @seealso [opiPresent()]
#'
opiPresent_for_Display <- function(stim, ...) {
    if(!exists(".opi_env") || !exists("Display", envir = .opi_env) || !("socket" %in% names(.opi_env$Display)) || is.null(.opi_env$Display$socket))
        return(list(err = "Cannot call opiPresent without an open socket to Monitor. Did you call opiInitialise()?."))

    if (is.null(stim)) return(list(err = "Nothing to do in opiPresent."))

    msg <- list(envSdx = stim$envSdx, lum = stim$lum, envSdy = stim$envSdy, envRotation = stim$envRotation, type = stim$type, stim.length = stim$stim.length, frequency = stim$frequency, color1 = stim$color1, color2 = stim$color2, fullFoV = stim$fullFoV, phase = stim$phase, imageFilename = stim$imageFilename, shape = stim$shape, sx = stim$sx, sy = stim$sy, rotation = stim$rotation, texRotation = stim$texRotation, defocus = stim$defocus, eye = stim$eye, t = stim$t, envType = stim$envType, w = stim$w, contrast = stim$contrast, optotype = stim$optotype, x = stim$x, y = stim$y)
    msg <- c(list(command = "present"), msg)
    msg <- msg[!unlist(lapply(msg, is.null))]
    msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
    writeLines(msg, .opi_env$Display$socket)

    res <- readLines(.opi_env$Display$socket, n = 1)
    if (length(res) == 0)
        return(list(err = "Monitor server exists but a connection was not closed properly using opiClose() last time it was used. Restart Monitor."))
    res <- jsonlite::parse_json(res)

        # flatten (error, msg[1:n]) into [err, msg[[1]], ..., msg[[n]]]
        #
    if (!"error" %in% names(res))
        return(list(err = "Server did not return a list with element 'error' in opiPresent"))
    if (!"msg" %in% names(res))
        return(list(err = "Server did not return a list with element 'msg' in opiPresent"))

    if (res$error) {
        opiRes <- list(err = res$msg)
    } else {
        if (is.list(res$msg))
            opiRes <- res$msg
        else
            opiRes <- list(msg = res$msg)
    }
    return(opiRes)
}

#' Implementation of opiClose for the Display machine.
#'
#' This is for internal use only. Use [opiClose()] after
#' \code{chooseOPI("Display")} to call this function.
#'
#' @usage NULL
#'
#'
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'

#'
#'
#'
#' @examples
#' \dontrun{
#' chooseOpi("Display")
#' opiInitialise(list(port = 50001, ip = "localhost"))
#' opiSetup(list(eye = "BOTH"))
#' result <- opiClose()
#' }
#'
#' @seealso [opiClose()]
#'
opiClose_for_Display <- function() {
    if(!exists(".opi_env") || !exists("Display", envir = .opi_env) || !("socket" %in% names(.opi_env$Display)) || is.null(.opi_env$Display$socket))
        return(list(err = "Cannot call opiClose without an open socket to Monitor. Did you call opiInitialise()?."))

    
    msg <- list()
    msg <- c(list(command = "close"), msg)
    msg <- msg[!unlist(lapply(msg, is.null))]
    msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
    writeLines(msg, .opi_env$Display$socket)

    res <- readLines(.opi_env$Display$socket, n = 1)
    if (length(res) == 0)
        return(list(err = "Monitor server exists but a connection was not closed properly using opiClose() last time it was used. Restart Monitor."))
    res <- jsonlite::parse_json(res)

        # flatten (error, msg[1:n]) into [err, msg[[1]], ..., msg[[n]]]
        #
    if (!"error" %in% names(res))
        return(list(err = "Server did not return a list with element 'error' in opiClose"))
    if (!"msg" %in% names(res))
        return(list(err = "Server did not return a list with element 'msg' in opiClose"))

    if (res$error) {
        opiRes <- list(err = res$msg)
    } else {
        if (is.list(res$msg))
            opiRes <- res$msg
        else
            opiRes <- list(msg = res$msg)
    }
    return(opiRes)
}

