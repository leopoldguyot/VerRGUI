#' @importFrom shiny showNotification
#' @keywords internal
#' @noRd
.envBasePath <- ".envs"

#' Build environment path
#'
#' @param envName Environment name (string)
#' @param subdir Optional subdirectory (string)
#'
#' @return A full file path (string)
#' @keywords internal
buildEnvPath <- function(envName, subdir = NULL) {
    if (is.null(subdir)) {
        return(file.path(.envBasePath, envName))
    }
    file.path(.envBasePath, envName, subdir)
}

#' Show success notification
#'
#' @param msg Message to display
#' @importFrom shiny showNotification
#' @keywords internal
#' @noRd
notifySuccess <- function(msg) showNotification(msg, type = "message")

#' Show error notification
#'
#' @param msg Message to display
#' @importFrom shiny showNotification
#' @keywords internal
#' @noRd
notifyError <- function(msg) showNotification(msg, type = "error")

#' Wrap an expression with a loading spinner
#'
#' @param message Spinner message (string)
#' @param expr Expression to evaluate
#'
#' @importFrom waiter waiter_show waiter_hide spin_fading_circles
#' @importFrom htmltools tagList
#' @keywords internal
#' @noRd
withSpinner <- function(message, expr) {
    waiter::waiter_show(html = tagList(spin_fading_circles(), message), color = "#333333d3")
    on.exit(waiter::waiter_hide(), add = TRUE)
    force(expr)
}

#' Generate labeled tooltips for fields
#'
#' @param label A `character(1)` main label string.
#' @param tooltip A `character(1)` string with the tooltip description.
#'
#' @return A `tagList` containing the label and tooltip icon.
#' @importFrom htmltools tagList tags
#' @importFrom shiny icon
#' @keywords internal
#' @noRd
tooltipMaker <- function(label, tooltip) {
    tagList(
        label,
        tags$span(
            icon("question-circle"),
            title = tooltip,
            style = "cursor: help; color: #3c8dbc; margin-left: 5px;"
        )
    )
}
envExists <- function(envName) {
    dir.exists(file.path(".envs", envName))
}


#' Get Installed Packages for an Environment
#'
#' This function retrieves the list of installed packages, including their version and source, for a specified environment.
#'
#' @param envName A `character(1)` string specifying the name of the environment.
#'
#' @return A `data.frame` with columns `Package`, `Version`, and `Source`, or `NULL` if no packages are found.
#' @importFrom jsonlite fromJSON
#' @noRd
.getInstalledPackages <- function(envName) {
    envPath <- file.path(".envs", envName)
    envLockfilePath <- file.path(envPath, "renv.lock")

    if (!dir.exists(envPath)) {
        stop("Environment does not exist: ", envName)
    }

    if (file.exists(envLockfilePath)) {
        lockfileData <- jsonlite::fromJSON(envLockfilePath)
        pkgList <- lapply(names(lockfileData$Packages), function(pkg) {
            pkgInfo <- lockfileData$Packages[[pkg]]
            source <- pkgInfo$Source %||% "Unknown"
            list(Package = pkg, Version = pkgInfo$Version, Source = source)
        })
        return(do.call(rbind, lapply(pkgList, as.data.frame)))
    } else {
        return(data.frame(
            Package = character(0),
            Version = character(0),
            Source = character(0)
        ))
    }
}
