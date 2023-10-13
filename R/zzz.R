# Check if WCVP data package is available.

.pkgenv <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  wcvp_available <- requireNamespace("rWCVPdata", quietly = TRUE)

  .pkgenv[["wcvp_available"]] <- wcvp_available
}

.onAttach <- function(libname, pkgname) {
  if (!.pkgenv$wcvp_available) {
    packageStartupMessage(unavailable_msg())
  }
}

.wcvp_available <- function() {
  wcvp_available <- requireNamespace("rWCVPdata", quietly = TRUE)

  if (!wcvp_available) {
    cli::cli_abort(c(
      "The package {.code rWCVPdata} is not installed.",
      "You should either:",
      "*" = "run {.code remotes::install_github('matildabrown/rWCVPdata')}",
      "*" = "pass a local WCVP using {.var wcvp_names} and {.var wcvp_distributions}"
    ))
  }
}

.wcvp_fresh <- function() {
  wcvp_fresh <- try(rWCVPdata::wcvp_check_version(silent=TRUE), silent=TRUE)
  
  timeout <- FALSE
  if (class(wcvp_fresh) == "try-error") {
    if (stringr::str_detect(wcvp_fresh[[1]], "Timeout was reached")) {
      timeout <- TRUE
      wcvp_fresh <- TRUE
    } else {
      cli::cli_abort(wcvp_fresh[[1]])
    }
  }
  
  if (rlang::env_has(.pkgenv, "wcvp_fresh")) {
    return(invisible(NULL))
  } else {
    .pkgenv[["wcvp_fresh"]] <- wcvp_fresh
  }

  if (timeout) {
    cli::cli_warn(c(
      "Unable to contact WCVP server at {.url http://sftp.kew.org/pub/data-repositories/WCVP},",
      "please check your internet connection and the server site.",
      "Assuming the WCVP data you have is up to date for now..."
      )
    )
  }

  if (!wcvp_fresh) {
    msg <- NULL
    withCallingHandlers(
      rWCVPdata::wcvp_check_version(),
      message = function(m) {
        msg <<- conditionMessage(m)
        tryInvokeRestart("muffleMessage")
      }
    )

    latest_url <- stringr::str_extract(msg, "(?<=available from )[^\\s]+")
    cli::cli_warn(c(
      "Not using the latest version of WCVP.",
      "You should either:",
      "*" = "update {.code rWCVPdata}",
      "*" = "download the latest WCVP from {.url {latest_url}}"
    ))
  }
}

unavailable_msg <- function() {
  options <- c(
    "run `remotes::install_github('matildabrown/rWCVPdata')`",
    "pass a local WCVP using `wcvp_names` and `wcvp_distributions`"
  )
  options <- paste0(
    "\t", cli::symbol$bullet, " ", options,
    collapse = "\n"
  )

  paste("The package `rWCVPdata` is not installed.",
    "You will need to either:",
    options,
    sep = "\n"
  )
}
