# Check if WCVP data package is available.
# Heavily stolen from https://github.com/idiv-biodiversity/lcvplants

.pkgenv <- new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  wcvp_available <- requireNamespace("rWCVPdata", quietly=TRUE)

  .pkgenv[["wcvp_available"]] <- wcvp_available
}

.onAttach <- function(libname, pkgname) {
  if (! .pkgenv$wcvp_available) {
    packageStartupMessage(unavailable_msg())
  }
}

.wcvp_available <- function() {
  wcvp_available <- requireNamespace("rWCVPdata", quietly=TRUE)

  if (!wcvp_available) {
    stop(unavailable_msg())
  }
}

unavailable_msg <- function() {
  options <- c(
    "run `remotes::install_github('matildabrown/rWCVPdata')`",
    "pass a local WCVP using `wcvp_names` and `wcvp_distributions"
  )
  options <- paste0(
    "\t", cli::symbol$pointer, " ", options,
    collapse="\n"
  )

  paste("The package `rWCVPdata` is not installed.",
        "You will need to either:",
        options, sep="\n")
}
