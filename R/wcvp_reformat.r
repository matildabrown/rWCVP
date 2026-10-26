#' Reformat local versions of WCVP
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated because it was a glorified renaming function.
#'
#' @param wcvp_local Data.frame. Local copy of the WCVP.
#' @param version Either 9 or "v9". We will add support for other versions as needed.
#'
#' @return A data.frame with the same variable structure as the WCVP that is
#' included in the data package `rWCVPdata`.
#' @details Note that not all of the original variables are preserved during reformatting.
#' For example, publication is a single variable in v9, but split over multiple in
#' the data package. It is therefore not possible to simply rename this variable.
#' Variables that are present in the data package but not in v9 are filled with `NA`.
#'
#' @keywords internal
#'
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' wcvp_reformat()

wcvp_reformat <- function(wcvp_local=NULL, version = NULL) {
  lifecycle::deprecate_warn("1.3.1", "wcvp_reformat()", "dplyr::rename()")
  # rlang::check_installed("rWCVPdata", reason = "to use the data set `wcvp_names`")

cli::cli_alert_info(
  "We are working on a new function which will allow rWCVP functions to be used with non-WCVP datasets."
)

}
