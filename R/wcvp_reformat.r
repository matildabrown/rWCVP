#' Reformat local versions of WCVP
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
#' @export
#'
#' @importFrom rlang .data

wcvp_reformat <- function(wcvp_local, version = NULL) {
  rlang::check_installed("rWCVPdata", reason = "to use the data set `wcvp_names`")

  if (version != 9 & version != "v9") {
    cli::cli_abort("This function currently only supports v9.")
  }

  colnames(wcvp_local)

  colnames(rWCVPdata::wcvp_names)

  test <- wcvp_local %>% dplyr::rename(
    plant_name_id = .data$kew_id,
    taxon_authors = .data$authors,
    taxon_rank = .data$rank,
    taxon_status = .data$taxonomic_status,
    accepted_plant_name_id = .data$accepted_kew_id,
    parent_plant_name_id = .data$parent_kew_id,
    basionym_plant_name_id = .data$original_name_id
  )

  emptycols <- setdiff(colnames(rWCVPdata::wcvp_names), colnames(test))

  test[, emptycols] <- NA
  test <- test[, colnames(rWCVPdata::wcvp_names)]
}
