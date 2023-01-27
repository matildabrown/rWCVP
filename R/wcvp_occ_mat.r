#' Generate occurrence matrix for taxa and areas
#'
#' @param taxon Character. One or many taxa to be included. Defaults to NULL (all species)
#' @param taxon_rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area_codes Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param native Logical. Include species occurrences not flagged as introduced, extinct or doubtful? Defaults to TRUE.
#' @param introduced Logical. Include species occurrences flagged as introduced? Defaults to TRUE.
#' @param extinct Logical. Include species occurrences flagged as extinct? Defaults to TRUE.
#' @param location_doubtful Logical. Include species occurrences flagged as location doubtful? Defaults to TRUE.
#' @param wcvp_names A data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL`, names will be loaded from [rWCVPdata::wcvp_names](https://matildabrown.github.io/rWCVPdata/).
#' @param wcvp_distributions A data frame of distributions from WCVP version 7 or later.
#'   If `NULL`, distributions will be loaded from [rWCVPdata::wcvp_names](https://matildabrown.github.io/rWCVPdata/).
#'
#' @details See \href{https://matildabrown.github.io/rWCVP/articles/occurrence-matrices.html}{here} for an example of how this output can be formatted for publication.
#'
#' @importFrom rlang .data
#' @return A data.frame containing the \code{taxon_name} and \code{plant_name_id}
#' of all species that are present in the \code{area}, plus one variable for each WGSPRD level 3 region in \code{area}, with species presences marked as 1 and absences marked as 0.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @import cli
#' @export
#'
#' @examples
#' \dontrun{
#' wcvp_occ_mat(
#'   taxon = "Poa", taxon_rank = "genus",
#'   area = c("TAS", "VIC", "NSW"), introduced = FALSE
#' )
#' }
#'
wcvp_occ_mat <- function(taxon = NULL, taxon_rank = c("species", "genus", "family", "order", "higher"), area_codes = NULL,
                         native = TRUE, introduced = TRUE,
                         extinct = TRUE, location_doubtful = TRUE,
                         wcvp_names = NULL,
                         wcvp_distributions = NULL) {
  taxon_rank <- match.arg(taxon_rank)

  if (!is.null(taxon)) {
    if (taxon_rank == "order" & !taxon %in% rWCVP::taxonomic_mapping$order) {
      cli_abort(c(
        "Taxon not found.",
        "Possible values for this taxonomic rank can be viewed using",
        "{.code unique(taxonomic_mapping$order)}"
      ))
    }
    if (taxon_rank == "higher" & !taxon %in% rWCVP::taxonomic_mapping$higher) {
      cli_abort(c(
        "Taxon not found.",
        "Possible values for this taxonomic rank are:",
        "{.val 'Angiosperms', 'Gymnosperms', 'Ferns' and 'Lycophytes'}"
      ))
    }
  }

  if (is.null(wcvp_names) | is.null(wcvp_distributions)) {
    .wcvp_available()
    .wcvp_fresh()
  }

  if (is.null(wcvp_names)) {
    wcvp_names <- rWCVPdata::wcvp_names
  }

  if (is.null(wcvp_distributions)) {
    wcvp_distributions <- rWCVPdata::wcvp_distributions
  }

  if (is.null(area_codes)) {
    cli_alert_info("No area specified. Generating global occurrence matrix.")
    area_codes <- unique(rWCVP::wgsrpd_mapping$LEVEL3_COD)
  }

  if (is.null(taxon)) cli_alert_info("No taxon specified. Generating occurrence matrix for all species.")

  wcvp_cols <- c(
    "plant_name_id", "taxon_name", "taxon_rank", "taxon_status",
    "family", "genus", "species"
  )
  checklist <- suppressMessages(
    wcvp_checklist(
      taxon = taxon,
      taxon_rank = taxon_rank,
      area_codes = area_codes,
      wcvp_names = wcvp_names,
      wcvp_distributions = wcvp_distributions,
      synonyms = FALSE,
      infraspecies = FALSE,
      native = native,
      introduced = introduced,
      extinct = extinct,
      location_doubtful = location_doubtful
    )
  )

  occurrences <- checklist %>%
    filter(.data$area_code_l3 %in% area_codes) %>%
    distinct(.data$plant_name_id, .data$taxon_name, .data$area_code_l3) %>%
    mutate(present = 1)

  if (nrow(occurrences) == 0) cli_abort("No occurrences in the input geography.")

  missing_areas <- setdiff(area_codes, occurrences$area_code_l3)
  missing_mat <- matrix(0,
    nrow = n_distinct(occurrences$plant_name_id),
    ncol = length(missing_areas)
  )
  colnames(missing_mat) <- missing_areas

  occ_mat <- occurrences %>%
    pivot_wider(names_from = "area_code_l3", values_from = "present", values_fill = 0) %>%
    bind_cols(as_tibble(missing_mat))

  occ_mat %>%
    select("plant_name_id", "taxon_name", order(colnames(occ_mat)))
}
