#' Generate spatial distribution objects for species, genera or families
#'
#' @param taxon Character. The taxon to be mapped. Must be provided.
#' @param taxon_rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value in \code{taxon}.
#' @param native Logical. Include native range? Defaults to `TRUE`.
#' @param introduced Logical. Include introduced range? Defaults to `TRUE`.
#' @param extinct Logical. Include extinct range? Defaults to `TRUE`.
#' @param location_doubtful Logical. Include occurrences that are thought to be
#'     doubtful? Defaults to `TRUE`.
#' @param wcvp_names A data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL` (the default), names will be loaded from [`rWCVPdata::wcvp_names`](https://matildabrown.github.io/rWCVPdata/).
#' @param wcvp_distributions A data frame of distributions from WCVP version 7 or later.
#'   If `NULL` (the default), distributions will be loaded from [`rWCVPdata::wcvp_names`](https://matildabrown.github.io/rWCVPdata/).
#'
#' @details Where \code{taxon_rank} is higher than species, the distribution of the whole
#' group will be returned, not individual species within that group. This also applies when
#' toggling options - for example, introduced occurrences will only be included if they are
#' outside the native range, regardless of whether \code{native=TRUE} or \code{native=FALSE}.
#' To identify extinctions, introductions or doubtful occurrences within the native range,
#' the \code{wcvp_summary} and \code{wcvp_occ_mat} functions can be used.
#' @return Simple features (`sf`) data frame containing the range polygon/s of the taxon.
#'
#' @importFrom rlang .data
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' \donttest{ # this example requires 'rWCVPdata'
#' if(requireNamespace("rWCVPdata")){
#' r <- wcvp_distribution("Callitris", taxon_rank = "genus")
#' p <- wcvp_distribution_map(r)
#' p
#' }
#' }
wcvp_distribution <- function(taxon, taxon_rank = c("species", "genus", "family", "order", "higher"), native = TRUE, introduced = TRUE,
                              extinct = TRUE, location_doubtful = TRUE,
                              wcvp_names = NULL,
                              wcvp_distributions = NULL) {
  taxon_rank <- match.arg(taxon_rank)

  if (!is.null(taxon)) {
    if (taxon_rank == "order" & !taxon %in% rWCVP::taxonomic_mapping$order) {
      cli_abort(c(
        "Taxon not found.",
        "Possible values for this taxonomic rank can be viewed using `unique(taxonomic_mapping$order)`"
      ))
    }

    if (taxon_rank == "higher" & !taxon %in% rWCVP::taxonomic_mapping$higher) {
      cli_abort(c(
        "Taxon not found.",
        "Possible values for this taxonomic rank are: 'Angiosperms', 'Gymnosperms', 'Ferns' and 'Lycophytes'"
      ))
    }
  }

  if (is.null(wcvp_names) | is.null(wcvp_distributions)) {
    .wcvp_available()
    .wcvp_fresh()
  }

  suppressMessages(sf::sf_use_s2(FALSE))

  if (is.null(wcvp_distributions)) {
    wcvp_distributions <- rWCVPdata::wcvp_distributions
  }

  if (is.null(wcvp_names)) {
    wcvp_names <- rWCVPdata::wcvp_names
  }

  if (taxon_rank %in% c("order", "higher")) {
    wcvp_names <- right_join(rWCVP::taxonomic_mapping, wcvp_names, by = "family", multiple="all")
  }

  if (length(taxon) > 1) {
    cli_abort("'taxon' argument must be a single name")
  }
  wcvp_cols <- c(
    "plant_name_id", "taxon_rank", "taxon_status", "higher", "order",
    "family", "genus", "species", "taxon_name", "taxon_authors"
  )
  df <- wcvp_names %>%
    select(any_of(wcvp_cols)) %>%
    right_join(wcvp_distributions, by = "plant_name_id", multiple="all")

  range_cols <- c("area_code_l3", "introduced", "extinct", "location_doubtful")
  if (taxon_rank == "species") {
    df <- filter(df, .data$taxon_name %in% taxon)
  }
  if (taxon_rank == "genus") {
    df <- filter(df, .data$genus %in% taxon)
  }
  if (taxon_rank == "family") {
    df <- filter(df, .data$family %in% taxon)
  }

  if (taxon_rank == "order") {
    df <- filter(df, .data$order %in% taxon)
  }

  if (taxon_rank == "higher") {
    df <- filter(df, .data$higher %in% taxon)
  }
  df <- select(df, all_of(range_cols))

  if (nrow(df) == 0) {
    cli_abort("No distribution for that taxon. Are the rank and spelling both correct?")
  }

  if (! native) {
    df <- filter(df, .data$introduced + .data$extinct + .data$location_doubtful > 0)
  }

  if (! introduced) {
    df <- filter(df, .data$introduced == 0)
  }

  if (! extinct) {
    df <- filter(df, .data$extinct == 0)
  }

  if (! location_doubtful) {
    df <- filter(df, .data$location_doubtful == 0)
  }

  df <- df %>%
    group_by(.data$area_code_l3) %>%
    summarise(
      occurrence_type=determine_occurrence_type_(
        .data$introduced, .data$extinct, .data$location_doubtful
      ),
      .groups="drop"
    )


  rWCVP::wgsrpd3 %>%
    inner_join(df, by=c("LEVEL3_COD"="area_code_l3"))
}

#' Utility function to determine occurrence type if there is more
#' than one taxon in a region.
#'
#' @noRd
#'
determine_occurrence_type_ <- function(introduced, extinct, location_doubtful) {

  if (any(introduced + extinct + location_doubtful == 0)) {
    return("native")
  }

  if (any(introduced == 1)) {
    return("introduced")
  }

  if (any(location_doubtful == 1)) {
    return("location_doubtful")
  }

  return("extinct")
}
