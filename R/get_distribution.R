#' Generate spatial distribution objects for species, genera or families
#'
#' @param taxon Character. The taxon to be mapped.
#' @param rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value in \code{taxon}.
#' @param native Logical. Include native range? Defaults to TRUE.
#' @param introduced Logical. Include introduced range? Defaults to TRUE.
#' @param extinct Logical. Include extinct range? Defaults to TRUE.
#' @param location_doubtful Logical. Include occurrences that are thought to be
#'     doubtful? Defaults to TRUE.
#' @param wcvp_names A data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL`, names will be loaded from [rWCVPdata::wcvp_names].
#' @param wcvp_distributions A data frame of distributions from WCVP version 7 or later.
#'   If `NULL`, distributions will be loaded from [rWCVPdata::wcvp_names].
#'
#' @details Where [rank] is higher than species, the distribution of the whole group will be returned, not individual species within that group. This also applies when toggling options - for example, introduced occurrences will only be included if they are outside the native range, regardless of whether \code{native=TRUE} or \code{native=FALSE}. To plot extinctions, introductions or doubtful occurrences within the native range, the \code{summary_table} and \code{generate_occurrence_matrix} functions can be used.
#' @return sf data.frame containing the range polygon/s of the taxon.
#'
#' @importFrom rlang .data
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' r <- get_distribution("Callitris", rank="genus")
#' p <- plot_distribution(r)
#' p
get_distribution <- function(taxon, rank=c("species", "genus", "family","order","higher"), native=TRUE, introduced=TRUE,
                              extinct=TRUE, location_doubtful=TRUE,
                              wcvp_names=NULL,
                              wcvp_distributions=NULL){

  rank <- match.arg(rank)

  if (is.null(wcvp_names) | is.null(wcvp_distributions)) {
    .wcvp_available()
  }

  occurrence_types <- c("native", "introduced", "extinct", "location_doubtful")
  show_types <- occurrence_types[c(native, introduced, extinct, location_doubtful)]

  suppressMessages(sf::sf_use_s2(FALSE))
  wgsrpd3 <- rWCVPdata::wgsprd3

  if(is.null(wcvp_distributions)){
    wcvp_distributions <- rWCVPdata::wcvp_distributions
  }

  if(is.null(wcvp_names)){
    wcvp_names <- rWCVPdata::wcvp_names
  }

  if(rank %in% c("order","higher")) {
    wcvp_names <- right_join(rWCVP::taxonomic_mapping, wcvp_names, by="family")
  }

  if(length(taxon)>1) {
    cli_abort("'taxon' argument must be a single name")
  }
  wcvp_cols <- c("plant_name_id", "taxon_rank", "taxon_status","higher", "order",
                 "family", "genus", "species", "taxon_name", "taxon_authors")
  df <- wcvp_names %>%
    select(any_of(wcvp_cols)) %>%
    right_join(wcvp_distributions, by="plant_name_id")

  range_cols <- c("area_code_l3", "introduced", "extinct", "location_doubtful")
  if(rank=="species"){
    df <- filter(df, .data$taxon_name %in% taxon)
  }
  if(rank=="genus"){
    df <- filter(df, .data$genus %in% taxon)
  }
  if(rank=="family"){
    df <- filter(df, .data$family %in% taxon)
  }

  if(rank=="order"){
    df <- filter(df, .data$order %in% taxon)
  }

  if(rank=="higher"){
    df <- filter(df, .data$higher %in% taxon)
  }
  df <- select(df, all_of(range_cols))

  if(nrow(df) == 0) {
    cli_abort("No distribution for that taxon. Are the rank and spelling both correct?")
  }

  wgsrpd3 %>%
    mutate(occurrence_type=case_when(
      .data$LEVEL3_COD %in% df[df$location_doubtful == 1, "area_code_l3"] ~ "location_doubtful",
      .data$LEVEL3_COD %in% df[df$location_doubtful == 0 &
                                 df$extinct == 0 &
                                 df$introduced == 0, "area_code_l3"] ~ "native",
      .data$LEVEL3_COD %in% df[df$extinct == 1, "area_code_l3"] ~ "extinct",
      .data$LEVEL3_COD %in% df[df$introduced == 1, "area_code_l3"] ~ "introduced"
    )) %>%
    filter(.data$occurrence_type %in% show_types)

}
