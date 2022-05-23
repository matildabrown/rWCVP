#' Generate occurrence matrix for taxa and areas
#'
#' @param taxon Character. One or many taxa to be included. Defaults to NULL (all species)
#' @param rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param native Logical. Include species occurrences not flagged as introduced, extinct or doubtful? Defaults to TRUE.
#' @param introduced Logical. Include species occurrences flagged as introduced? Defaults to TRUE.
#' @param extinct Logical. Include species occurrences flagged as extinct? Defaults to TRUE.
#' @param location_doubtful Logical. Include species occurrences flagged as location doubtful? Defaults to TRUE.
#' @param local_wcvp Logical. If FALSE (the default), use data from \code{rWCVPdata}.
#' If TRUE, use a local copy of the data (useful if rWCVPdata is not the latest
#' version of the checklist).
#' @param wcvp_names Pointer to the WCVP names dataset. Ignored if \code{local.wcvp = FALSE}. Defaults to NULL.
#' @param wcvp_distributions Pointer to the WCVP distributions dataset. Ignored if \code{local.wcvp = FALSE}. Defaults to NULL.
#'
#' @details See vignette "Generating occurrence matrices with rWCVP" for an example of how this output can be formatted for publication.
#'
#' @importFrom rlang .data
#' @return A data.frame containing the \code{taxon_name} and \code{plant_name_id}
#' of all species that are present in the \code{area}, plus one variable for each WGSPRD level 3 region in \code{area}, with species presences marked as 1 and absences marked as 0.
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @export
#'
#' @examples generate_occurrence_matrix(taxon="Poa",rank="genus",
#' area=c("TAS", "VIC","NSW"), introduced=FALSE)
#'
#'
generate_occurrence_matrix <- function(taxon=NULL, rank=c("species", "genus", "family","order","higher"), area=NULL,
                          native=TRUE, introduced=TRUE,
                          extinct=TRUE, location_doubtful=TRUE,
                          local_wcvp=FALSE, wcvp_names=NULL,
                          wcvp_distributions=NULL){

  if(! local_wcvp){
    wcvp_distributions <- rWCVPdata::wcvp_distributions
    wcvp_names <- rWCVPdata::wcvp_names
  } else {
    if (is.null(wcvp_names)) stop("Pointer to wcvp_names missing.")
    if (is.null(wcvp_distributions)) stop("Pointer to wcvp_distributions missing.")
  }

  if(rank %in% c("order","higher")) {
    wcvp_names <- right_join(rWCVP::taxonomic_mapping, wcvp_names, by="family")
  }

  if (is.null(area)) message("No area specified. Generating global occurrence matrix.")
  if (is.null(taxon)) message("No taxon specified. Generating occurrence matrix for all species.")

  wcvp_cols <- c("plant_name_id", "taxon_name", "taxon_rank", "taxon_status",
                 "family", "genus")

  df <- wcvp_names %>%
    select(all_of(wcvp_cols)) %>%
    left_join(wcvp_distributions, by="plant_name_id") %>%
    filter(.data$taxon_status %in% c("Accepted"),#not sure if should include unplaced names here
           .data$taxon_rank == "Species")

  if(!is.null(taxon)) df <- df %>% filter(if_any(rank) %in% taxon)
  if(! introduced) df <- filter(df, .data$introduced == 0)
  if(! extinct) df <- filter(df, .data$extinct == 0)
  if(! location_doubtful) df <- filter(df, .data$location_doubtful == 0)

  species_total <- df %>%
    filter(.data$area_code_l3 %in% area) %>%
    select("plant_name_id", "taxon_name", "area_code_l3") %>%
    mutate(present = 1)

  species_total %>%
    pivot_wider(names_from="area_code_l3", values_from="present", values_fill=0) %>%
    distinct()


}
