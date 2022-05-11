#' Generate occurrence matrix for taxa and areas
#'
#' @param taxon Character. One or many taxa to be included. Defaults to NULL (all species)
#' @param taxon.rank Character. One of "species", "genus" or "family" giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is NULL. Note that this must be one value only - it is not possible to mix and match families and genera, for example.
#' @param area Character. One or many WGSPRD level 3 region codes. Defaults to NULL (global).
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
#' @export
#'
#' @examples generate_occurrence_matrix(taxon="Poa",taxon.rank="genus",
#' area=c("TAS", "VIC","NSW"), introduced=FALSE)
#'
#'
generate_occurrence_matrix <- function(taxon=NULL, taxon.rank=c("species", "genus", "family"), area=NULL,
                          native=TRUE, introduced = TRUE,
                          extinct = TRUE, location_doubtful = TRUE,
                          local_wcvp=FALSE, wcvp_names=NULL,
                          wcvp_distributions=NULL){

  #to avoid confusion with the variable names in distribution data
  nat <- native
  int <- introduced
  ext <- extinct
  dou <- location_doubtful
  omarea <- area

   if(local_wcvp == FALSE){
    wcvp_distributions <- rWCVPdata::wcvp_distributions
    wcvp_names <- rWCVPdata::wcvp_names
  } else {
    if (is.null(wcvp_names)) stop("Pointer to wcvp_names missing.")
    if (is.null(wcvp_distributions)) stop("Pointer to wcvp_distributions missing.")
  }

  if (is.null(area)) message("No area specified. Generating global occurrence matrix.")
  if (is.null(taxon)) message("No taxon specified. Generating occurrence matrix for all species.")

   df <- suppressMessages(dplyr::left_join(wcvp_names %>%
                     dplyr::select(.data$plant_name_id, .data$taxon_name, .data$taxon_status, .data$taxon_rank, .data$family, .data$genus),
                   wcvp_distributions)) %>%
     dplyr::filter(.data$taxon_status %in% c("Accepted"),#not sure if should include unplaced names here
                   .data$taxon_rank == "Species")

  if(!is.null(taxon)) df <- df %>% dplyr::filter(dplyr::if_any(taxon.rank) %in% taxon)
  if(int==FALSE) df <- dplyr::filter(df, .data$introduced==0)
  if(ext==FALSE) df <- dplyr::filter(df, .data$extinct==0)
  if(dou==FALSE) df <- dplyr::filter(df, .data$location_doubtful==0)


   species_total <- dplyr::filter(df, .data$area_code_l3 %in% omarea) %>%
     dplyr::select(.data$plant_name_id, .data$taxon_name, .data$area_code_l3) %>%
     dplyr::mutate(present = 1)

   ocmat <-  tidyr::pivot_wider(species_total, names_from=.data$area_code_l3, values_from=.data$present, values_fill=0) %>%
     unique()

 return(ocmat)

}
