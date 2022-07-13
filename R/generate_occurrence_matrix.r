#' Generate occurrence matrix for taxa and areas
#'
#' @param taxon Character. One or many taxa to be included. Defaults to NULL (all species)
#' @param rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param native Logical. Include species occurrences not flagged as introduced, extinct or doubtful? Defaults to TRUE.
#' @param introduced Logical. Include species occurrences flagged as introduced? Defaults to TRUE.
#' @param extinct Logical. Include species occurrences flagged as extinct? Defaults to TRUE.
#' @param location_doubtful Logical. Include species occurrences flagged as location doubtful? Defaults to TRUE.
#' @param wcvp_names Pointer to the WCVP names dataset. Defaults to NULL (data from \code{rWCVPdata}).
#' @param wcvp_distributions Pointer to the WCVP distributions dataset. Defaults to NULL (data from \code{rWCVPdata}).
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
#' @import cli
#' @export
#'
#' @examples generate_occurrence_matrix(taxon="Poa",rank="genus",
#' area=c("TAS", "VIC","NSW"), introduced=FALSE)
#'
#'
generate_occurrence_matrix <- function(taxon=NULL, rank=c("species", "genus", "family","order","higher"), area=NULL,
                          native=TRUE, introduced=TRUE,
                          extinct=TRUE, location_doubtful=TRUE,
                          wcvp_names=NULL,
                          wcvp_distributions=NULL){

  rank <- match.arg(rank)
  #print(rank) #for debugging
  input.area <- area

    if (is.null(wcvp_names)) wcvp_names <- rWCVPdata::wcvp_names
    if (is.null(wcvp_distributions)) wcvp_distributions <- rWCVPdata::wcvp_distributions


  if (is.null(input.area)) {
    cli_alert_info("No area specified. Generating global occurrence matrix.")
    input.area <- unique(wgsrpd_mapping$LEVEL3_COD)
  }
  if (is.null(taxon)) cli_alert_info("No taxon specified. Generating occurrence matrix for all species.")

  wcvp_cols <- c("plant_name_id", "taxon_name", "taxon_rank", "taxon_status",
                 "family", "genus","species")

  df <- wcvp_names %>%
    select(all_of(wcvp_cols)) %>%
    left_join(wcvp_distributions, by="plant_name_id") %>%
    filter(.data$taxon_status %in% c("Accepted"),#not sure if should include unplaced names here
           .data$taxon_rank == "Species")

  if(rank %in% c("order","higher")) {
    df <- right_join(rWCVP::taxonomic_mapping, df, by="family")
  }

  if(!is.null(taxon)){
    if(rank =="species"){
        df <- filter(df, .data$taxon_name %in% taxon)
    } else {
    if  (!taxon %in% df[,rank]) cli_abort("Taxon not found. Are the rank and spelling correct?")
    df <- df %>% filter(df[,rank] %in% taxon)
    }
  }

  if(! native) df <- filter(df, .data$introduced == 1)
  if(! introduced) df <- filter(df, .data$introduced == 0)
  if(! extinct) df <- filter(df, .data$extinct == 0)
  if(! location_doubtful) df <- filter(df, .data$location_doubtful == 0)

  data_blank <- data.frame(t(data.frame(row.names = input.area, val=rep(0, times=length(input.area)))))

  species_total <- df %>%
    filter(.data$area_code_l3 %in% input.area) %>%
    select("plant_name_id", "taxon_name", "area_code_l3") %>%
    mutate(present = 1) %>%
    pivot_wider(names_from="area_code_l3", values_from="present", values_fill=0) %>%
    distinct() %>%
    bind_rows(data_blank) %>% #add the zero-entry columns
    filter(!is.na(.data$taxon_name))

  if(nrow(species_total)==0) cli_abort("No occurrences in the input geography.")

  species_total[is.na(species_total)] <- 0
  colorder <- c(1,2,order(colnames(species_total[,3:ncol(species_total)]))+2)
  species_total <- species_total[,colorder] #rearrange area columns alphabetically

  return(species_total)

}
