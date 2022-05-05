#' Generate a species checklist from WCVP
#'
#' @param taxon Character. One or many taxa to be included. Defaults to NULL (all species)
#' @param rank Character. One of "species", "genus" or "family" giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is NULL. Note that this must be one value only - it is not possible to mix and match families and genera, for example.
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
#' @param synonyms Logical. Include synonyms in checklist? Defaults to TRUE.
#' @param render.report Logical. Render the checklist as a markdown report? Defaults to FALSE.
#' @param report.filename Character. Filename for the HTML file generated. Defaults to "[taxon]_[area]_[checklist.type].html".
#' @param report.type Character; one of "alphabetical" or "taxonomic". Should the generated checklist be sorted alphabetically, or by taxonomic status?
#'
#' @return Data.frame with filtered data and, if render.report=TRUE. a report HTML file.
#'
#' @importFrom rlang .data
#' @import dplyr
#' @export
#'
#' @examples generate_checklist(taxon="Myrtaceae", rank="family", area=get_wgsrpd3_codes("Brazil"))
generate_checklist <- function(taxon=NULL, rank=c("species", "genus", "family"), area=NULL,
                               synonyms=TRUE, render.report=FALSE,
                               native=TRUE, introduced = TRUE,
                               extinct = TRUE, location_doubtful = TRUE,
                               report.filename=NULL, report.type=c("alphabetical", "taxonomic"),
                               local_wcvp=FALSE, wcvp_names=NULL,
                               wcvp_distributions=NULL){


report.type <- match.arg(report.type)
rank <- match.arg(rank)

if(render.report==TRUE & is.null(report.filename)){
  if(!is.null(taxon)) n1 <- taxon else n1 <- NULL
  if(!is.null(area)) {
    n2 <- get_area_name(area)
    if(length(n2)>1) n2 <- paste(n2, sep="-")
    } else n2 <- NULL
    report.filename <- paste(n1,n2,report.type,"checklist.html", sep="_")
    report.filename <- gsub(" ", "_",report.filename)
    report.filename <- sub("_$","",report.filename)
    report.filename <- sub("^_","",report.filename)
    report.filename <- sub("__","_",report.filename)
}

occurrence_types <- c("native", "introduced", "extinct", "location_doubtful")
show_types <- occurrence_types[c(native, introduced, extinct, location_doubtful)]


#set up local wcvp
if(local_wcvp == FALSE){
  wcvp_distributions <- rWCVPdata::wcvp_distributions
  wcvp_names <- rWCVPdata::wcvp_names
} else {
  if (is.null(wcvp_names)) stop("Pointer to wcvp_names missing.")
  if (is.null(wcvp_distributions)) stop("Pointer to wcvp_distributions missing.")
}

# give some messages
if (is.null(area)) message("No area specified. Generating global checklist.")
if (is.null(taxon)) message("No taxon specified. Generating checklist for all species.")
if (synonyms==FALSE) message("Generating a checklist of accepted species names only. Use synonyms = TRUE to include all names")



# get the full dataset to work with
df <- left_join(wcvp_names, wcvp_distributions,by = "plant_name_id") %>%
      left_join(wcvp_names %>%
                     mutate(accepted_name = .data$taxon_name) %>%
                     select(.data$plant_name_id, .data$accepted_name),
                   by=c("accepted_plant_name_id"="plant_name_id"))

# prune early if possible, to reduce computation time
if (synonyms==FALSE) df <- filter(df, .data$taxon_status %in% c("Accepted","Unplaced"))

#filter by taxon
if(!is.null(taxon)) {
  df <- df %>%
    filter(if_any(all_of(rank)) %in% taxon)
}


if(nrow(df)==0) stop("No occurrences. Are the rank and spelling correct?")

#annotate with single-tdwg endemic status
df_summ <- df %>%
  filter(!is.na(.data$area_code_l3)) %>%
  group_by(.data$plant_name_id) %>%
  summarise(endemic=n()) %>%
  mutate(endemic = case_when(
    .data$endemic > 1 ~ 0,
    .data$endemic ==1 ~ 1
  ))


df <- left_join(df, df_summ, by = "plant_name_id")

#annotate with regional endemic status; filter by species that occur in the input geog
if (!is.null(area)){
  df$in_geography <- df$area_code_l3 %in% area

  df_summ <- df %>%
    filter(!is.na(.data$area_code_l3)) %>%
    group_by(.data$plant_name_id, in_geography) %>%
    summarise(counts=n(), .groups = "drop_last") %>%
    mutate(in_geography=paste0("in.geography.",.data$in_geography)) %>%
    tidyr::pivot_wider(names_from = .data$in_geography, values_from = .data$counts) %>%
    filter(!is.na(.data$in.geography.TRUE)) %>%
    mutate(area_endemic = as.numeric(is.na(.data$in.geography.FALSE))) %>%
    select(-c(.data$in.geography.FALSE, .data$in.geography.TRUE))

  df_sp <- df_summ$plant_name_id

#keep only species that were identified in the summary for area endemics, plus synonyms of those names
  df <- df %>%
    left_join(df_summ, by= "plant_name_id") %>%
    filter(.data$plant_name_id %in% df_sp | .data$accepted_plant_name_id %in% df_sp)
} else {
  area <- "global"
  df$area_endemic <- NA
}


if(nrow(df)==0) stop("No occurrences. Are the rank, geography and spelling all correct?")

#filter out genus-level names, excluded occurrence types
df <- df %>%
  mutate(occurrence_type=case_when(
    .data$location_doubtful ==1 ~ "location_doubtful",
    .data$extinct==1 ~ "extinct",
    .data$introduced==1 ~ "introduced",
    TRUE ~ "native",
  )) %>%
  filter(.data$occurrence_type %in% show_types) %>%
  filter(taxon_rank != "Genus") %>%
  arrange(.data$genus, .data$species, .data$infraspecies) %>%
  relocate(.data$accepted_name, .after = .data$accepted_plant_name_id) %>%
  relocate(.data$occurrence_type, .before = .data$introduced) %>%
  relocate(.data$in_geography, .after = .data$location_doubtful)

if(nrow(df)==0) stop("No occurrences after filtering by occurrence type.")


if(render.report==TRUE){


if(report.type=="alphabetical"){
message(glue::glue("Saving report as: ", report.filename))
rmarkdown::render(system.file("rmd", "checklist.Rmd", package = "rWCVP"),
                  quiet = TRUE,
       params=list( version = "New Phytologist Special Issue",
                    taxa = taxon,
                    area_delim = area,
                    mydata = df,
                    synonyms = synonyms), output_file = paste0(getwd(),"/",report.filename))
}
       }

return(df)
}
