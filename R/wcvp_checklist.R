#' Generate a species checklist from WCVP
#'
#' @param taxon Character. Taxon to be included. Defaults to NULL (no taxonomic filter; all taxa).
#' @param taxon_rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param native Logical. Include species occurrences not flagged as introduced, extinct or doubtful? Defaults to \code{TRUE}.
#' @param introduced Logical. Include species occurrences flagged as introduced? Defaults to \code{TRUE}.
#' @param extinct Logical. Include species occurrences flagged as extinct? Defaults to \code{TRUE}.
#' @param location_doubtful Logical. Include species occurrences flagged as \code{location_doubtful}? Defaults to \code{TRUE}.
#' @param wcvp_names A data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL` (the default), names will be loaded from [`rWCVPdata::wcvp_names`].
#' @param wcvp_distributions A data frame of distributions from WCVP version 7 or later.
#'   If `NULL` (the default), distributions will be loaded from [`rWCVPdata::wcvp_names`].
#' @param synonyms Logical. Include synonyms in checklist (see Details)? Defaults to \code{TRUE}.
#' @param hybrids Logical. Include hybrid species in checklist? Defaults to `FALSE`.
#' @param infraspecies Logical. Include hybrid species in checklist? Defaults to `TRUE`.
#' @param render_report Logical. Render the checklist as a markdown report? Defaults to `FALSE`.
#' @param report_filename Character. Name for the HTML file. Defaults to taxon_area_type.html
#' @param report_dir Character. Directory for the HTML file to be saved in. Must be provided by user.
#' @param report_type Character; one of "alphabetical" (the default) or "taxonomic". Should the generated checklist be sorted alphabetically, or by taxonomic status?
#' @details The \code{synonyms} argument can be used to limit names to those that are Accepted. If \code{synonyms = TRUE} then invalid, illegitimate and other non-accepted names are also included (i.e., the checklist is not limited to names for which \code{taxon_status == "Synonym"}).
#' Two styles of checklist are supported in \code{rWCVP} - alphabetical and taxonomic.
#' In an alphabetical checklist, all names are arranged alphabetically with accepted names in bold, and synonyms are followed by their accepted name.
#' For a taxonomic checklist, names are grouped by their accepted names, and synonyms are listed beneath. Both types of checklist include author, publication and distribution information, though note that family headings are only supported in alphabetical checklists (due to the additional grouping requirement of the taxonomic format).
#'
#'
#' @return Data frame with filtered data and, if `render_report=TRUE`. a report HTML file.
#'
#' @importFrom rlang .data
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' #These examples take >10 seconds to run.
#' \dontrun{
#' wcvp_checklist(taxon="Myrtaceae", taxon_rank="family", area=get_wgsrpd3_codes("Brazil"))
#' wcvp_checklist(taxon="Ferns", taxon_rank="higher", area=get_wgsrpd3_codes("New Zealand"))
#' }
wcvp_checklist <- function(taxon=NULL, taxon_rank=c("species", "genus", "family","order","higher"), area=NULL,
                               synonyms=TRUE, render_report=FALSE,
                               native=TRUE, introduced = TRUE,
                               extinct = TRUE, location_doubtful = TRUE,
                               hybrids = FALSE,
                               infraspecies = TRUE,
                               report_filename=NULL, report_dir=NULL,
                               report_type=c("alphabetical", "taxonomic"),
                               wcvp_names=NULL,
                               wcvp_distributions=NULL){


report_type <- match.arg(report_type)
taxon_rank <- match.arg(taxon_rank)

if(!is.null(taxon)){
if(taxon_rank == "order" &
   !taxon %in% rWCVP::taxonomic_mapping$order) cli_abort(
     "Taxon not found. Possible values for this taxonomic rank can be viewed using `unique(taxonomic_mapping$order)`")
if(taxon_rank == "higher" &
   !taxon %in% rWCVP::taxonomic_mapping$higher) cli_abort(
     "Taxon not found. Possible values for this taxonomic rank are: 'Angiosperms', 'Gymnosperms', 'Ferns' and 'Lycophytes'")
}

if(render_report & is.null(report_dir)) {
  cli_abort("Must provide a directory to save report, using 'report_dir'.")
}

occurrence_types <- c("native", "introduced", "extinct", "location_doubtful")
show_types <- occurrence_types[c(native, introduced, extinct, location_doubtful)]

if (is.null(wcvp_names) | is.null(wcvp_distributions)) {
  .wcvp_available()
  .wcvp_fresh()
}

if(is.null(wcvp_distributions)){
  wcvp_distributions <- rWCVPdata::wcvp_distributions
}
if(is.null(wcvp_names)){
  wcvp_names <- rWCVPdata::wcvp_names
}

if(!is.null(taxon)){
if(taxon_rank %in% c("order","higher")) {
  wcvp_names <- right_join(rWCVP::taxonomic_mapping, wcvp_names, by="family")
}
}

# give some messages
if (is.null(area)) message("No area specified. Generating global checklist.")
if (is.null(taxon)) message("No taxon specified. Generating checklist for all species.")
if (synonyms==FALSE) message("Generating a checklist of accepted species names only. Use synonyms = TRUE to include all names")

# get the full dataset to work with
df <- left_join(wcvp_names, wcvp_distributions,by = "plant_name_id") %>%
      left_join(wcvp_names %>%
                     select(.data$plant_name_id, accepted_name=.data$taxon_name),
                   by=c("accepted_plant_name_id"="plant_name_id"))

#replace Unknown with blank in publication info
df$place_of_publication <- gsub("Unknown", "", df$place_of_publication)

# prune early if possible, to reduce computation time
if (synonyms==FALSE) df <- filter(df, .data$taxon_status == "Accepted")

#filter by taxon
if(!is.null(taxon)) {
  df <- df %>%
    filter(if_any(all_of(taxon_rank)) %in% taxon)
}

#filter out hybrids and infraspecies depending on input
if(hybrids==FALSE){
  df <- df %>% filter(.data$genus_hybrid=="" & .data$species_hybrid=="")
}

if(infraspecies==FALSE){
  df <- df %>% filter(.data$infraspecific_rank=="" & .data$infraspecies=="")
}


if(nrow(df)==0) cli_abort("No occurrences. Are the rank and spelling correct?")

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
    group_by(.data$plant_name_id, .data$in_geography) %>%
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
  df$in_geography <- 1
}


if(nrow(df)==0) cli_abort("No occurrences. Are the rank, geography and spelling all correct?")

#filter out genus-level names,
df <- df %>%
  mutate(occurrence_type=case_when(
    .data$location_doubtful ==1 ~ "location_doubtful",
    .data$extinct==1 ~ "extinct",
    .data$introduced==1 ~ "introduced",
    TRUE ~ "native",
  )) %>%
  filter(.data$taxon_rank != "Genus") %>%
  arrange(.data$family, .data$genus, .data$species, .data$infraspecies) %>%
  relocate(.data$accepted_name, .after = .data$accepted_plant_name_id) %>%
  relocate(.data$occurrence_type, .before = .data$introduced) %>%
  relocate(.data$in_geography, .after = .data$location_doubtful)

#  remove species with excluded occurrence types

df_showtypes <- df %>% filter(.data$occurrence_type %in% show_types, .data$in_geography==TRUE) %>%
  pull(.data$plant_name_id)

df <- df %>% filter(.data$plant_name_id %in% df_showtypes)


if(nrow(df)==0) cli_abort("No occurrences after filtering by occurrence type.")


if(render_report==TRUE){

  if(length(unique(df$plant_name_id))>5000) {
    cli::cli_alert_warning("Checklist longer than 5,000 names; it will be very slow (many hours) to render and might exceed available memory.\nConsider setting `synonyms=FALSE` or generating chunked checklists (e.g. by family).")
    invisible(readline("Press [Enter] to continue or [Escape] to exit:"))
  }
  if(report_type =="taxonomic" & length(unique(df$family)) > 1) {
    cli_warn("Taxonomic checklist format does not display family information.")
  }

    if(is.null(report_filename)){
      if(!is.null(taxon)) n1 <- taxon else n1 <- NULL
      if(!is.null(area)) {
        n2 <- get_area_name(area)
        if(length(n2)>1) n2 <- paste(n2, sep="-")
      } else n2 <- NULL
      report_filename <- paste(n1,n2,report_type,"checklist.html", sep="_")
      report_filename <- gsub(" ", "_",report_filename)
      report_filename <- sub("_$","",report_filename)
      report_filename <- sub("^_","",report_filename)
      report_filename <- sub("__","_",report_filename)
    }



  if(report_type=="alphabetical"){
    cli_alert_info("Saving report as: {.file {report_filename}}")
    suppressWarnings(rmarkdown::render(system.file("rmd", "checklist.Rmd", package = "rWCVP"),
                                       quiet = TRUE,
                                       params=list(version = "New Phytologist Special Issue",
                                                  taxa = taxon,
                                                  area_delim = area,
                                                  mydata = df,
                                                  synonyms = synonyms),
                                       output_file = paste0(report_dir,"/",report_filename))
    )
  }

  if(report_type=="taxonomic"){
    cli_alert_info("Saving report as: {.file {report_filename}}")
    suppressWarnings(rmarkdown::render(system.file("rmd", "checklist_tax.Rmd", package = "rWCVP"),
                      quiet = TRUE,
                      params=list( version = "New Phytologist Special Issue",
                                   taxa = taxon,
                                   area_delim = area,
                                   mydata = df,
                                   synonyms = synonyms), output_file = paste0(report_dir,"/",report_filename))
    )
  }
       }

  return(df)
}