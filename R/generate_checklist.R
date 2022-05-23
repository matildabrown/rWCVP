#' Generate a species checklist from WCVP
#'
#' @param taxon Character. Taxon to be included. Defaults to NULL (no taxonomic filter; all taxa).
#' @param rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param native Logical. Include species occurrences not flagged as introduced, extinct or doubtful? Defaults to \code{TRUE}.
#' @param introduced Logical. Include species occurrences flagged as introduced? Defaults to \code{TRUE}.
#' @param extinct Logical. Include species occurrences flagged as extinct? Defaults to \code{TRUE}.
#' @param location_doubtful Logical. Include species occurrences flagged as \code{location_doubtful}? Defaults to \code{TRUE}.
#' @param local_wcvp Logical. If \code{FALSE} (the default), use data from \code{rWCVPdata}.
#' If \code{TRUE}, use a local copy of the data (useful if \code{rWCVPdata} is not the latest
#' version of the checklist).
#' @param wcvp_names Pointer to the WCVP names dataset. Ignored if \code{local.wcvp = FALSE}. Defaults to \code{NULL}.
#' @param wcvp_distributions Pointer to the WCVP distributions dataset. Ignored if \code{local.wcvp = FALSE}. Defaults to \code{NULL}.
#' @param synonyms Logical. Include synonyms in checklist (see Details)? Defaults to \code{TRUE}.
#' @param render.report Logical. Render the checklist as a markdown report? Defaults to FALSE.
#' @param report.filename Character. Name for the HTML file. Defaults to taxon_area_type.html
#' @param report.type Character; one of "alphabetical" or "taxonomic". Should the generated checklist be sorted alphabetically, or by taxonomic status?
#' @details The \code{synonyms} argument can be used to limit names to those that are Accepted and Unplaced. If \code{synonyms = TRUE} then invalid, illegitimate and other non-accepted names are also included (i.e., the checklist is not limited to names for which \code{taxon_status == "Synonym"}).
#' Two styles of checklist are supported in \code{rWCVP} - alphabetical and taxonomic. In an alphabetical checklist, all names are arranged alphabetically with accepted names in bold, and synonyms are followed by their accepted name. For a taxonomic checklist, names are grouped by their accepted names, and synonyms are listed beneath. Both types of checklist include author, publication and distribution information, though note that family headings are only supported in alphabetical checklists (due to the additional grouping requirement of the taxonomic format).
#'
#' @return Data.frame with filtered data and, if render.report=TRUE. a report HTML file.
#'
#' @importFrom rlang .data
#' @import dplyr
#' @export
#'
#' @examples
#' #These examples take >10 seconds to run.
#' \dontrun{
#' generate_checklist(taxon="Myrtaceae", rank="family", area=get_wgsrpd3_codes("Brazil"))
#' generate_checklist(taxon="Ferns", rank="higher", area=get_wgsrpd3_codes("New Zealand"))
#' }
generate_checklist <- function(taxon=NULL, rank=c("species", "genus", "family","order","higher"), area=NULL,
                               synonyms=TRUE, render.report=FALSE,
                               native=TRUE, introduced = TRUE,
                               extinct = TRUE, location_doubtful = TRUE,
                               report.filename=NULL, report.type=c("alphabetical", "taxonomic"),
                               local_wcvp=FALSE, wcvp_names=NULL,
                               wcvp_distributions=NULL){


report.type <- match.arg(report.type)
rank <- match.arg(rank)


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

if(rank %in% c("order","higher")) {
  wcvp_names <- right_join(rWCVP::taxonomic_mapping, wcvp_names, by="family")
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
  filter(.data$taxon_rank != "Genus") %>%
  arrange(.data$genus, .data$species, .data$infraspecies) %>%
  relocate(.data$accepted_name, .after = .data$accepted_plant_name_id) %>%
  relocate(.data$occurrence_type, .before = .data$introduced) %>%
  relocate(.data$in_geography, .after = .data$location_doubtful)


if(nrow(df)==0) stop("No occurrences after filtering by occurrence type.")


if(render.report==TRUE){
  if(report.type =="taxonomic" & length(unique(df$family))>1) warning("Taxonomic checklist format does not display family information.")

    if(is.null(report.filename)){
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



if(report.type=="alphabetical"){
message(glue::glue("Saving report as: ", report.filename))
  suppressWarnings(rmarkdown::render(system.file("rmd", "checklist.Rmd", package = "rWCVP"),
                  quiet = TRUE,
       params=list( version = "New Phytologist Special Issue",
                    taxa = taxon,
                    area_delim = area,
                    mydata = df,
                    synonyms = synonyms), output_file = paste0(getwd(),"/",report.filename))
  )
}

  if(report.type=="taxonomic"){
    message(glue::glue("Saving report as: ", report.filename))
    suppressWarnings(rmarkdown::render(system.file("rmd", "checklist_tax.Rmd", package = "rWCVP"),
                      quiet = TRUE,
                      params=list( version = "New Phytologist Special Issue",
                                   taxa = taxon,
                                   area_delim = area,
                                   mydata = df,
                                   synonyms = synonyms), output_file = report.filename)
    )
  }
       }

return(df)
}
