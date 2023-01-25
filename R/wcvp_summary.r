#' Generate a summary table from the WCVP
#'
#' @param taxon Character. Taxon to be included. Defaults to NULL (no taxonomic filter; all taxa).
#' @param taxon_rank Character. One of "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area_codes Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param grouping_var Character; one of \code{"area_code_l3", "genus", "family","order"} or \code{"higher"} specifying how the summary should be arranged. Defaults to \code{area_code_l3}.
#' @param hybrids Logical. Include hybrid species in counts? Defaults to FALSE.
#' @param wcvp_names A data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL`, names will be loaded from [rWCVPdata::wcvp_names](https://matildabrown.github.io/rWCVPdata).
#' @param wcvp_distributions A data frame of distributions from WCVP version 7 or later.
#'   If `NULL`, distributions will be loaded from [rWCVPdata::wcvp_names](https://matildabrown.github.io/rWCVPdata).
#' @details
#'  Valid values for rank 'higher' are 'Angiosperms', 'Gymnosperms', 'Ferns' and 'Lycophytes'.
#'  Note that grouping variable (if taxonomic) should be of a lower level than \code{taxon} and \code{taxon_rank} to produce a meaningful summary (i.e., it does not make sense to group a genus by genus, family or higher classification).
#' Additionally, if the grouping variable is taxonomic then species occurrences are aggregated across the input area. This means that if a species is native to any of the input area (even if it is introduced or extinct in other parts) it is counted as 'Native'. Similarly, introduced occurrences take precedence over extinct occurrences. Note that in this type of summary table, 'Endemic' means endemic to the input area, not necessarily to a single WGSRPD Level 3 Area within the input area.
#' @return Data.frame with filtered data, or a \code{gt} table
#'
#' @importFrom rlang .data
#' @import dplyr gt cli
#' @export
#'
#' @examples
#' ferns <- wcvp_summary("Ferns", "higher", get_wgsrpd3_codes("New Zealand"), grouping_var="family")
#' wcvp_summary_gt(ferns)
#'
wcvp_summary <- function(taxon=NULL,
                          taxon_rank=c("species", "genus", "family","order","higher"), #species makes no sense
                          area_codes=NULL,
                          grouping_var = c("area_code_l3","genus","family","order","higher"),
                          hybrids = FALSE,
                          wcvp_names=NULL,wcvp_distributions=NULL){

  taxon_rank <- match.arg(taxon_rank)
  grouping_var <- match.arg(grouping_var)

  if (! is.null(taxon)) {
    if(taxon_rank == "order" & !taxon %in% rWCVP::taxonomic_mapping$order) {
      cli_abort(c("Taxon not found.",
                  "Possible values for this taxonomic rank can be viewed using",
                  "{.code unique(taxonomic_mapping$order)}"))
    }
    if(taxon_rank == "higher" & !taxon %in% rWCVP::taxonomic_mapping$higher) {
      cli_abort(c("Taxon not found.",
                  "Possible values for this taxonomic rank are:",
                  "{.val 'Angiosperms', 'Gymnosperms', 'Ferns' and 'Lycophytes'}"))
    }
  }

  if (is.null(wcvp_names) | is.null(wcvp_distributions)) {
    .wcvp_available()
    .wcvp_fresh()
  }

  if (is.null(wcvp_distributions)) {
    wcvp_distributions <- rWCVPdata::wcvp_distributions
  }

  if (is.null(wcvp_names)) {
    wcvp_names <- rWCVPdata::wcvp_names
  }

  if (is.null(area_codes)) cli_alert_info("No area specified. Generating global summary.")
  if (is.null(taxon)) cli_alert_info("No taxon specified. Generating summary of all species.")

  if(is.null(area_codes)) {
    area_codes <- unique(rWCVP::wgsrpd3$LEVEL3_COD)
    area_name <- "the world"
  } else {
    area_name <- get_area_name(area_codes)
  }

  checklist <- suppressMessages(
    wcvp_checklist(
      taxon=taxon,
      taxon_rank=taxon_rank,
      area_codes=area_codes,
      hybrids=hybrids,
      wcvp_names=wcvp_names,
      wcvp_distributions=wcvp_distributions
    )
  )

  if(!"in_geography" %in% colnames(checklist)) {
    checklist$in_geography <- TRUE
    checklist$area_endemic <- TRUE
  }

  checklist <- checklist %>%
    filter(.data$taxon_rank=="Species",
           .data$taxon_status == "Accepted",
           .data$in_geography) %>%
    filter(.data$area_code_l3 %in% area_codes)

  occ_type_levels <- c("native", "introduced", "extinct", "location_doubful")
  checklist$occurrence_type <- factor(checklist$occurrence_type, levels=occ_type_levels, ordered=TRUE)

  if (grouping_var %in% c("order", "higher") & ! grouping_var %in% colnames(checklist)) {
    checklist <- right_join(rWCVP::taxonomic_mapping, checklist, by="family")
  }

  if (grouping_var %in% c("genus", "family", "order", "higher")) {
    cli_alert_info("Aggregating occurrence types across input area ({.val {area_name}}) - see {.fun ?wcvp_summary} for details.")

    checklist <- checklist %>%
      group_by(.data$plant_name_id) %>%
      slice_min(order_by=.data$occurrence_type, n=1, with_ties=FALSE) %>%
      ungroup()
  }

  total_species <- n_distinct(checklist$taxon_name)

  summary_types <- checklist %>%
    group_by(across(all_of(c(grouping_var, "occurrence_type")))) %>%
    summarise(n=n_distinct(.data$taxon_name), .groups="drop") %>%
    complete(.data$occurrence_type, !!sym(grouping_var), fill=list(n=0)) %>%
    pivot_wider(names_from="occurrence_type", values_from="n", values_fill=0,
                names_glue="{stringr::str_to_title(occurrence_type)}")

  if (grouping_var == "area_code_l3") {
    summary_endemic <- checklist %>%
      group_by(across(all_of(grouping_var))) %>%
      summarise(Endemic=sum(.data$endemic))

    regional_endemics <- checklist %>%
      distinct(.data$plant_name_id, .keep_all=TRUE) %>%
      filter(.data$area_endemic) %>%
      nrow()
  } else {
    summary_endemic <- checklist %>%
      group_by(across(all_of(grouping_var))) %>%
      summarise(Endemic=sum(.data$area_endemic))

    regional_endemics <- sum(checklist$area_endemic, na.rm=TRUE)
  }

  summary_total <- checklist %>%
    group_by(across(all_of(grouping_var))) %>%
    summarise(
      Total=n_distinct(.data$taxon_name)
    )

  summary <- summary_types %>%
    left_join(summary_endemic, by=grouping_var) %>%
    left_join(summary_total, by=grouping_var) %>%
    select(all_of(grouping_var), "Native", "Endemic", "Introduced", "Extinct", "Total")

  list(
    Taxon=taxon,
    Area=area_name,
    Grouping_variable=grouping_var,
    Total_number_of_species=total_species,
    Number_of_regionally_endemic_species=regional_endemics,
    Summary=summary
  )
}


#' Render a summary table from [wcvp_summary]
#'
#' @param x List.

#' @return [gt] table
#'
#' @import gt
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#' @export
#'
#' @examples
#' ferns <- wcvp_summary("Ferns", "higher", get_wgsrpd3_codes("New Zealand"), grouping_var="family")
#' wcvp_summary_gt(ferns)
#'

wcvp_summary_gt <- function(x){
  if(is.null(x$Taxon)) x$Taxon <- "Plants"
  tab_title <- paste("<b>",x$Taxon, "of", x$Area, "</b>")


  if(!is.null(x$Number_of_regionally_endemic_species)){
    tab_sub <- paste0("Total number of species: ", as.numeric(x$Total_number_of_species), "<br>",
                                 "Number of regionally endemic species: ", as.numeric(x$Number_of_regionally_endemic_species))
  } else {
    tab_sub <- paste0("Total number of species: ", as.numeric(x$Total_number_of_species))
  }

  if(x$Grouping_variable =="area_code_l3") colnames(x$Summary)[colnames(x$Summary)=="area_code_l3"] <- "region"
  colnames(x$Summary) <- str_to_sentence(colnames(x$Summary))

  if(x$Grouping_variable =="area_code_l3") {
    x$Summary %>%
      dplyr::group_by(.data$Region) %>%
      gt() %>%
      tab_options(
        row_group.as_column= TRUE,
        stub_row_group.font.weight = "bold",
        column_labels.font.weight = "bold",
        data_row.padding = px(1),
        table.font.size = 12,
        table_body.hlines.color = "transparent",
        heading.subtitle.font.size = 12,
        heading.align = "left") %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_row_groups()
      ) %>%
      tab_style(
        style=cell_text(weight="bold"),
        locations = cells_body(
          columns= .data$Total
        )
      ) %>%
      text_transform(
        locations = cells_body(
          columns = c(.data$Native,.data$Endemic,.data$Introduced,.data$Extinct),
        ),
        fn = function(x){
          ifelse(x == 0, "", x)
        }
      ) %>%
      tab_header(title=html(tab_title), subtitle=html(tab_sub))
  } else {
    x$Summary %>%
      gt() %>%
      tab_options(
        row_group.as_column= TRUE,
        stub_row_group.font.weight = "bold",
        column_labels.font.weight = "bold",
        data_row.padding = px(1),
        table.font.size = 12,
        table_body.hlines.color = "transparent",
        heading.subtitle.font.size = 12,
        heading.align = "left") %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_row_groups()
      ) %>%
      tab_style(
        style=cell_text(weight="bold"),
        locations = cells_body(
          columns= .data$Total
        )
      ) %>%
      text_transform(
        locations = cells_body(
          columns = c(.data$Native,.data$Endemic,.data$Introduced,.data$Extinct),
        ),
        fn = function(x){
          ifelse(x == 0, "", x)
        }
      ) %>%
      tab_header(title=html(tab_title), subtitle=html(tab_sub))
    }


}




