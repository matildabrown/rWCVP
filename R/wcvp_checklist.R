#' Generate a species checklist from WCVP
#'
#' @param taxon Character. Taxon to be included. Defaults to NULL (no taxonomic filter; all taxa).
#' @param taxon_rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area_codes Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param native Logical. Include species occurrences not flagged as introduced, extinct or doubtful? Defaults to \code{TRUE}.
#' @param introduced Logical. Include species occurrences flagged as introduced? Defaults to \code{TRUE}.
#' @param extinct Logical. Include species occurrences flagged as extinct? Defaults to \code{TRUE}.
#' @param location_doubtful Logical. Include species occurrences flagged as \code{location_doubtful}? Defaults to \code{TRUE}.
#' @param wcvp_names A data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL` (the default), names will be loaded from [`rWCVPdata::wcvp_names`](https://matildabrown.github.io/rWCVPdata).
#' @param wcvp_distributions A data frame of distributions from WCVP version 7 or later.
#'   If `NULL` (the default), distributions will be loaded from [`rWCVPdata::wcvp_names`](https://matildabrown.github.io/rWCVPdata).
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
#' @import tidyr
#' @export
#'
#' @examples
#' # These examples take >10 seconds to run.
#' \dontrun{
#' wcvp_checklist(taxon = "Myrtaceae", taxon_rank = "family", area = get_wgsrpd3_codes("Brazil"))
#' wcvp_checklist(taxon = "Ferns", taxon_rank = "higher", area = get_wgsrpd3_codes("New Zealand"))
#' }
wcvp_checklist <- function(taxon = NULL, taxon_rank = c("species", "genus", "family", "order", "higher"), area_codes = NULL,
                           synonyms = TRUE, render_report = FALSE,
                           native = TRUE, introduced = TRUE,
                           extinct = TRUE, location_doubtful = TRUE,
                           hybrids = FALSE,
                           infraspecies = TRUE,
                           report_filename = NULL, report_dir = NULL,
                           report_type = c("alphabetical", "taxonomic"),
                           wcvp_names = NULL,
                           wcvp_distributions = NULL) {
  ## input checks ----
  report_type <- match.arg(report_type)
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

  if (render_report & is.null(report_dir)) {
    cli_abort("Must provide a directory to save report, using 'report_dir'.")
  }

  occurrence_types <- c("native", "introduced", "extinct", "location_doubtful")
  show_types <- occurrence_types[c(native, introduced, extinct, location_doubtful)]

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

  if (!is.null(taxon)) {
    if (taxon_rank %in% c("order", "higher")) {
      wcvp_names <- right_join(rWCVP::taxonomic_mapping, wcvp_names, by = "family")
    }
  }

  ## input alerts
  if (is.null(area_codes)) cli_alert_info("No area specified. Generating global checklist.")
  if (is.null(taxon)) cli_alert_info("No taxon specified. Generating checklist for all species.")
  if (!synonyms) cli_alert_info("Generating a checklist of accepted species names only. Use {.code synonyms = TRUE} to include all names")

  ## filter checklist names ----
  # prune early if possible, to reduce computation time
  if (!synonyms) wcvp_names <- filter(wcvp_names, .data$taxon_status == "Accepted")

  # filter by taxon
  if (!is.null(taxon)) {
    wcvp_names <- filter(wcvp_names, if_any(all_of(taxon_rank)) %in% taxon)
  }

  # filter out hybrids and infraspecies depending on input
  if (!hybrids) {
    wcvp_names <- filter(wcvp_names, is.na(.data$genus_hybrid), is.na(.data$species_hybrid))
  }

  if (!infraspecies) {
    wcvp_names <- filter(wcvp_names, is.na(.data$infraspecific_rank), is.na(.data$infraspecies))
  }

  if (nrow(wcvp_names) == 0) cli_abort("No occurrences. Are the rank and spelling correct?")

  ## distribution calculations ----
  distribution <- filter(wcvp_distributions, .data$plant_name_id %in% wcvp_names$plant_name_id)

  if (nrow(distribution) == 0) cli_abort("No occurrences. Are the rank, geography and spelling all correct?")

  distribution <- distribution %>%
    mutate(occurrence_type = case_when(
      .data$location_doubtful == 1 ~ "location_doubtful",
      .data$extinct == 1 ~ "extinct",
      .data$introduced == 1 ~ "introduced",
      TRUE ~ "native",
    )) %>%
    select(-c("extinct", "location_doubtful", "introduced")) %>%
    filter(.data$occurrence_type %in% show_types)

  if (nrow(distribution) == 0) cli_abort("No occurrences after filtering by occurrence type.")

  endemics <- distribution %>%
    group_by(.data$plant_name_id) |>
    summarise(endemic = n() == 1, .groups = "drop")

  if (!is.null(area_codes)) {
    regional_endemics <- distribution %>%
      group_by(.data$plant_name_id) %>%
      summarise(
        area_endemic = n() == sum(.data$area_code_l3 %in% area_codes, na.rm = TRUE),
        in_geography = any(.data$area_code_l3 %in% area_codes),
        .groups = "drop"
      ) %>%
      filter(.data$in_geography)

    endemics <- endemics %>%
      inner_join(regional_endemics, by = "plant_name_id") %>%
      replace_na(list(in_geography = FALSE))
  } else {
    endemics$area_endemic <- NA
    endemics$in_geography <- TRUE
    area_codes <- "global"
  }

  distribution <- distribution %>%
    inner_join(endemics, by = "plant_name_id")

  ## build checklist ----
  checklist <- wcvp_names %>%
    left_join(
      wcvp_names %>% select("plant_name_id", "accepted_name" = "taxon_name"),
      by = c("accepted_plant_name_id" = "plant_name_id")
    )

  checklist$place_of_publication <- replace_na(checklist$place_of_publication, "Unknown")

  checklist <- checklist %>%
    inner_join(distribution, by = "plant_name_id")

  # filter out genus-level names
  checklist <- filter(checklist, .data$taxon_rank != "Genus")

  checklist <- checklist %>%
    arrange(.data$family, .data$genus, .data$species, .data$infraspecies) %>%
    relocate("accepted_name", .after = "accepted_plant_name_id") %>%
    relocate("in_geography", .after = "occurrence_type")

  ## render report ----
  if (render_report) {
    if (length(unique(checklist$plant_name_id)) > 5000) {
      cli_alert_warning(c(
        "Checklist longer than 5,000 names",
        "Rendering will be very slow (many hours) and might exceed available memory.",
        "Consider setting {.code synonyms=FALSE} or generating chunked checklists (e.g. by family)."
      ))
      invisible(readline("Press [Enter] to continue or [Escape] to exit:"))
    }

    if (report_type == "taxonomic" & n_distinct(checklist, .data$family) > 1) {
      cli_warn("Taxonomic checklist format does not display family information.")
    }

    if (is.null(report_filename)) {
      area_name <- NULL
      if (!is.null(area_codes)) {
        area_name <- paste(get_area_name(area_codes), collapse = "-")
      }
      report_filename <- paste(taxon, area_name, report_type, "checklist.html", sep = "_")
      report_filename <- gsub(" ", "_", report_filename)
      report_filename <- sub("_$", "", report_filename)
      report_filename <- sub("^_", "", report_filename)
      report_filename <- sub("__", "_", report_filename)
    }

    if (report_type == "alphabetical") {
      cli_alert_info("Saving report as: {.file {report_filename}}")

      param_list <- list(
        version = "New Phytologist Special Issue",
        taxa = taxon,
        area_delim = area_codes,
        mydata = checklist,
        synonyms = synonyms
      )

      suppressWarnings(
        rmarkdown::render(system.file("rmd", "checklist.Rmd", package = "rWCVP"),
          quiet = TRUE,
          params = param_list,
          output_file = file.path(report_dir, report_filename)
        )
      )
    }

    if (report_type == "taxonomic") {
      cli_alert_info("Saving report as: {.file {report_filename}}")

      param_list <- list(
        version = "New Phytologist Special Issue",
        taxa = taxon,
        area_delim = area_codes,
        mydata = checklist,
        synonyms = synonyms
      )

      suppressWarnings(
        rmarkdown::render(system.file("rmd", "checklist_tax.Rmd", package = "rWCVP"),
          quiet = TRUE,
          output_file = file.path(report_dir, report_filename)
        )
      )
    }
  }

  checklist
}
