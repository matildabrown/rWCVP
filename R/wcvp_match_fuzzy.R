#' Fuzzy (approximate) matching to the WCVP.
#'
#' Fuzzy matching to names in the WCVP using phonetic matching and edit distance.
#' The WCVP can be loaded for matching from [rWCVPdata::wcvp_names](https://matildabrown.github.io/rWCVPdata/).
#'
#' @param names_df Data frame of names for matching.
#' @param wcvp_names Data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL` (the default), names will be loaded from [`rWCVPdata::wcvp_names`](https://matildabrown.github.io/rWCVPdata/).
#' @param name_col Character. The column in `names_df` that has the taxon name
#' for matching.
#' @param progress_bar Logical. Show progress bar when matching? Defaults to
#'  `TRUE`; should be changed to `FALSE` if used in a markdown report.
#'
#' @return Match results from WCVP bound to the original data from `names_df`.
#'
#' @details The `wcvp_match_fuzzy` function uses phonetic matching first and then finds
#' the closest match based on edit distance for any remaining names.
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \donttest{ # this example requires 'rWCVPdata'
#' if(requireNamespace("rWCVPdata")){
#' wcvp_names <- rWCVPdata::wcvp_names
#' wcvp_match_fuzzy(redlist_example, wcvp_names, "scientificName")
#' }
#' }
#'
#' @family name matching functions
#'
wcvp_match_fuzzy <- function(names_df, wcvp_names, name_col, progress_bar = TRUE) {
  # check for duplicated names
  if (sum(duplicated(names_df[[name_col]])) > 10) {
    cli::cli_alert_warning("Fuzzy matching is very slow for data with repeated names. \nWe recommend matching a unique set of names to the WCVP, \nthen attaching the resolved matches to the original data using e.g. `*_join` functions.")
    invisible(readline("Press [Enter] to continue or [Escape] to exit:"))
  }

  wcvp_species <- filter(wcvp_names, .data$taxon_rank != "Genus")

  phonetic_matches <-
    names_df %>%
    phonetic_match(wcvp_species, name_col = name_col) %>%
    filter(!is.na(.data$wcvp_id))

  matched_names <- phonetic_matches[[name_col]]

  unmatched <-
    names_df %>%
    filter(.data[[name_col]] %notin% matched_names)

  if (nrow(unmatched) == 0) {
    return(phonetic_matches)
  }

  if (progress_bar == TRUE) {
    edit_matches <- edit_match(unmatched, wcvp_species, name_col = name_col)
  } else {
    edit_matches <- suppressMessages(edit_match(unmatched, wcvp_species, name_col = name_col))
  }


  if (nrow(phonetic_matches) > 0) {
    return(
      phonetic_matches %>%
        bind_rows(edit_matches)
    )
  } else {
    return(edit_matches)
  }
}

#' @rdname wcvp_match_fuzzy
#'
#' @details Phonetic matching uses [phonics::metaphone] encoding with a maximum
#' code length of 20.
#'
#' @importFrom phonics metaphone
#' @importFrom RecordLinkage levenshteinSim
#' @importFrom utils adist
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \donttest{ # this example requires 'rWCVPdata'
#' if(requireNamespace("rWCVPdata")){
#' wcvp_names <- rWCVPdata::wcvp_names
#' phonetic_match(redlist_example, wcvp_names, "scientificName")
#' }
#' }
#'
phonetic_match <- function(names_df, wcvp_names, name_col) {
  original_names <- colnames(names_df)
  wcvp_names$mp <- metaphone(wcvp_names$taxon_name, maxCodeLen = 20, clean = FALSE)

  matches <-
    names_df %>%
    mutate(sanitised_ = sanitise_names_(.data[[name_col]])) %>%
    mutate(mp = metaphone(.data$sanitised_, maxCodeLen = 20, clean = FALSE)) %>%
    left_join(wcvp_names, by = "mp", suffix = c("", "_wcvp_names"))

  matches <-
    matches %>%
    mutate(
      match_similarity = levenshteinSim(.data$sanitised_, .data$taxon_name),
      match_similarity = round(.data$match_similarity, 3)
    ) %>%
    rowwise() %>%
    mutate(match_edit_distance = adist(.data$sanitised_, .data$taxon_name)[, 1]) %>%
    ungroup()

  matches <-
    matches %>%
    mutate(match_type = case_when(
      .data$match_similarity > 0.9 ~ "Fuzzy (phonetic)",
      .data$match_similarity >= 0.75 ~ "Fuzzy (phonetic)",
      TRUE ~ NA_character_
    )) %>%
    mutate(
      match_similarity = ifelse(.data$match_similarity < 0.75, NA_real_, .data$match_similarity),
      match_edit_distance = ifelse(is.na(.data$match_type), NA_real_, .data$match_edit_distance)
    ) %>%
    add_count(.data[[name_col]]) %>%
    mutate(multiple_matches = .data$n > 1) %>%
    select(-"n")

  matches %>%
    format_output_(original_cols = original_names) %>%
    mutate(across(starts_with("wcvp_"), ~ ifelse(is.na(.data$match_type), NA, .x)))
}

#' @rdname wcvp_match_fuzzy
#'
#' @details Edit distance matching finds the closest match based on Levenshtein similarity,
#' calculated by [RecordLinkage::levenshteinSim].
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom cli cli_progress_along
#' @importFrom purrr map
#'
#' @export
#'
#' @examples
#' \donttest{ # this example requires 'rWCVPdata'
#' if(requireNamespace("rWCVPdata")){
#' wcvp_names <- rWCVPdata::wcvp_names
#' edit_match(redlist_example, wcvp_names, "scientificName")
#' }
#' }
#'
edit_match <- function(names_df, wcvp_names, name_col) {

  original_names <- colnames(names_df)
  names_df <-
    names_df %>%
    filter(!is.na(.data[[name_col]])) %>%
    mutate(sanitised_ = sanitise_names_(.data[[name_col]]))

  matches <-
    names_df %>%
    mutate(match_info = map(
      .data$sanitised_, ~{edit_match_name_(.data$sanitised_[.x], wcvp_names)},
      .progress = list(
        type = "iterator",
        format = "Matching {cli::pb_bar} {cli::pb_percent} ETA {cli::pb_eta}",
        clear = TRUE)
    ))

  matches <-
    matches %>%
    unnest("match_info")

  matches %>%
    left_join(
      wcvp_names,
      by = c("plant_name_id", "taxon_name"),
      suffix = c("", "_wcvp")
    ) %>%
    format_output_(original_cols = original_names)
}

#' Match a name to a lookup table using Levenshtein similarity (edit distance).
#'
#' @import dplyr
#' @importFrom RecordLinkage levenshteinSim
#' @importFrom utils adist
#' @importFrom stringr str_extract str_remove_all str_squish
#' @importFrom rlang .data
#'
#' @noRd
#'
edit_match_name_ <- function(name, lookup) {
  genus <- str_extract(name, "^[^ ]+")
  genus_lookup <-
    lookup %>%
    filter(.data$genus == genus)

  similarity <- levenshteinSim(name, genus_lookup$taxon_name)

  best_idx <- which(similarity > 0.9)

  if (length(best_idx) == 0) {
    best_idx <- which(similarity == max(similarity) & similarity > 0.75)
  }

  best_name_id <- genus_lookup$plant_name_id[best_idx]
  best_name <- genus_lookup$taxon_name[best_idx]

  if (length(best_name_id) == 0) {
    similarity <- levenshteinSim(name, lookup$taxon_name)
    best_idx <- which(similarity == max(similarity) & similarity > 0.75)
    best_name_id <- lookup$plant_name_id[best_idx]
    best_name <- lookup$taxon_name[best_idx]
  }

  if (length(best_name_id) == 0) {
    best_name_id <- NA_real_
    best_name <- NA_character_
    best_similarity <- NA_real_
    match_type <- NA_character_
  } else {
    best_similarity <- round(similarity[best_idx], 3)
    match_type <- "Fuzzy (edit distance)"
  }

  data.frame(
    taxon_name = best_name,
    plant_name_id = best_name_id,
    match_similarity = best_similarity,
    match_edit_distance = as.vector(adist(name, best_name)),
    match_type = match_type,
    multiple_matches = length(best_name_id) > 1
  )
}
