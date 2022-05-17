#' Fuzzy (approximate) matching to the WCVP
#'
#' @param x data.frame containing the variables \code{id, taxon.name} to match
#'
#' @return data.frame consisting of \code{x} bound to \code{plant_name_id, taxon_name,
#' taxon_rank, taxon_status,}
#' \code{accepted_plant_name_id, homotypic_synonym} and
#' \code{match_type} (a descriptor of the match result)
#' @details This function first uses phonetic matching, using metaphone encoding
#' (via the \code{phonics} package) with a maximum codelength of 20. For names
#' that remain unmatched, fuzzy matching is performed using the Levenshtein
#' edit distance and similarity (standardised edit distance). It is strongly recommended that this function only be called using \code{wcvp_name_match}, as it is much more efficient to find any exact matches before proceeding to fuzzy matching.
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' #column names must include id and taxon.name
#' colnames(redlist_example)[1:2] <- c("id","taxon.name")
#' wcvp_fuzzy_match(redlist_example)
#'
wcvp_fuzzy_match <- function(x){

  if("taxon.name" %notin% colnames(x)) stop("There is no variable named taxon.name")

  wcvp_species <-
    rWCVPdata::wcvp_names %>%
    filter(.data$taxon_rank != "Genus")

  phonetic_matches <- x %>%
    phonetic_match(wcvp_species) %>%
    filter(!is.na(.data$match_id))

  unmatched <- x %>%
    filter(id %notin% phonetic_matches$id)

  fuzzy_matches <- fuzzy_match(unmatched, wcvp_species)

  matches <- phonetic_matches %>%
    bind_rows(fuzzy_matches) %>%
    left_join(
      wcvp_species %>%
        select("plant_name_id", "taxon_name",
               "ipni_id", "taxon_rank", "taxon_status", "family", "accepted_plant_name_id",
               "homotypic_synonym"),
      by=c("match_id"="plant_name_id", "match_name"="taxon_name")
    )

  x %>%
    left_join(
      matches,
      by="id"
    )
}

#' Phonetic matching
#'
#' @param x data.frame containing the variable \code{taxon.name} to match
#' @param wcvp_to_search data.frame containing selected variables from the WVCP,
#' without genus-level names
#'
#' @return data.frame with relevant WCVP information, plus match_type - an
#' informative column giving the match result and Levenshtein similarity to
#' original name (note that this is not the metric that is used to link the
#' names, see \code{fuzzy_match} for Levenshtein matching)
#'
#' @importFrom phonics metaphone
#' @importFrom RecordLinkage levenshteinSim
#' @importFrom utils adist
#' @import dplyr
#'
#' @noRd
#'
phonetic_match <- function(x, wcvp_to_search){

  wcvp_to_search$mp <- metaphone(wcvp_to_search$taxon_name, maxCodeLen = 20, clean=FALSE)

  x$mp <- metaphone(x$taxon.name, maxCodeLen = 20, clean=FALSE)

  x %>%
    left_join(wcvp_to_search, by="mp") %>%
    mutate(
      match_similarity=levenshteinSim(.data$taxon.name, .data$taxon_name),
      match_similarity=round(.data$match_similarity),
      match_edit_distance=diag(adist(.data$taxon.name, .data$taxon_name))
    ) %>%
    mutate(match_type=case_when(.data$match_similarity > 0.9 ~ "Fuzzy matched (phonetically)",
                                .data$match_similarity >= 0.75 ~ "Fuzzy matched (phonetically)",
                                TRUE ~ "No fuzzy match found")) %>%
    mutate(match_similarity=ifelse(.data$match_similarity < 0.75, NA_real_, .data$match_similarity)) %>%
    select("id", "match_name"="taxon_name", "match_id"="plant_name_id",
           "match_type", "match_similarity", "match_edit_distance")
}

#' Distance-based matching using Levenshtein similarity
#'
#' @param x data.frame containing the variable \code{taxon.name} to match
#' @param wcvp_to_search data.frame containing selected variables from the WVCP,
#' without genus-level names
#'
#' @return data.frame with relevant WCVP information, plus match_type - an
#' informative column giving the match result and Levenshtein similarity (as
#' implemented in the \code{RecordLinkage} package)
#'
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#'
#' @noRd
#'
fuzzy_match <- function(x, wcvp_to_search){
  x <- x[!is.na(x$taxon.name),]

  matches <-
    x %>%
    filter(!is.na(.data$taxon.name)) %>%
    rowwise() %>%
    mutate(match_info=list(fuzzy_match_name_(.data$taxon.name, wcvp_to_search))) %>%
    select("id", "taxon.name", "match_info") %>%
    unnest(.data$match_info)

  matches %>%
    left_join(wcvp_to_search, by=c("match_id"="plant_name_id", "match_name"="taxon_name")) %>%
    select("id", "match_name", "match_id", "match_type",
           "match_similarity", "match_edit_distance")
}

#' Match a name to a lookup table using Levenshtein similarity.
#'
#' @import dplyr
#' @importFrom RecordLinkage levenshteinSim
#' @importFrom utils adist
#' @importFrom stringr str_extract
#' @importFrom rlang .data
#'
#' @noRd
#'
fuzzy_match_name_ <- function(name, lookup) {
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
    best_name_id <- NA_character_
    best_name <- NA_character_
    best_similarity <- NA_real_
    match_type <- "No fuzzy match found"
  } else {
    best_similarity <- round(similarity[best_idx], 3)
    match_type <- "Fuzzy matched (edit distance)"
  }

  if (length(best_name_id) > 1) {
    match_type <- "Multiple matches found"
  }

  data.frame(
    match_name=best_name,
    match_id=best_name_id,
    match_similarity=best_similarity,
    match_edit_distance=as.vector(adist(name, best_name)),
    match_type=match_type
  )
}

