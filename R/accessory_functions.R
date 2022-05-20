# Accessory functions

`%notin%` <- Negate(`%in%`)

#function to format the text correctly
html <- function(text, replace_sign_entities = FALSE, ...) {

  if(replace_sign_entities) {
    text <- stringr::str_replace_all(text, c("&lt;" = "<", "&gt;" = ">"))
  }

  htmltools::HTML(text, ...)
}

longest_string <- function(s){return(s[which.max(nchar(s))])}

lcsbstr <- function(a,b) {
  matches <- gregexpr("M+", drop(attr(utils::adist(a, b, counts=TRUE), "trafos")))[[1]];
  lengths<- attr(matches, 'match.length')
  which_longest <- which.max(lengths)
  index_longest <- matches[which_longest]
  length_longest <- lengths[which_longest]
  longest_cmn_sbstr  <- substring(longest_string(c(a,b)), index_longest , index_longest + length_longest - 1)
  return(longest_cmn_sbstr )
}


length_lcs <- function(a,b) {
  matches <- gregexpr("M+", drop(attr(utils::adist(a, b, counts=TRUE), "trafos")))[[1]];
  lengths<- attr(matches, 'match.length')
  longest <- max(lengths)
 return(longest)
}

#' Format columns of name matching output.
#'
#' @import dplyr
#' @noRd
#'
format_output_ <- function(output, original_cols=NULL) {
  output_cols <- c(
    "match_type",
    "multiple_matches",
    "match_similarity",
    "match_edit_distance",
    "wcvp_id"="plant_name_id",
    "wcvp_name"="taxon_name",
    "wcvp_authors"="taxon_authors",
    "wcvp_rank"="taxon_rank",
    "wcvp_status"="taxon_status",
    "wcvp_homotypic"="homotypic_synonym",
    "wcvp_ipni_id"="ipni_id",
    "wcvp_accepted_id"="accepted_plant_name_id"
  )

  output %>%
    select(all_of(original_cols), all_of(output_cols))
}

#' Remove hybrid symbol from names.
#'
#' @importFrom stringr str_remove_all str_squish
#' @noRd
remove_hybrid_ <- function(names) {
  names <- str_remove_all(names, "(?<![A-Za-z])(x|\u00d7)")
  str_squish(names)
}

#' Standardise infraspecific ranks.
#'
#' @importFrom stringr str_replace_all
#' @noRd
standardise_infras_ <- function(names) {
  infras <- c(" subsp "=" subsp. ", " ssp "=" subsp. ", " ssp. "=" subsp. ",
              " var "=" var. ", " f "=" f. ", " forma "=" f. ")

  str_replace_all(names, infras)
}

#' Sanitise names.
#'
#' @noRd
#'
sanitise_names_ <- function(names) {
  names <- remove_hybrid_(names)
  standardise_infras_(names)
}
