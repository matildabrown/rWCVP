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
    "match_id"="plant_name_id",
    "match_name"="taxon_name",
    "match_authors"="taxon_authors",
    "match_rank"="taxon_rank",
    "match_status"="taxon_status",
    "match_homotypic"="homotypic_synonym",
    "match_ipni_id"="ipni_id",
    "match_accepted_id"="accepted_plant_name_id"
  )

  output %>%
    select(all_of(original_cols), all_of(output_cols))
}
