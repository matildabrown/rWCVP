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
