# Accessory functions

`%notin%` <- Negate(`%in%`)

#function to format the text correctly
html <- function(text, replace_sign_entities = FALSE, ...) {

  if(replace_sign_entities) {
    text <- stringr::str_replace_all(text, c("&lt;" = "<", "&gt;" = ">"))
  }

  htmltools::HTML(text, ...)
}
