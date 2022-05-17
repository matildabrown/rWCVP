#' Exact matching to WCVP.
#'
#' Exact matching of names to the WCVP, optionally using the author string to
#' refine results.
#'
#' @inherit fuzzy_match return params
#' @param author_col the column in `names_df` that has the name authority, to aid
#'  matching. Set to `NULL` to match with no author string.
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' wcvp <- rWCVPdata::wcvp_names
#'
#' # including author string
#' exact_match(redlist_example, wcvp, "scientificName", author_col="authority")
#'
#' # without author string
#' exact_match(redlist_example, wcvp, "scientificName")
#'
#' @family name matching functions
#'
exact_match <- function(names_df, wcvp, name_col, author_col=NULL) {
  original_names <- colnames(names_df)

  match_type <- ifelse(is.null(author_col),
                       "Matched in WCVP (without author)",
                       "Matched in WCVP (with author)")

  join_key <- "taxon_name"
  join_names <- name_col
  if (! is.null(author_col)) {
    join_key <- c(join_key, "taxon_authors")
    join_names <- c(join_names, author_col)
  }
  names(join_key) <- join_names

  matches <-
    names_df %>%
    left_join(
      wcvp,
      by=join_key,
      keep=TRUE,
      na_matches="never",
      suffix=c("", "_wcvp")
    )

  matches <-
    matches %>%
    mutate(match_type=ifelse(is.na(.data$taxon_name), "No exact match found", match_type),
           match_similarity=ifelse(is.na(.data$taxon_name), NA_real_, 1),
           match_edit_distance=ifelse(is.na(.data$taxon_name), NA_real_, 0)) %>%
    add_count(.data[[name_col]]) %>%
    mutate(match_type=ifelse(n > 1, "Multiple matches found", .data$match_type)) %>%
    select(-.data$n)

  matches %>%
    format_output_(original_cols=original_names)
}
