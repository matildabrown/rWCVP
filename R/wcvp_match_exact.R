#' Exact matching to WCVP.
#'
#' Exact matching of names to the WCVP, optionally using the author string to
#' refine results.
#'
#' @inherit wcvp_match_fuzzy return params
#' @param author_col the column in `names_df` that has the name authority, to aid
#'  matching. Set to `NULL` to match with no author string.
#' @param id_col the column in `names_df` that has the observation id.
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' wcvp_names <- rWCVPdata::wcvp_names
#'
#' # including author string
#' wcvp_match_exact(redlist_example, wcvp_names, "scientificName", author_col="authority",
#' id_col = "assessmentId")
#'
#' # without author string
#' wcvp_match_exact(redlist_example, wcvp_names, "scientificName", id_col = "assessmentId")
#'
#' @family name matching functions
#'
wcvp_match_exact <- function(names_df, wcvp_names, name_col, author_col=NULL, id_col) {
  original_names <- colnames(names_df)

  match_type <- ifelse(is.null(author_col), "Exact (without author)",
                       "Exact (with author)")

  join_key <- "wcvp_sanitised_"
  join_names <- "sanitised_"
  if (! is.null(author_col)) {
    join_key <- c(join_key, "taxon_authors")
    join_names <- c(join_names, author_col)
  }
  names(join_key) <- join_names

  wcvp_names$wcvp_sanitised_ <- sanitise_names_(wcvp_names$taxon_name)

  matches <-
    names_df %>%
    mutate(sanitised_=sanitise_names_(.data[[name_col]])) %>%
    left_join(
      wcvp_names,
      by=join_key,
      keep=TRUE,
      na_matches="never",
      suffix=c("", "_wcvp")
    )

  matches <-
    matches %>%
    mutate(match_type=ifelse(is.na(.data$taxon_name), NA_character_, match_type),
           match_similarity=ifelse(is.na(.data$taxon_name), NA_real_, 1),
           match_edit_distance=ifelse(is.na(.data$taxon_name), NA_real_, 0)) %>%
    add_count(.data[[id_col]]) %>%
    mutate(multiple_matches=.data$n > 1) %>%
    select(-.data$n)

  matches %>%
    format_output_(original_cols=original_names)
}
