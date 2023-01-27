#' Match names to the WCVP.
#'
#' Match names to WCVP, first using exact matching and then using fuzzy matching
#' on any remaining unmatched names.
#'
#' @inherit wcvp_match_exact return params
#' @param id_col Character. A column in `names_df` with a unique ID for each
#'   name. Will be created from the row number if not provided.
#' @param join_cols Character. A vector of name parts to make the taxon name,
#'  if `name_col` is not provided.
#' @param fuzzy Logical; whether or not fuzzy matching should be used for names
#'   that could not be matched exactly.
#' @param progress_bar Logical. Show progress bar when matching? Defaults to
#'  `TRUE`; should be changed to `FALSE` if used in a markdown report.
#'
#' @details By default, exact matching uses only the taxon name (supplied by `name_col`)
#'  unless a column specifying the author string is provided (as `author_col`).
#'
#'  Columns setting out name parts can be supplied as `join_cols` in place of a
#'  taxon name, but must be supplied in the order you want them joined
#'  (e.g. `c("genus", "species", "infra_rank", "infra")`).
#'
#'  Fuzzy matching uses a combination of phonetic and edit distance matching,
#'  and can optionally be turned off using `fuzzy=FALSE`.
#'
#'  The WCVP can be loaded for matching from [`rWCVPdata::wcvp_names`](https://matildabrown.github.io/rWCVPdata/).
#'
#'  See \href{https://matildabrown.github.io/rWCVP/articles/redlist-name-matching.html}{here} for an example workflow.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom utils adist
#' @importFrom tidyr unite
#' @importFrom stringr str_detect
#' @import dplyr
#' @import cli
#'
#' @examples
#' \dontrun{
#' wcvp_names <- rWCVPdata::wcvp_names
#'
#' # without author
#' wcvp_match_names(redlist_example, wcvp_names,
#'   name_col = "scientificName",
#'   id_col = "assessmentId"
#' )
#'
#' # with author
#' wcvp_match_names(redlist_example, wcvp_names,
#'   name_col = "scientificName",
#'   id_col = "assessmentId", author_col = "authority"
#' )
#' }
#'
#' @family name matching functions
#'
wcvp_match_names <- function(names_df, wcvp_names = NULL, name_col = NULL, id_col = NULL, author_col = NULL,
                             join_cols = NULL, fuzzy = TRUE, progress_bar = TRUE) {
  cli_h1("Matching names to WCVP")
  if (is.null(wcvp_names)) {
    .wcvp_available()
    .wcvp_fresh()
    wcvp_names <- rWCVPdata::wcvp_names
  }

  label_col <- name_col

  # warning for missing name info
  if (is.null(name_col) & is.null(join_cols)) {
    cli_abort("Names not supplied - use either {.arg name_col} or {.arg join_cols} to supply names")
  }

  # get taxon name from name parts
  if (is.null(name_col)) {
    name_col <- "original_name"
    names_df <- unite(names_df, "original_name", all_of(join_cols),
      sep = " ",
      remove = FALSE, na.rm = TRUE
    )
    label_col <- join_cols
  }

  cli_alert_info("Using the {.var {label_col}} column{?s}")

  if (is.null(id_col)) {
    names_df$internal_id <- seq_len(nrow(names_df))
    id_col <- "internal_id"
  }

  # rename authors
  if (is.null(author_col)) {
    cli_alert_warning("No author information supplied - matching on taxon name only")
    n_names1 <- nrow(unique(names_df[[name_col]]))
  } else {
    n_names1 <- nrow(unique(cbind(names_df[[name_col]], names_df[[author_col]])))
  }

  matchcols <- c(
    "match_type", "multiple_matches", "match_similarity", "match_edit_distance",
    "wcvp_id", "wcvp_name", "wcvp_authors", "wcvp_rank", "wcvp_status",
    "wcvp_homotypic", "wcvp_ipni_id", "wcvp_accepted_id", "wcvp_author_edit_distance", "wcvp_author_lcs"
  )

  if (length(
    setdiff(colnames(names_df), matchcols)
  ) < length(colnames(names_df))
  ) {
    cli_abort("Column names of input data frame must not contain any of the following: {matchcols}")
  }

  # set up id col for returned df
  names_df$row_order <- seq_len(nrow(names_df))

  unmatched <- names_df

  # 1. Match within WCVP including authority if present ####
  cli_h2("Exact matching {n_names1} name{?s}")
  matches <-
    unmatched %>%
    wcvp_match_exact(wcvp_names, name_col = name_col, author_col = author_col, id_col = id_col) %>%
    filter(!is.na(.data$wcvp_id))

  # Only names without authors or those that were not matched before
  unmatched <- filter(names_df, !.data[[id_col]] %in% matches[[id_col]])

  # 2. Match within WCVP excluding authority, if not done already ####
  # (this includes names that could not be matched using authority from step 1)
  if (!is.null(author_col)) {
    n_names2 <- length(unique(unmatched[[name_col]]))

    # Match names
    matches_no_author <-
      unmatched %>%
      wcvp_match_exact(wcvp_names, name_col = name_col, author_col = NULL, id_col = id_col) %>%
      filter(!is.na(.data$wcvp_id))

    matches <- bind_rows(matches, matches_no_author)
  }

  n_matched <- length(unique(matches[[name_col]]))

  cli_alert_success("Found {n_matched} of {n_names1} names")

  # 3. Use fuzzy maching on remaining names ####
  unmatched <- filter(names_df, !.data[[id_col]] %in% matches[[id_col]])
  if (fuzzy & nrow(unmatched) > 0) {
    cli_h2("Fuzzy matching {length(unique(unmatched[[name_col]]))} name{?s}")
    fuzzy_matches <- wcvp_match_fuzzy(unmatched, wcvp_names, name_col = name_col, progress_bar = progress_bar)

    cli_alert_success("Found {sum(!is.na(unique(fuzzy_matches$wcvp_name)))} of {length(unique(unmatched[[name_col]]))} names")

    matches <- bind_rows(matches, fuzzy_matches)
  }

  # 4. Compile results ####
  unmatched <- filter(names_df, !.data[[id_col]] %in% matches[[id_col]])
  if (nrow(unmatched) > 0) unmatched$match_type <- "No match found"

  matches <-
    matches %>%
    bind_rows(unmatched) %>%
    arrange(.data$row_order) %>%
    select(-"row_order")

  n_matched <-
    matches %>%
    filter(!is.na(.data$wcvp_id)) %>%
    distinct(.data[[name_col]]) %>%
    nrow()

  multi_matches <-
    matches %>%
    distinct(.data[[name_col]], .keep_all = TRUE) %>%
    pull(.data$multiple_matches) %>%
    sum(na.rm = TRUE)

  match_summary <-
    matches %>%
    distinct(.data[[name_col]], .keep_all = TRUE) %>%
    count(.data$match_type) %>%
    filter(!is.na(.data$match_type)) %>%
    tibble::deframe()

  cli_h2("Matching complete!")
  cli_alert_success("Matched {n_matched} of {length(unique(names_df[[name_col]]))} name{?s}")
  for (i in seq_along(match_summary)) {
    cli_alert_info("{names(match_summary)[i]}: {.value {match_summary[i]}}")
  }
  cli_alert_warning("Names with multiple matches: {multi_matches}")

  if (!is.null(author_col)) {
    matches <-
      matches %>%
      rowwise() %>%
      mutate(
        wcvp_author_edit_distance = adist(.data[[author_col]], .data$wcvp_authors)[, 1],
        wcvp_author_lcs = length_lcs(.data[[author_col]], .data$wcvp_authors)
      ) %>%
      ungroup()
  }

  if ("internal_id" %in% colnames(matches)) {
    matches <- select(matches, -"internal_id")
  }

  matches
}
