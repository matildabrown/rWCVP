#' Match names to the WCVP.
#'
#' Match names to WCVP, first using exact matching and then using fuzzy matching
#' on any remaining unmatched names.
#'
#' @inherit exact_match return params
#' @param id_col a column in `names_df` with a unique ID for each name. Will be
#'  created from the row number if not provided.
#' @param join_cols a character vector of name parts to make the taxon name,
#'  if `name_col` is not provided.
#' @param fuzzy whether or not fuzzy matching should be used for names that could
#'  not be matched exactly.
#'
#' @details By default, exact matching uses only the taxon name (supplied by `name_col`)
#'  unless a column specifying the author string is provided (as `author_col`).
#'
#'  Columns setting out name parts can be supplied as `join_cols` in place of a
#'  taxon name, but must be supplied in the order you want them joined
#'  (e.g. `c("genus", "species", "infra_rank", "infra")`).
#'
#'  Fuzzy matching uses a combination of phonetic and edit distance matching,
#'  and can optionally be turned off.
#'
#'  The WCVP can be loaded for matching from [rWCVPdata::wcvp_names].
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom utils adist
#' @importFrom tidyr unite
#' @import dplyr
#'
#' @examples
#' wcvp <- rWCVPdata::wcvp_names
#'
#' # without author
#' match_names(redlist_example, wcvp, name_col="scientificName",
#'             id_col="assessmentId")
#'
#' # with author
#' match_names(redlist_example, wcvp, name_col="scientificName",
#'             id_col="assessmentId", author_col="authority")
#'
#' @family name matching functions
#'
match_names <- function(names_df, wcvp, name_col=NULL, id_col=NULL, author_col=NULL,
                        join_cols=NULL, fuzzy=TRUE) {
  latest_version <- rWCVPdata::check_wcvp_version(silent=TRUE)
  if(! latest_version) message("Warning: This version of WCVP is out of date, see check_wcvp_version() for details.")

  # warning for missing name info
  if (is.null(name_col) & is.null(join_cols)) {
    stop("Names not supplied - use either name_col or join_cols to supply names")
  }

  # get taxon name from name parts
  if(is.null(name_col)){
    name_col <- "original_name"
    names_df <- unite(names_df, "original_name", all_of(join_cols), sep=" ",
                      remove=FALSE, na.rm=TRUE)
  }

  if (is.null(id_col)) {
    names_df$id <- 1:nrow(names_df)
    id_col <- "id"
  }

  # rename authors
  if(is.null(author_col)) {
    message("No author information supplied - matching on taxon name only")
  }

  #set up id col for returned df
  names_df$row_order <- 1:nrow(names_df)
  n_names <- length(unique(names_df[[name_col]]))

  message(glue::glue("________________________________________________________________________________
                     Matching {n_names} names..."))

  unmatched <- names_df

  # 1. Match within WCVP including authority if present ####
  matches <-
    unmatched %>%
    exact_match(wcvp, name_col=name_col, author_col=author_col) %>%
    filter(! is.na(.data$match_id))

  multi_matches <-
    matches %>%
    filter(.data$match_type == "Multiple matches found") %>%
    distinct(.data[[id_col]], .keep_all=TRUE)

  n_matched <- length(unique(matches[[name_col]]))

  message(glue::glue("--------------------------------------------------------------------------------\n",
               "Searching WCVP with{ifelse(is.null(author_col), ' no', '')} author:",
               "resolved matches for {n_matched} ",
               "out of {n_names} names."))

  message(glue::glue("Multiple matches found for {nrow(multi_matches)} names."))

  # Only names without authors or those that were not matched before
  unmatched <- filter(names_df, ! .data[[id_col]] %in% matches[[id_col]])

  # 2. Match within WCVP excluding authority, if not done already ####
  # (this includes names that could not be matched using authority from step 1)
  if (! is.null(author_col)) {
    n_names <- length(unique(unmatched[[name_col]]))

    # Match names
    matches_no_author <-
      unmatched %>%
      exact_match(wcvp, name_col=name_col, author_col=NULL) %>%
      filter(! is.na(.data$match_id))

    n_matched <- length(unique(matches_no_author[[name_col]]))

    multi_matches <-
      matches_no_author %>%
      filter(.data$match_type == "Multiple matches found") %>%
      distinct(.data[[id_col]], .keep_all=TRUE)

    matches <- bind_rows(matches, matches_no_author)
    message(glue::glue("---\n",
                      "Searching WCVP without author: found matches for {n_matched} ",
                       "out of {n_names} names."))

    message(glue::glue("Multiple matches found for {nrow(multi_matches)} names."))
  }

  #_____________________________________________________________________________
  # 3. Fuzzy matching  ####
  unmatched <- filter(names_df, ! .data[[id_col]] %in% matches[[id_col]])
  if(fuzzy){
    message(glue::glue("---
                       Fuzzy matching {length(unique(unmatched[[name_col]]))} names...this may take some time"))
    fuzzy_matches <- fuzzy_match(unmatched, wcvp, name_col=name_col)
    message(glue::glue("Fuzzy matched {sum(!is.na(unique(fuzzy_matches$match_name)))} ",
                       "out of {length(unique(unmatched[[name_col]]))} names."))

    matches <- bind_rows(matches, fuzzy_matches)
  }
#_____________________________________________________________________________
#
#   # Compilation of matched results  ####
  unmatched <- filter(names_df, ! .data[[id_col]] %in% matches[[id_col]])
  unmatched$match_type <- "No match found"

  matches <-
    matches %>%
    bind_rows(unmatched) %>%
    arrange(.data$row_order) %>%
    select(-"row_order")

  n_matched <-
    matches %>%
    filter(!is.na(.data$match_id)) %>%
    distinct(.data[[name_col]]) %>%
    nrow()

  message(glue::glue("---
                     Matching complete - {n_matched}",
                     "names matched out of {length(unique(names_df[[name_col]]))}
                     ________________________________________________________________________________"))

  if(! is.null(author_col)){
    matches <-
      matches %>%
      rowwise() %>%
      mutate(author_edit_distance=adist(.data[[author_col]], .data$match_authors)[,1],
             author_lcs=length_lcs(.data[[author_col]], .data$match_authors)) %>%
      ungroup()
  }

  matches
}


