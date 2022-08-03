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
#' @importFrom stringr str_detect
#' @import dplyr
#' @import cli
#'
#' @examples
#' wcvp_names <- rWCVPdata::wcvp_names
#'
#' # without author
#' match_names(redlist_example, wcvp_names, name_col="scientificName",
#'             id_col="assessmentId")
#'
#' # with author
#' match_names(redlist_example, wcvp_names, name_col="scientificName",
#'             id_col="assessmentId", author_col="authority")
#'
#' @family name matching functions
#'
match_names <- function(names_df, wcvp_names=NULL, name_col=NULL, id_col=NULL, author_col=NULL,
                        join_cols=NULL, fuzzy=TRUE) {

  cli_h1("Matching names to WCVP")
  if (is.null(wcvp_names)) {
    wcvp_names <- rWCVPdata::wcvp_names
    latest_version <- rWCVPdata::check_wcvp_version(silent=TRUE)
    if(! latest_version) {
      cli_alert_danger("This version of WCVP is out of date, see {.fun check_wcvp_version} for details.")
    }
  }
  label_col <- name_col

  # warning for missing name info
  if (is.null(name_col) & is.null(join_cols)) {
    cli_abort("Names not supplied - use either {.arg name_col} or {.arg join_cols} to supply names")
  }

  # get taxon name from name parts
  if(is.null(name_col)){
    name_col <- "original_name"
    names_df <- unite(names_df, "original_name", all_of(join_cols), sep=" ",
                      remove=FALSE, na.rm=TRUE)
    label_col <- join_cols
  }

  cli_alert_info("Using the {.var {label_col}} column{?s}")

  if (is.null(id_col)) {
    names_df$internal_id <- 1:nrow(names_df)
    id_col <- "internal_id"
  }

  # rename authors
  if(is.null(author_col)) {
    cli_alert_warning("No author information supplied - matching on taxon name only")
  }

  #set up id col for returned df
  names_df$row_order <- 1:nrow(names_df)
  n_names <- length(unique(names_df[[name_col]]))

  unmatched <- names_df

  # 1. Match within WCVP including authority if present ####
  cli_h2("Exact matching {n_names} name{?s}")
  matches <-
    unmatched %>%
    exact_match(wcvp_names, name_col=name_col, author_col=author_col, id_col=id_col) %>%
    filter(! is.na(.data$wcvp_id))

  # Only names without authors or those that were not matched before
  unmatched <- filter(names_df, ! .data[[id_col]] %in% matches[[id_col]])

  # 2. Match within WCVP excluding authority, if not done already ####
  # (this includes names that could not be matched using authority from step 1)
  if (! is.null(author_col)) {
    n_names <- length(unique(unmatched[[name_col]]))

    # Match names
    matches_no_author <-
      unmatched %>%
      exact_match(wcvp_names, name_col=name_col, author_col=NULL, id_col=id_col) %>%
      filter(! is.na(.data$wcvp_id))

    matches <- bind_rows(matches, matches_no_author)
  }

  n_matched <- length(unique(matches[[name_col]]))

  cli_alert_success("Found {n_matched} of {n_names} names")

  #_____________________________________________________________________________
  # 3. Fuzzy matching  ####
  unmatched <- filter(names_df, ! .data[[id_col]] %in% matches[[id_col]])
  if(fuzzy){
    cli_h2("Fuzzy matching {length(unique(unmatched[[name_col]]))} name{?s}")
    fuzzy_matches <- fuzzy_match(unmatched, wcvp_names, name_col=name_col)

    cli_alert_success("Found {sum(!is.na(unique(fuzzy_matches$wcvp_name)))} of {length(unique(unmatched[[name_col]]))} names")

    matches <- bind_rows(matches, fuzzy_matches)
  }
#_____________________________________________________________________________
#
#   # Compilation of matched results  ####
  unmatched <- filter(names_df, ! .data[[id_col]] %in% matches[[id_col]])
  if(nrow(unmatched)>0) unmatched$match_type <- "No match found"

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
    distinct(.data[[name_col]], .keep_all=TRUE) %>%
    pull(.data$multiple_matches) %>%
    sum(na.rm=TRUE)

  match_summary <-
    matches %>%
    distinct(.data[[name_col]], .keep_all=TRUE) %>%
    count(.data$match_type) %>%
    filter(!is.na(.data$match_type)) %>%
    tibble::deframe()

  cli_h2("Matching complete!")
  cli_alert_success("Matched {n_matched} of {length(unique(names_df[[name_col]]))} name{?s}")
  for (i in seq_along(match_summary)) {
    cli_alert_info("{names(match_summary)[i]}: {.value {match_summary[i]}}")
  }
  cli_alert_warning("Names with multiple matches: {multi_matches}")

  if(! is.null(author_col)){
    matches <-
      matches %>%
      rowwise() %>%
      mutate(wcvp_author_edit_distance=adist(.data[[author_col]], .data$wcvp_authors)[,1],
             wcvp_author_lcs=length_lcs(.data[[author_col]], .data$wcvp_authors)) %>%
      ungroup()
  }

  if ("internal_id" %in% colnames(matches)) {
    matches <- select(matches, -"internal_id")
  }

  matches
}


