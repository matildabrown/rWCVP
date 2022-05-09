#' Match names to the WCVP
#'
#' @param x data.frame of names to match
#' @param taxon.name Character. The name of the variable in \code{x}
#'     containing the genus/binomial/trinomial taxon name. Defaults to NULL.
#'     Note: if using default, \code{genus} and \code{spec} \emph{must} be
#'     supplied.
#' @param id Character. The name of the variable in \code{x} containing the
#'     identifier (e.g. Red List assessment ID). Defaults to NULL.
#' @param authors Character. The name of the variable in \code{x} containing
#'     the author information. Defaults to NULL.
#' @param genus Character. The name of the variable in \code{x} containing the
#'     genus name. Defaults to NULL.
#' @param spec Character. The name of the variable in \code{x} containing the
#'     specific epithet. Defaults to NULL.
#' @param infra.rank Character. The name of the variable in \code{x} containing the
#'     infraspecific rank (e.g. subsp., var.). Defaults to NULL.
#' @param infra Character. The name of the variable in \code{x} containing the
#'     infraspecific epithet. Defaults to NULL.
#' @param fuzzy Logical. Perform fuzzy matching? Defaults to TRUE.
#' @return data.frame consisting of the input \code{x} with name matching variables appended (see Details)
#' @export
#' @import rWCVPdata
#' @importFrom rlang .data
#' @import dplyr
#'
#' @examples
#' # without author
#' wcvp_name_match(redlist_example, taxon.name = "scientificName")
#' # with author
#' wcvp_name_match(redlist_example, taxon.name = "scientificName", authors = "authority")
wcvp_name_match <- function(x,
                            taxon.name = NULL,
                            id = NULL,
                            authors = NULL,
                            genus = NULL,
                            spec = NULL,
                            infra.rank = NULL,
                            infra = NULL,
                            fuzzy = TRUE){

  taxon.authors <- . <- plant_name_id <- row.order <- taxon_name <- full.col <-   NULL


  version <- rWCVPdata::check_wcvp_version(silent=TRUE)
  if(version==FALSE) message("Warning: This version of WCVP is out of date, see check_wcvp_version() for details.")

  # warning for missing name info
  if (is.null(taxon.name) &
      is.null(genus) &
      is.null(spec)) stop("Names not supplied - use either
                             taxon.name or genus & spec to supply names")

  # get taxon name from name parts
  if(is.null(taxon.name)){
    name_cols <- c(genus, spec, infra.rank, infra)[which(!is.null(c(genus, spec, infra.rank, infra)))]
    x$taxon.name <- paste(x[,name_cols], sep=" ")
    x$taxon.name <- gsub("NA ","",x$taxon.name)
    } else {colnames(x)[which(colnames(x)==taxon.name)] <- "taxon.name"}

  if (!is.null(id)) {
    colnames(x)[which(colnames(x)==id)] <- "id"
  } else { x$id <- 1:nrow(x)}


  # rename authors
  if(is.null(authors)) {
    message("No author information supplied - matching on taxon name only")
  } else {
    colnames(x)[which(colnames(x)==authors)] <- "taxon.authors"
    x$full.col <- paste(x$taxon.name, x$taxon.authors)
    x$full.col <- gsub("NA ","",x$full.col)
    }


  #set up id col for returned df
  x$row.order <- 1:nrow(x)

  #get original number of columns
  ncol.x <- ncol(x)

  #column names of input (modified as above) and output
  new.cols <- colnames(x)
  end.cols <- c(new.cols, "plant_name_id","taxon_name","taxon_rank","taxon_status","taxon_authors","accepted_plant_name_id","homotypic_synonym")

  message(glue::glue("________________________________________________________________________________
                     Matching {length(unique(x$taxon.name))} names..."))

  #Names already in WCVP (for Steps 1 and 2)
  x_inwcvp <- x[which(x$taxon.name %in% rWCVPdata::wcvp_names$taxon_name),]
  #_____________________________________________________________

  if ("taxon.authors" %in% new.cols){
    # 1. Match within WCVP including authority ####

    # Only names with authors
    x_inwcvp_auth <- filter(x_inwcvp, !is.na(taxon.authors))

    # Match names
    matched_inwcvp_auth <- x_inwcvp_auth %>%
      left_join(., rWCVPdata::wcvp_names %>%
                         mutate(taxon_name2= .data$taxon_name,
                                       taxon_authors2= .data$taxon_authors),
                by=c("taxon.name"="taxon_name2",
                     "taxon.authors"="taxon_authors2"))

    res_inwcvp_auth <-  matched_inwcvp_auth %>%
      select(all_of(end.cols)) %>%
      filter(!is.na(plant_name_id)) %>%
      mutate(match_type = "Matched in WCVP (with author)",
                    match_similarity = 1,
                    match_edit_distance = 0)

    multi_matches <- unique(res_inwcvp_auth$id[which(duplicated(res_inwcvp_auth$id)==TRUE)])

    res_inwcvp_auth[which(res_inwcvp_auth$id %in% multi_matches), "match_type"] <- "Multiple matches found"


    message(glue::glue("--------------------------------------------------------------------------------
                 Searching WCVP with author: resolved matches for {length(unique(res_inwcvp_auth$taxon.name))} ",
                 "out of {length(unique(x$taxon.name))} names."))

    if(length(multi_matches)>0)   message(glue::glue("Multiple matches found for {length(multi_matches)} names."))

    # Only names without authors or those that were not matched before
    x_inwcvp_noauth <- filter(x_inwcvp, is.na(taxon.authors)) %>%
      rbind(.,filter(matched_inwcvp_auth,is.na(plant_name_id)) %>%
              select(1:ncol.x))

    matches <- res_inwcvp_auth
  } else {
    x_inwcvp_noauth <- x_inwcvp
  }
  #_____________________________________________________________________________

  # 2. Match within WCVP excluding authority  ####
  # (this includes names that could not be matched using authority from step 1)

  # Match names
  matched_inwcvp_noauth <- x_inwcvp_noauth %>%
    left_join(., rWCVPdata::wcvp_names %>%
                       mutate(taxon_name2=taxon_name),
              by=c("taxon.name"="taxon_name2"))

  res_inwcvp_noauth <-  matched_inwcvp_noauth %>%
    select(all_of(end.cols)) %>%
    mutate(match_type = "Matched in WCVP (without author)",
                  match_similarity = 1,
                  match_edit_distance = 0)

  multi_matches <- unique(res_inwcvp_noauth$id[which(duplicated(res_inwcvp_noauth$id)==TRUE)])

  res_inwcvp_noauth[which(res_inwcvp_noauth$id %in% multi_matches), "match_type"] <- "Multiple matches found"


  if ("taxon.authors" %in% new.cols){
    matches <- bind_rows(matches, res_inwcvp_noauth)
    message(glue::glue("---
                     Searching WCVP without author: found matches for {length(unique(res_inwcvp_noauth$taxon.name))} ",
                     "out of {length(unique(x$taxon.name))-length(unique(res_inwcvp_auth$taxon.name))} names."))
  }else{
    matches <- res_inwcvp_noauth
    message(glue::glue("---
                     Searching WCVP without author: found matches for {length(unique(res_inwcvp_noauth$taxon.name))} ",
               "out of {length(unique(x$taxon.name))} names."))
  }


if(length(multi_matches)>0)   message(glue::glue("Multiple matches found for {length(multi_matches)} names."))
  #_____________________________________________________________________________
  x_notinwcvp <-  x[which(x$taxon.name %notin% rWCVPdata::wcvp_names$taxon_name),]

  if(fuzzy==TRUE){

  # 3. Fuzzy matching  ####
  message(glue::glue("---
                     Fuzzy matching {length(unique(x_notinwcvp$taxon.name))} names...this may take some time"))
  fuzzy_matches <- wcvp_fuzzy_match(x_notinwcvp)
  message(glue::glue("Fuzzy matched {length(which(!is.na(unique(fuzzy_matches$taxon_name))))} ",
                     "out of {length(unique(x_notinwcvp$taxon.name))} names."))

  matches <- bind_rows(matches, fuzzy_matches)
  }
#_____________________________________________________________________________
#
#   # Compilation of matched results  ####

  matches <- matches %>%
    arrange(all_of(row.order)) %>%
    select(-all_of(row.order))
  message(glue::glue("---
                     Matching complete - {length(unique(matches$taxon.name[which(!is.na(matches$plant_name_id))]))} names matched out of {length(unique(x$taxon.name))}
                     ________________________________________________________________________________"))

  matches %>%
    select(-all_of(row.order)) %>%
    select(-all_of(full.col))

  if(!is.null(authors)){
    matches <- matches %>%
    mutate(author_edit_distance = diag(utils::adist(matches$taxon.authors, matches$taxon_authors)),
           author_lcs = mapply(length_lcs, matches$taxon.authors, matches$taxon_authors))
  }

  # rename cols
  if(is.null(taxon.name)){
    matches <- select(matches, -taxon.name)
  } else {colnames(matches)[which(colnames(matches)=="taxon.name")] <- taxon.name}

  if (!is.null(id)) {
    colnames(matches)[which(colnames(matches)==id)] <- "id"
  } else { matches <- select(matches, -id)}

matches <- select(matches, -full.col) %>%
  select(-row.order)

  # rename authors
  if(!is.null(authors)) {
    colnames(matches)[which(colnames(matches)=="taxon.authors")] <- authors
  }

  matches <- matches %>%
    rename(wcvp_plant_name_id = .data$plant_name_id,
                  wcvp_taxon_name = .data$taxon_name,
                  wcvp_taxon_rank = .data$taxon_rank,
                  wcvp_taxon_status = .data$taxon_status,
                  wcvp_taxon_authors = .data$taxon_authors,
                  wcvp_accepted_plant_name_id = .data$accepted_plant_name_id,
                  wcvp_homotypic_synonym = .data$homotypic_synonym)

  return(matches)

  }
