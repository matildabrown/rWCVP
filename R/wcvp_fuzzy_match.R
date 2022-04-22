

#' Fuzzy (approximate) matching to the WCVP
#'
#' @param x data.frame containing the variables \code{id, taxon.name} to match
#'
#' @return data.frame consisting of \code{x} bound to \code{plant_name_id, taxon_name,
#' taxon_rank, taxon_status, accepted_plant_name_id, homotypic_synonym} and
#' \code{match_type} (a descriptor of the match result)
#' @details This function first uses phonetic matching, using metaphone encoding
#' (via the \code{phonics} package) with a maximum codelength of 20. For names
#' that remain unmatched, fuzzy matching is performed using the Levenshtein
#' similarity
#' @export
#'
#' @examples
#' #column names must include id and taxon.name
#' colnames(redlist_example)[1:2] <- c("id","taxon.name")
#' wcvp_fuzzy_match(redlist_example)
#'
wcvp_fuzzy_match <- function(x){

  plant_name_id <- taxon_rank <- id <- NULL

  if("taxon.name" %notin% colnames(x)) stop("There is no variable named taxon.name")

  wcvp_to_search <- dplyr::filter(rWCVPdata::wcvp_names, taxon_rank != "Genus") %>%
    dplyr::select(dplyr::all_of(c("plant_name_id","genus","taxon_name","taxon_rank","taxon_status","taxon_authors",
                           "accepted_plant_name_id","homotypic_synonym")))

  x_phonetic <- phonetic_match(x, wcvp_to_search) %>%
    dplyr::filter(!is.na(plant_name_id))

  x_unmatched <- x %>%
    dplyr::filter(id %notin% x_phonetic$id)

  x_fuzzy <- fuzzy_match(x_unmatched, wcvp_to_search)



x_matched <- dplyr::left_join(x, rbind(x_phonetic, x_fuzzy), by="id")


  return(x_matched)

}



#' Phonetic matching - DEEXPORT WHEN FINISHED
#'
#' @param x data.frame containing the variable \code{taxon.name} to match
#' @param wcvp_to_search data.frame containing selected variables from the WVCP,
#' without genus-level names
#'
#' @return data.frame with relevant WCVP information, plus match_type - an
#' informative column giving the match result and Levenshtein similarity to
#' original name (note that this is not the metric that is used to link the
#' names, see \code{fuzzy_match} for Levenshtein matching)

phonetic_match <- function(x, wcvp_to_search){
  genus <- id <- plant_name_id <- taxon_name <- taxon_rank <- taxon_status <- taxon_authors <- accepted_plant_name_id <- homotypic_synonym <- match_type <- match_similarity <-  NULL

  wcvp_to_search$mp <- phonics::metaphone(wcvp_to_search$taxon_name, maxCodeLen = 20, clean=FALSE)


  x$mp <- phonics::metaphone(x$taxon.name,maxCodeLen = 20, clean=FALSE)

  x_join <- dplyr::left_join(x, wcvp_to_search, by="mp")

  x_join$match_similarity <- round(RecordLinkage::levenshteinSim(x_join$taxon.name, x_join$taxon_name),3)
  x_join$match_edit_distance <- diag(utils::adist(x_join$taxon.name, x_join$taxon_name))

  x_join[which(x_join$match_similarity < 0.75),"match_type"] <- paste0("No phonetic match with similarity >0.75 found")
  x_join[which(x_join$match_similarity >= 0.75),"match_type"] <- "Phonetically matched (weak)"
  x_join[which(x_join$match_similarity > 0.9),"match_type"] <- "Phonetically matched (strong)"


  x_join <- x_join %>%
    dplyr:: select(id, plant_name_id, taxon_name, taxon_rank, taxon_status, taxon_authors,
                   accepted_plant_name_id, homotypic_synonym, match_type, match_similarity, match_edit_distance)

  return(x_join)
}

#' Distance-based matching using Levenshtein similarity DEEXPORT WHEN FINISHED
#'
#' @param x data.frame containing the variable \code{taxon.name} to match
#' @param wcvp_to_search data.frame containing selected variables from the WVCP,
#' without genus-level names
#'
#' @return data.frame with relevant WCVP information, plus match_type - an
#' informative column giving the match result and Levenshtein similarity (as
#' implemented in the \code{RecordLinkage} package)

fuzzy_match <- function(x,wcvp_to_search){
  genus <- id <- plant_name_id <- taxon.name <-  taxon_name <- taxon_rank <- taxon_status <-taxon_authors <-  accepted_plant_name_id <- homotypic_synonym <- match_type <- NULL

  x <- x[!is.na(x$taxon.name),]
  x_genera <- strsplit(x$taxon.name," ")
  x_genera <- sapply(x_genera,"[[",1)

  matches <- NULL

  for (i in 1:nrow(x)){
    genus_i <- x_genera[i]
    wcvp_i <- wcvp_to_search %>%
      dplyr::filter(genus == genus_i)
    lev_sim <- RecordLinkage::levenshteinSim(x$taxon.name[i], wcvp_i$taxon_name)
    if(length(which(lev_sim>0.90)) >1){
      k <- which(lev_sim >0.90)
      bestname <- wcvp_i[k,]
      bestname$match_type <- "Multiple fuzzy matches found"
      bestname$id <- x$id[i]
      bestname$taxon.name <- x$taxon.name[i]
    }
    if(length(which(lev_sim>0.90)) == 1){
      k <- which.max(lev_sim)
      bestname <- wcvp_i[k,]
      bestname$match_type <- "Fuzzy matched (strong)"
      bestname$id <- x$id[i]
      bestname$taxon.name <- x$taxon.name[i]
    }
    if(length(which(lev_sim>0.90)) == 0){
      if(max(lev_sim)>0.75){
      k <- which.max(lev_sim)
      bestname <- wcvp_i[k,]
      bestname$match_type <- "Fuzzy matched (weak)"
      bestname$id <- x$id[i]
      bestname$taxon.name <- x$taxon.name[i]
      }else{
        lev_sim <- RecordLinkage::levenshteinSim(x$taxon.name[i], wcvp_to_search$taxon_name)
        if(max(lev_sim)>0.75){
          k <- which.max(lev_sim)
          bestname <- wcvp_i[k,]
          bestname$match_type <- "Fuzzy matched (weak)"
          bestname$id <- x$id[i]
          bestname$taxon.name <- x$taxon.name[i]
        }else{
        bestname <- data.frame(id= x$id[i], taxon.name=x$taxon.name[i], plant_name_id=NA, taxon_name=NA, taxon_rank= NA,
                               taxon_status= NA, accepted_plant_name_id= NA,
                               homotypic_synonym= NA,
                               match_type="No fuzzy match with similarity >0.75 found")}
        }
    }
    matches <- dplyr::bind_rows(matches, bestname) %>%
      dplyr:: select(id, plant_name_id,taxon.name, taxon_name, taxon_rank, taxon_status, taxon_authors,
                     accepted_plant_name_id, homotypic_synonym, match_type)
  }

  matches$match_similarity <- round(RecordLinkage::levenshteinSim(matches$taxon.name, matches$taxon_name),3)
  matches$match_edit_distance <- diag(utils::adist(matches$taxon.name, matches$taxon_name))
  matches <- matches %>% dplyr::select(-taxon.name)


  return(matches)

}




