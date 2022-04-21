#' Generate spatial distribution objects for species, genera or families
#'
#' @param taxon Character. The taxon to be mapped.
#' @param rank Character. The rank of \code{taxon}. Note: if multiple taxa are
#' to be mapped, they must be of the same rank.
#' @param native Logical. Include native range? Defaults to TRUE.
#' @param introduced Logical. Include introduced range? Defaults to TRUE.
#' @param extinct Logical. Include extinct range? Defaults to TRUE.
#' @param location_doubtful Logical. Include occurrences that are thought to be
#'     doubtful? Defaults to TRUE.
#' @param local_wcvp Logical. If FALSE (the default), use data from \code{rWCVPdata}.
#' If TRUE, use a local copy of the data (useful if rWCVPdata is not the latest
#' version of the checklist).
#' @param wcvp_names Pointer to the WCVP names dataset. Ignored if \code{local.wcvp = FALSE}. Defaults to NULL.
#' @param wcvp_distributions Pointer to the WCVP distributions dataset. Ignored if \code{local.wcvp = FALSE}. Defaults to NULL.
#'
#'
#' @return sf data.frame containing the range polygon/s of the taxon.
#'
#' @export
#'
#' @examples
#' r <- get_distribution("Callitris", rank="genus")
#' p <- plot_distribution(r)
#' p[[1]]
#' p[[2]]
get_distribution <- function(taxon, rank=c("species", "genus", "family"), native = TRUE, introduced = TRUE,
                              extinct = TRUE, location_doubtful = TRUE,
                              local_wcvp=FALSE, wcvp_names=NULL,
                              wcvp_distributions=NULL){

  LEVEL3_COD <- df <- doubt <- intro <- occurrence_type <- plant_name_id <- NULL
  species <- taxon_authors <- taxon_rank <- taxon_status <- NULL

  rank <- match.arg(rank)

  shown <- native
  showi <- introduced
  showe <- extinct
  showl <- location_doubtful
  requireNamespace("sf")
  suppressMessages(sf::sf_use_s2(FALSE))

  if(local_wcvp == FALSE){
    wcvp_distributions <- rWCVPdata::wcvp_distributions
    wcvp_names <- rWCVPdata::wcvp_names
  } else {
    if (is.null(wcvp_names)) stop("Pointer to wcvp_names missing.")
    if (is.null(wcvp_distributions)) stop("Pointer to wcvp_distributions missing.")
  }

  if(length(taxon)>1) stop("'taxon' argument must be a single name")

  df <- dplyr::right_join(wcvp_names %>% dplyr::select(plant_name_id, taxon_rank, taxon_status,family, genus,species, taxon_name, taxon_authors),
    wcvp_distributions)

  occurrence_type <- taxon_name <-  area_code_l3 <- native <-  introduced <-  extinct <-  location_doubtful <- genus <- family <- LEVEL3_COD <-  NULL
  if(rank=="species"){
    df <- df %>%
      dplyr::filter(taxon_name %in% taxon) %>%
      dplyr::select(area_code_l3, introduced, extinct, location_doubtful)
  }
  if(rank=="genus"){
    df <- df %>%
      dplyr::filter(genus %in% taxon)%>%
      dplyr::select(area_code_l3, introduced, extinct, location_doubtful)
  }
  if(rank=="family"){
    df <- df %>%
      dplyr::filter(family %in% taxon)%>%
      dplyr::select(area_code_l3, introduced, extinct, location_doubtful)
  }

  if(nrow(df)==0) stop("No distribution for that taxon. Are the rank and spelling both correct?")

  intro <-  df[which(df$introduced==1),"area_code_l3"]
  extinct <-  df[which(df$extinct==1),"area_code_l3"]
  doubt <-  df[which(df$location_doubtful==1),"area_code_l3"]

  range <- rWCVPdata::wgsprd3[which(rWCVPdata::wgsprd3$LEVEL3_COD %in% df$area_code_l3),]


  range$occurrence_type [which(range$LEVEL3_COD %in% df$area_code_l3)] <- "native"
  range$occurrence_type [which(range$LEVEL3_COD %in% intro)] <- "introduced"
  range$occurrence_type [which(range$LEVEL3_COD %in% extinct)] <- "extinct"
  range$occurrence_type [which(range$LEVEL3_COD %in% doubt)] <- "location doubtful"

  if(shown==FALSE) range <- range %>% dplyr::filter(LEVEL3_COD %notin% df$area_code_l3)
  if(showi==FALSE) range <- range %>% dplyr::filter(LEVEL3_COD %notin% intro)
  if(showe==FALSE) range <- range %>% dplyr::filter(LEVEL3_COD %notin% extinct)
  if(showl==FALSE) range <- range %>% dplyr::filter(LEVEL3_COD %notin% doubt)

  return(range)

}


