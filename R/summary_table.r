#' Generate a summary table from the WCVP
#'
#' @param taxon Character. Taxon to be included. Defaults to NULL (no taxonomic filter; all taxa).
#' @param rank Character. One of "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param grouping.var Character; one of \code{"area_code_l3", "genus", "family","order"} or \code{"higher"} specifying how the summary should be arranged. Defaults to \code{area_code_l3}.
#' @param wcvp_names A data frame of taxonomic names from WCVP version 7 or later.
#'   If `NULL`, names will be loaded from [rWCVPdata::wcvp_names].
#' @param wcvp_distributions A data frame of distributions from WCVP version 7 or later.
#'   If `NULL`, distributions will be loaded from [rWCVPdata::wcvp_names].
#' @details Valid values for rank 'higher' are 'Angiosperms', 'Gymnosperms', 'Ferns' and 'Lycophytes'.Note that grouping variable (if taxonomic) should be of a lower level than \code{taxon} and \code{rank} to produce a meaningful summary (i.e., it does not make sense to group a genus by genus, family or higher classification).
#' Additionally, if the grouping variable is taxonomic then species occurrences are aggregated across the input area. This means that if a species is native to any of the input area (even if it is introduced or extinct in other parts) it is counted as 'Native'. Similarly, introduced occurrences take precedence over extinct occurrences. Note that in this type of summary table, 'Endemic' means endemic to the input area, not necessarily to a single WGSRPD Level 3 Area within the input area.
#' @return Data.frame with filtered data, or a \code{gt} table
#'
#' @importFrom rlang .data
#' @import dplyr gt cli
#' @export
#'
#' @examples
#' ferns <- summary_table("Ferns", "higher", get_wgsrpd3_codes("New Zealand"), grouping.var="family")
#' summary_table_gt(ferns)
#'
summary_table <- function(taxon=NULL,
                          rank=c("species", "genus", "family","order","higher"), #species makes no sense
                          area=NULL,
                          grouping.var = c("area_code_l3","genus","family","order","higher"),
                          wcvp_names=NULL,wcvp_distributions=NULL){

  rank <- match.arg(rank)
  grouping.var <- match.arg(grouping.var)

  if(rank == "order" &
     !taxon %in% rWCVP::taxonomic_mapping$order) cli_abort(
       "Taxon not found. Possible values for this taxonomic rank can be viewed using `unique(taxonomic_mapping$order)`")
  if(rank == "higher" &
     !taxon %in% rWCVP::taxonomic_mapping$higher) cli_abort(
       "Taxon not found. Possible values for this taxonomic rank are: 'Angiosperms', 'Gymnosperms', 'Ferns' and 'Lycophytes'")

  if (is.null(wcvp_names) | is.null(wcvp_distributions)) {
    .wcvp_available()
    .wcvp_fresh()
  }

  if (is.null(wcvp_distributions)) {
    wcvp_distributions <- rWCVPdata::wcvp_distributions
  }
  if (is.null(wcvp_names)) {
    wcvp_names <- rWCVPdata::wcvp_names
  }


  if (is.null(area)) message("No area specified. Generating global summary.")
  if (is.null(taxon)) message("No taxon specified. Generating summary of all species.")

  df <- suppressMessages(generate_checklist(taxon=taxon, rank=rank, area=area, wcvp_names = wcvp_names, wcvp_distributions = wcvp_distributions))
  input.area <- area


  if(!"in_geography" %in% colnames(df)) {
    df$in_geography <- 1
    df$area_endemic <- 1
  }

  df <- df %>% filter(.data$taxon_rank=="Species",
                      .data$taxon_status %in% c("Accepted", "Unplaced"),
                      .data$in_geography == TRUE)

  if(grouping.var %in% c("order", "higher")){
    if(!grouping.var %in% colnames(df)) right_join(rWCVP::taxonomic_mapping, df, by="family")
  }

  if(grouping.var %in% c("genus", "family", "order", "higher") & length(input.area>1)){
    cli_alert_info("Aggregating occurrence types across input areas - see {.fun ?summary_table} for details.")
    occ_type_levels <- c("native", "introduced", "extinct", "location_doubful")
    df <- df %>% left_join(df %>%
                                  mutate(occurrence_type_num = as.numeric(factor(.data$occurrence_type, levels=occ_type_levels))) %>%
                                  group_by(.data$plant_name_id) %>%
                                  summarise(reg_occ_type = min(.data$occurrence_type_num)) %>%
                                  mutate(reg_occ_type = occ_type_levels[.data$reg_occ_type])
                                , by="plant_name_id") %>%
      mutate(occurrence_type = .data$reg_occ_type,
             endemic = .data$area_endemic) %>%
      select(-c(.data$continent_code_l1, .data$continent, .data$region_code_l2,
                .data$region, .data$area_code_l3, .data$area, .data$introduced,
                .data$extinct, .data$location_doubtful, .data$reg_occ_type)) %>%
      unique()

  }


  summ_df <- data.frame(area_code_l3="Region",
                        cat=c("Total", "Endemic (to region)"),
                        n.sp= c(nrow(unique(df %>%
                                              filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                                     .data$taxon_rank == "Species") %>%
                                              select(.data$taxon_name))),
                                nrow(unique(df %>%
                                              filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                                     .data$taxon_rank == "Species",
                                                     .data$area_endemic == 1) %>%
                                              select(.data$taxon_name))))) %>%
    tidyr::pivot_wider(names_from=.data$cat, values_from = .data$n.sp)



  if(is.null(input.area)) {
    r.end.text <- NULL
    input.area <- unique(df$area_code_l3)

  } else {
    r.end.text <- as.numeric(summ_df[1,3])
  }

  df_split <- df %>%
    group_by_at(grouping.var) %>%
    group_split()
  summ_split <- list()
  for(i in 1:length(df_split)){
    native <- nrow(unique(df_split[[i]] %>%
                            filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                   .data$taxon_rank == "Species",
                                   .data$occurrence_type=="native" ) %>%
                            select(.data$taxon_name)))
    endemic <- nrow(unique(df_split[[i]] %>%
                             filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                    .data$taxon_rank == "Species",
                                    .data$endemic ==1 ) %>%
                             select(.data$taxon_name)))
    introduced <- nrow(unique(df_split[[i]] %>%
                                filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                       .data$taxon_rank == "Species",
                                       .data$occurrence_type=="introduced" ) %>%
                                select((.data$taxon_name))))
    extinct <- nrow(unique(df_split[[i]] %>%
                             filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                    .data$taxon_rank == "Species",
                                    .data$occurrence_type=="extinct" ) %>%
                             select(.data$taxon_name)))
    total <- nrow(unique(df_split[[i]] %>%
                           filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                  .data$taxon_rank == "Species") %>%
                           select(.data$taxon_name)))

    summ_split[[i]] <- data.frame(group = df_split[[i]][1,grouping.var],
                                  cat=c("Native",
                                        "Endemic",
                                        "Introduced",
                                        "Extinct",
                                        "Total"),
                                  n.sp=c(native,
                                         endemic,
                                         introduced,
                                         extinct,
                                         total))
  }

  summ <- do.call(rbind, summ_split)



res <- summ %>%
  tidyr::pivot_wider(names_from = .data$cat, values_from = .data$n.sp)



  if(grouping.var== "area_code_l3") {
    zeroareas <- input.area[!input.area %in% res$area_code_l3]
    data_blanks <- data.frame(area_code_l3 = zeroareas,
                              Native=rep(0, times=length(zeroareas)),
                              Endemic=rep(0, times=length(zeroareas)),
                              Introduced=rep(0, times=length(zeroareas)),
                              Extinct=rep(0, times=length(zeroareas)),
                              Total=rep(0, times=length(zeroareas))
                              )
    res <- res %>% bind_rows(data_blanks) %>%
   left_join(rWCVP::wgsrpd_mapping %>% select(.data$LEVEL1_NAM, .data$LEVEL2_NAM, .data$LEVEL3_COD),by = c("area_code_l3"="LEVEL3_COD")) %>%
   arrange(.data$LEVEL1_NAM, .data$LEVEL2_NAM, .data$area_code_l3) %>%
   rename(region = .data$LEVEL2_NAM) %>%
   select(-.data$LEVEL1_NAM) %>%
   group_by(.data$region) %>%
   unique() %>%
   stats::na.omit()
  } else {
     res <- res %>% arrange(vars(grouping.var))
   }

if(is.null(area)){
  output.area <- "the world"
} else {
  output.area <- get_area_name(input.area)
  }


x <- list(
  Taxon = taxon,
  Area = output.area,
  Grouping_variable = grouping.var,
  Total_number_of_species = as.numeric(summ_df[1,2]),
  Number_of_regionally_endemic_species = r.end.text,
  Summary = res %>% ungroup()
)




  return(x)

}


#' Render a summary table from [summary_table]
#'
#' @param x List.

#' @return [gt] table
#'
#' @import gt
#' @importFrom rlang .data
#' @importFrom stringr str_to_sentence
#' @export
#'
#' @examples
#' ferns <- summary_table("Ferns", "higher", get_wgsrpd3_codes("New Zealand"), grouping.var="family")
#' summary_table_gt(ferns)
#'

summary_table_gt <- function(x){
  if(is.null(x$Taxon)) x$Taxon <- "Plants"
  tab_title <- paste("<b>",x$Taxon, "of", x$Area, "</b>")


  if(!is.null(x$Number_of_regionally_endemic_species)){
    tab_sub <- paste0("Total number of species: ", as.numeric(x$Total_number_of_species), "<br>",
                                 "Number of regionally endemic species: ", as.numeric(x$Number_of_regionally_endemic_species))
  } else {
    tab_sub <- paste0("Total number of species: ", as.numeric(x$Total_number_of_species))
  }

  if(x$Grouping_variable =="area_code_l3") colnames(x$Summary)[colnames(x$Summary)=="area_code_l3"] <- "area"
  colnames(x$Summary) <- str_to_sentence(colnames(x$Summary))

  if(x$Grouping_variable =="area_code_l3") {
    x$Summary %>%
      dplyr::group_by(.data$Region) %>%
      gt() %>%
      tab_options(
        row_group.as_column= TRUE,
        stub_row_group.font.weight = "bold",
        column_labels.font.weight = "bold",
        data_row.padding = px(1),
        table.font.size = 12,
        table_body.hlines.color = "transparent",
        heading.subtitle.font.size = 12,
        heading.align = "left") %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_row_groups()
      ) %>%
      tab_style(
        style=cell_text(weight="bold"),
        locations = cells_body(
          columns= .data$Total
        )
      ) %>%
      text_transform(
        locations = cells_body(
          columns = c(.data$Native,.data$Endemic,.data$Introduced,.data$Extinct),
        ),
        fn = function(x){
          ifelse(x == 0, "", x)
        }
      ) %>%
      tab_header(title=html(tab_title), subtitle=html(tab_sub))
  } else {
    x$Summary %>%
      gt() %>%
      tab_options(
        row_group.as_column= TRUE,
        stub_row_group.font.weight = "bold",
        column_labels.font.weight = "bold",
        data_row.padding = px(1),
        table.font.size = 12,
        table_body.hlines.color = "transparent",
        heading.subtitle.font.size = 12,
        heading.align = "left") %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_row_groups()
      ) %>%
      tab_style(
        style=cell_text(weight="bold"),
        locations = cells_body(
          columns= .data$Total
        )
      ) %>%
      text_transform(
        locations = cells_body(
          columns = c(.data$Native,.data$Endemic,.data$Introduced,.data$Extinct),
        ),
        fn = function(x){
          ifelse(x == 0, "", x)
        }
      ) %>%
      tab_header(title=html(tab_title), subtitle=html(tab_sub))
    }


}




