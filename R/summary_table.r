#' Generate a summary table from the WCVP
#'
#' @param taxon Character. Taxon to be included. Defaults to NULL (no taxonomic filter; all taxa).
#' @param rank Character. One of "species", "genus", "family", "order" or "higher", giving the rank of the value/s in \code{taxon}. Must be specified unless taxon is \code{NULL}.
#' @param area Character. One or many WGSPRD level 3 region codes. Defaults to \code{NULL} (global).
#' @param grouping.var Character; one of \code{"area_code_l3", "genus", "family","order"} or \code{"higher"} specifying how the summary should be arranged. Defaults to \code{area_code_l3}.
#' @param wcvp_names Pointer to the WCVP names dataset. Defaults to \code{NULL} (uses data from \code{WCVPdata}).
#' @param wcvp_distributions Pointer to the WCVP distributions dataset. Defaults to \code{NULL} (uses data from \code{WCVPdata}).
#' @param return Return the checklist as a \code{gt} table \code{"gt_table"} or \code{"data.frame"}? Defaults to \code{"data.frame"}.


#' @return Data.frame with filtered data, or a \code{gt} table
#'
#' @importFrom rlang .data
#' @import dplyr gt
#' @export
#'
#' @examples
#' ferns <- summary_table("Ferns", "higher", get_wgsrpd3_codes("New Zealand"), grouping.var="family")
#' summary_table_gt(ferns)
#'

summary_table <- function(taxon=NULL,
                          rank=c("species", "genus", "family","order","higher"),
                          area=NULL,
                          return = c("data.frame","gt_table"),
                          grouping.var = c("area_code_l3","genus","family","order","higher"),
                          wcvp_names=NULL,wcvp_distributions=NULL){


  rank <- match.arg(rank)
  return <- match.arg(return)
  grouping.var <- match.arg(grouping.var)

  if(is.null(wcvp_distributions)){
    wcvp_distributions <- rWCVPdata::wcvp_distributions
  }
  if(is.null(wcvp_names)){
    wcvp_names <- rWCVPdata::wcvp_names
  }

  df <- generate_checklist(taxon=taxon, rank=rank, area=area, wcvp_names = wcvp_names, wcvp_distributions = wcvp_distributions, synonyms = FALSE)
  input.area <- area

  if(!"in_geography" %in% colnames(df)) {
    df$in_geography <- 1
    df$area_endemic <- 1
  }

  if(grouping.var %in% c("order", "higher")){
    if(!grouping.var %in% colnames(df)) right_join(rWCVP::taxonomic_mapping, df, by="family")
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
    filter(.data$area_code_l3 %in% input.area) %>%
    group_by_at(grouping.var) %>%
    group_split()
  summ_split <- list()
  for(i in 1:length(df_split)){
    native <- nrow(unique(df_split[[i]] %>%
                            filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                   .data$taxon_rank == "Species",
                                   .data$introduced ==0 ) %>%
                            select(.data$taxon_name)))
    endemic <- nrow(unique(df_split[[i]] %>%
                             filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                    .data$taxon_rank == "Species",
                                    .data$endemic ==1 ) %>%
                             select(.data$taxon_name)))
    introduced <- nrow(unique(df_split[[i]] %>%
                                filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                       .data$taxon_rank == "Species",
                                       .data$introduced ==1 ) %>%
                                select(.data$taxon_name)))
    extinct <- nrow(unique(df_split[[i]] %>%
                             filter(.data$taxon_status %in% c("Accepted","Unplaced"),
                                    .data$taxon_rank == "Species",
                                    .data$extinct ==1 ) %>%
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
    res <- res %>%
   left_join(df %>% select(.data$continent, .data$region, .data$area_code_l3),by = "area_code_l3") %>%
   arrange(.data$continent, .data$region, .data$area_code_l3) %>%
   select(-.data$continent) %>%
   group_by(.data$region) %>%
   unique() %>%
   stats::na.omit()
  } else {
     res <- res %>% arrange(vars(grouping.var))
   }


x <- list(
  Taxon = taxon,
  Area = get_area_name(input.area),
  Grouping_variable = grouping.var,
  Total_number_of_species = as.numeric(summ_df[1,2]),
  Number_of_regionally_endemic_species = r.end.text,
  Summary = res %>% ungroup()
)

if(return=="data.frame"){
  return(x)} else {
    return(summary_table_gt(x))
  }

}


#' Render a summary table from [summary_table]
#'
#' @param x List.

#' @return [gt] table
#'
#' @import gt
#' @export
#'
#' @examples
#' ferns <- summary_table("Ferns", "higher", get_wgsrpd3_codes("New Zealand"), grouping.var="family")
#' summary_table_gt(ferns)
#'

summary_table_gt <- function(x){
  tab_title <- paste("<b>",x$Taxon, "of", x$Area, "</b>")
  if(!is.null(x$Number_of_regionally_endemic_species)){
    tab_sub <- paste0("Total number of species: ", as.numeric(x$Total_number_of_species), "<br>",
                                 "Number of regionally endemic species: ", as.numeric(x$Number_of_regionally_endemic_species))
  } else {
    tab_sub <- paste0("Total number of species: ", as.numeric(x$Total_number_of_species))
  }

  if(x$Grouping_variable =="area_code_l3") colnames(x$Summary)[colnames(x$Summary)=="area_code_l3"] <- "area"
  colnames(x$Summary) <- Toupper(colnames(x$Summary))

  x$Summary %>%
    gt() %>%
    tab_options(
      row_group.as_column= TRUE,
      stub_row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      data_row.padding = px(1),
      table.font.size = 12,
      table_body.hlines.color = "transparent",
      table.align = "left",
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
      locations = cells_body(),
      fn = function(x){
        ifelse(x == 0, "", x)
      }
    ) %>%
    tab_header(title=html(tab_title), subtitle=html(tab_sub))
}




