#' Extract WGSRPD Level 3 (area) codes.
#'
#' @param geography Character. The geography to convert into Level 3 codes. May be a WGSRPD area (Level 3), region (Level 2) or continent (Level 1), country (political) or hemisphere ("Northern Hemisphere", "Southern Hemisphere" or "Equatorial")
#' @param include_equatorial Logical. Include Level 3 areas that span the equator? Defaults to `NULL`, which generates a message and includes these areas. Ignored if geography is not a hemisphere.
#' @details Country mapping follows Gallagher et al. (2020). Importantly, this means that some overseas territories are not considered part of the country in this system, e.g. the Canary Islands are designated as their own Level 3 area, rather than part of Spain in this mapping. Where this is ambiguous, the mapping can be explored using \code{View(wgsrpd_mapping)}.
#'
#' Gallagher, R. V., Allen, S., Rivers, M. C., Allen, A. P., Butt, N., Keith, D., & Adams, V. M. (2020). Global shortfalls in extinction risk assessments for endemic flora. bioRxiv, 2020.2003.2012.984559. https://doi.org/10.1101/2020.03.12.984559
#' @return Character with area codes (Level 3) that fall within the geography.
#'
#' @import cli
#' @export
#'
#' @examples
#' get_wgsrpd3_codes("Brazil")
#'
get_wgsrpd3_codes <- function(geography, include_equatorial = NULL) {
  wgsrpd_mapping <- rWCVP::wgsrpd_mapping

  levelnames <- c(
    LEVEL3_NAM = "Area (Level 3)",
    COUNTRY = "Country (Gallagher)",
    LEVEL2_NAM = "Region (Level 2)",
    LEVEL1_NAM = "Continent (Level 1)",
    HEMISPHERE = "Hemisphere level"
  )
  if (length(which(wgsrpd_mapping == geography |
    wgsrpd_mapping == toupper(geography))) == 0) {
    cli_abort("No matches to area, country, region, continent or hemisphere found.")
  }

  matchlevel <- levelnames[which(names(levelnames) %in% colnames(wgsrpd_mapping[which(wgsrpd_mapping == geography |
    wgsrpd_mapping == toupper(geography), arr.ind = TRUE)[, 2] %>%
    unique()]))]

  if (length(matchlevel) == 0) {
    cli_abort("No matches to area, country, region, continent or hemisphere found.")
  } else {
    cli_alert_info("Matches to input geography found at {matchlevel}")
  }

  if (length(grep("hemisphere", geography, ignore.case = TRUE)) != 0) {
    if (is.null(include_equatorial)) {
      cli_alert_info("Including WGSRPD areas that span the equator. To turn this off, use {.var include_equatorial = FALSE}")
      include_equatorial <- TRUE
    }
    if (include_equatorial == TRUE) {
      geography <- c(geography, "Equatorial")
    }
  }

  wgsrpd_mapping %>%
    dplyr::filter(.data$LEVEL1_NAM %in% geography |
      .data$LEVEL1_NAM %in% toupper(geography) |
      .data$LEVEL2_NAM %in% geography |
      .data$LEVEL3_NAM %in% geography |
      .data$COUNTRY %in% geography |
      .data$HEMISPHERE %in% geography) %>%
    dplyr::pull(.data$LEVEL3_COD) %>%
    unique() %>%
    return()
}


#' Get area description from vector of area codes
#'
#' @param area_codes Character vector containing the set of codes to be mapped to a name.
#'
#' @return Character. Either a vector of length one, with a name for the set of
#' Level 3 areas, or (if no name exists for that set of areas) the input vector of codes.
#' @details Usually used as an inverse function for \code{get_wgsrpd3_codes}. Useful for condensing sets of codes for e.g. file names, plotting and table formatting.
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
#'
#' @examples get_area_name(get_wgsrpd3_codes("Brazil"))
get_area_name <- function(area_codes) {
  wgsrpd_mapping <- rWCVP::wgsrpd_mapping
  global <- sort(unique(wgsrpd_mapping$LEVEL3_COD))


  if (setequal(area_codes, hemispheres$north_with_eq)) {
    return("Northern Hemisphere (incl. equatorial Level 3 areas)")
  }
  if (setequal(area_codes, hemispheres$north_without_eq)) {
    return("Northern Hemisphere (excl. equatorial Level 3 areas)")
  }
  if (setequal(area_codes, hemispheres$south_with_eq)) {
    return("Southern Hemisphere (incl. equatorial Level 3 areas)")
  }
  if (setequal(area_codes, hemispheres$south_without_eq)) {
    return("Southern Hemisphere (excl. equatorial Level 3 areas)")
  }
  if (setequal(area_codes, global)) {
    return("Global")
  }

  level_names <- c(
    LEVEL3_NAM = "Area (Level 3)",
    COUNTRY = "Country (Gallagher)",
    LEVEL2_NAM = "Region (Level 2)",
    LEVEL1_NAM = "Continent (Level 1)",
    HEMISPHERE = "Hemisphere"
  )

  area_mapping <- filter(wgsrpd_mapping, .data$LEVEL3_COD %in% area_codes)

  levels_covered <- area_mapping %>%
    summarise(across(everything(), n_distinct)) %>%
    pivot_longer(everything(), names_to = "level", values_to = "n") %>%
    filter(n == 1)

  best_level <- intersect(names(level_names), levels_covered$level)[1]
  if (is.na(best_level)) {
    cli_alert_info("No higher level name found. Returning original vector of area codes.")
    return(area_codes)
  }

  best_level_value <- unique(area_mapping[[best_level]])

  all_codes <- wgsrpd_mapping %>%
    filter(if_any(all_of(best_level), ~ .x %in% best_level_value)) %>%
    arrange(.data$LEVEL3_COD) %>%
    pull(.data$LEVEL3_COD)


  if (setequal(all_codes, area_codes)) {
    return(best_level_value)
  } else {
    cli_alert_info("No higher level name found. Returning original area codes as string.")
    return(paste(area_codes, collapse = "-"))
  }
}
