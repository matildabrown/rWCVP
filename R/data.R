#' Example dataset for name matching
#'
#' A dataset containing 20 sampled Red List assessments for name-matching
#'
#' @format A data frame with 20 rows and 4 variables:
#' \describe{
#'   \item{assessmentId}{Red List identifier}
#'   \item{scientificName}{Taxon name.}
#'   \item{redlistCategory}{Red List threat category}
#'   \item{authority}{Taxon author/s.}
#'   }
#'
#'
#' @source Downloaded and sampled from \url{https://www.iucnredlist.org/}

"redlist_example"

#' Data for mapping WGSRPD geography to other levels
#'
#' A dataset containing the area (Level 3), #' region (Level 2),
#' continent (Level 1), country (political) and hemisphere category
#' for each Level 3 area. Country mapping follows:
#' Gallagher, R. V., Allen, S., Rivers, M. C., Allen, A. P., Butt, N., Keith, D., & Adams, V. M. (2020). Global shortfalls in extinction risk assessments for endemic flora. bioRxiv, 2020.2003.2012.984559. https://doi.org/10.1101/2020.03.12.984559
#'
#' @format A data frame with 370 rows and 7 variables:
#' \describe{
#'   \item{HEMISPHERE}{Northern, Southern or Equatorial (spanning the equator).}
#'   \item{LEVEL1_COD}{Continent code.}
#'   \item{LEVEL1_NAM}{Continent.}
#'   \item{LEVEL2_COD}{Region code.}
#'   \item{LEVEL2_NAM}{Region.}
#'   \item{COUNTRY}{Country (political; from Gallagher et al., 2020)}
#'   \item{LEVEL3_COD}{Area code.}
#'   \item{LEVEL3_NAM}{Area.}
#'   }
#'
#'
#' @source Downloaded from \url{https://github.com/tdwg/wgsrpd}

"wgsrpd_mapping"
