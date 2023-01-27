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
#' for each Level 3 area. Country mapping follows Gallagher et al. (2020).
#'
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
#' @source Modified from data available at \url{https://github.com/tdwg/wgsrpd}

"wgsrpd_mapping"

#' Data for mapping plant family to order or higher classification
#'
#' A dataset containing the higher classification (Angiosperms, Gymnosperms, Ferns and Lycophytes) and Order for each family in the WCVP.
#'
#'
#' @format A data frame with 457 rows and 3 variables: \code{family, order} and \code{higher_classification}
#'
#'
#' @source Fern and lycophyte taxonomy from PPG I (2016; \doi{10.1111/jse.12229}). Angiosperm taxonomy from APG IV (2016; \doi{10.1111/boj.12385}). Gymnosperm taxonomy from Forest et al (2018; \doi{10.1038/s41598-018-24365-4})
#'
#'
"taxonomic_mapping"

#' Biodiversity Information Standards (TDWG) World Geographical Scheme for
#' Recording Plant Distributions (WGSRPD)
#'
#' Spatial data for WGSRPD Level 3, for plotting maps
#'
#' @format An 'sf' object with 20 rows and 4 variables:
#' \describe{
#'   \item{LEVEL3_NAM}{Region name}
#'   \item{LEVEL3_COD}{Region code}
#'   \item{LEVEL2_COD}{Level 2 code}
#'   \item{LEVEL1_COD}{Level 1 code (continent)}
#'   \item{geometry}{sf geometry}
#'   \item{fillcol}{Used for mapping.}
#'   }
#'
#' @source  \url{https://github.com/tdwg/wgsrpd/tree/master/level3}

"wgsrpd3"
