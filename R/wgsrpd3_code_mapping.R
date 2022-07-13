#' Extract WGSRPD Level 3 (area) codes.
#'
#' @param geography Character. The geography to convert into Level 3 codes. May be a WGSRPD area (Level 3), region (Level 2) or continent (Level 1), country (political) or hemisphere ("Northern Hemisphere", "Southern Hemisphere" or "Equatorial")
#' @param include.equatorial Logical. Include Level 3 areas that span the equator? Defaults to Null, which generates a message and includes these areas. Ignored if geography is not a hemisphere.
#' @details Country mapping follows Gallagher et al. (2020). Importantly, this means that some overseas territories are not considered part of the Country in this system, e.g. the Canary Islands are designated as a Country (Gallagher), rather than part of Spain in this mapping. Where this is ambiguous, the mapping can be explored using \code{View(wgsrpd_mapping)}.
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
get_wgsrpd3_codes <- function(geography, include.equatorial=NULL) {

  wgsrpd_mapping <- rWCVP::wgsrpd_mapping

  levelnames <- c(LEVEL3_NAM = "Area (Level 3)",
                  COUNTRY = "Country (Gallagher)",
                  LEVEL2_NAM = "Region (Level 2)",
                  LEVEL1_NAM = "Continent (Level 1)",
                  HEMISPHERE = "Hemisphere level"
                  )
  if(length(which(wgsrpd_mapping == geography |
                  wgsrpd_mapping == toupper(geography))
            )==0)  cli_abort("No matches to area, country, region, continent or hemisphere found.")

  matchlevel <- levelnames[which(names(levelnames) %in% colnames(wgsrpd_mapping[which(wgsrpd_mapping==geography |
                                                                                      wgsrpd_mapping == toupper(geography), arr.ind = T)[,2] %>%
                                                                                  unique()]))]

  if (length(matchlevel)==0) {
    cli_abort("No matches to area, country, region, continent or hemisphere found.")
  } else {
    cli_alert_info("Matches to input geography found at {matchlevel}")
  }

  if(length(grep("hemisphere", geography, ignore.case = TRUE))!=0) {
    if(is.null(include.equatorial)){
      cli_alert_info("Including WGSRPD areas that span the equator. To turn this off, use {.var include.equatorial = FALSE}")
      include.equatorial <- TRUE
    }
    if(include.equatorial==TRUE) {
      geography <- c(geography, "Equatorial")
    }

  }

  wgsrpd_mapping %>%
  dplyr::filter(.data$LEVEL1_NAM %in% geography |
                  .data$LEVEL1_NAM %in% toupper(geography) |
                  .data$LEVEL2_NAM %in% geography |
                  .data$LEVEL3_NAM %in% geography |
                  .data$HEMISPHERE %in% geography) %>%
  dplyr::pull(.data$LEVEL3_COD) %>%
  return()

}



#' Get area description from vector of area codes
#'
#' @param area.codes Character vector containing the set of codes to be mapped to a name.
#'
#' @return Character. Either a vector of length one, with a name for the set of
#' Level 3 areas, or (if no name exists for that set of areas) the input vector of codes.
#' @details Usually used as an inverse function for \code{get_wgsrpd3_codes}. Useful for condensing sets of codes for e.g. file names, plotting and table formatting.
#' @export
#'
#' @examples get_area_name(get_wgsrpd3_codes("Brazil"))
get_area_name <- function(area.codes){


  wgsrpd_mapping <- rWCVP::wgsrpd_mapping
  global <- unique(wgsrpd_mapping$LEVEL3_COD[order(wgsrpd_mapping$LEVEL3_COD)])
  nhi <-   c("ABT", "AFG", "ALA", "ALB", "ALG", "ALT", "ALU", "AMU", "AND",
             "ARI", "ARK", "ARU", "ASK", "ASS", "AUT", "AZO", "BAH", "BAL",
             "BAN", "BEN", "BER", "BGM", "BKN", "BLR", "BLT", "BLZ", "BOR",
             "BRC", "BRY", "BUL", "BZN", "CAF", "CAL", "CAY", "CBD", "CHA",
             "CHC", "CHH", "CHI", "CHM", "CHN", "CHQ", "CHS", "CHT", "CHX",
             "CLM", "CMN", "CNT", "CNY", "COL", "CON", "COR", "COS", "CPI",
             "CRL", "CTA", "CUB", "CVI", "CYP", "CZE", "DEL", "DEN", "DJI",
             "DOM", "EAI", "ECU", "EGY", "EHM", "ELS", "EQG", "ERI", "ETH",
             "FIN", "FLA", "FOR", "FRA", "FRG", "GAB", "GAL", "GAM", "GEO",
             "GER", "GGI", "GHA", "GIL", "GNB", "GNL", "GRB", "GRC", "GST",
             "GUA", "GUI", "GUY", "HAI", "HAW", "HBI", "HON", "HUN", "ICE",
             "IDA", "ILL", "IND", "INI", "IOW", "IRE", "IRK", "IRN", "IRQ",
             "ITA", "IVO", "JAM", "JAP", "KAM", "KAN", "KAZ", "KEN", "KGZ",
             "KHA", "KOR", "KRA", "KRI", "KRY", "KTY", "KUR", "KUW", "KZN",
             "LAB", "LAO", "LBR", "LBS", "LBY", "LDV", "LEE", "LIN", "LOU",
             "MAG", "MAI", "MAN", "MAS", "MCS", "MDR", "MDV", "MIC", "MIN",
             "MLI", "MLY", "MNT", "MOL", "MON", "MOR", "MRN", "MRS", "MRY",
             "MSI", "MSO", "MTN", "MXC", "MXE", "MXG", "MXI", "MXN", "MXS",
             "MXT", "MYA", "NBR", "NCA", "NCB", "NCS", "NDA", "NEB", "NEP",
             "NET", "NEV", "NFL", "NGA", "NGR", "NIC", "NLA", "NNS", "NOR",
             "NSC", "NUN", "NWH", "NWJ", "NWM", "NWT", "NWY", "OGA", "OHI",
             "OKL", "OMA", "ONT", "ORE", "PAK", "PAL", "PAN", "PEI", "PEN",
             "PHI", "POL", "POR", "PRM", "PUE", "QUE", "RHO", "ROM", "RUC",
             "RUE", "RUN", "RUS", "RUW", "SAK", "SAR", "SAS", "SAU", "SCA",
             "SCS", "SDA", "SEL", "SEN", "SIC", "SIE", "SIN", "SOC", "SOM",
             "SPA", "SPA", "SRL", "SUD", "SUL", "SUM", "SUR", "SVA", "SWC",
             "SWE", "SWI", "TAI", "TCI", "TCS", "TEN", "TEX", "THA", "TKM",
             "TOG", "TRT", "TUE", "TUN", "TUR", "TVA", "TZK", "UGA", "UKR",
             "UTA", "UZB", "VEN", "VER", "VIE", "VNA", "VRG", "WAK", "WAS",
             "WDC", "WHM", "WIN", "WIS", "WSA", "WSB", "WVA", "WYO", "YAK",
             "YEM", "YUG", "YUK", "ZAI")
  nhe <-  c("ABT", "AFG", "ALA", "ALB", "ALG", "ALT", "ALU", "AMU", "AND",
            "ARI", "ARK", "ARU", "ASK", "ASS", "AUT", "AZO", "BAH", "BAL",
            "BAN", "BEN", "BER", "BGM", "BKN", "BLR", "BLT", "BLZ", "BRC",
            "BRY", "BUL", "CAF", "CAL", "CAY", "CBD", "CHA", "CHC", "CHH",
            "CHI", "CHM", "CHN", "CHQ", "CHS", "CHT", "CHX", "CMN", "CNT",
            "CNY", "COL", "COR", "COS", "CPI", "CRL", "CTA", "CUB", "CVI",
            "CYP", "CZE", "DEL", "DEN", "DJI", "DOM", "EAI", "EGY", "EHM",
            "ELS", "EQG", "ERI", "ETH", "FIN", "FLA", "FOR", "FRA", "FRG",
            "GAM", "GEO", "GER", "GHA", "GNB", "GNL", "GRB", "GRC", "GST",
            "GUA", "GUI", "GUY", "HAI", "HAW", "HBI", "HON", "HUN", "ICE",
            "IDA", "ILL", "IND", "INI", "IOW", "IRE", "IRK", "IRN", "IRQ",
            "ITA", "IVO", "JAM", "JAP", "KAM", "KAN", "KAZ", "KGZ", "KHA",
            "KOR", "KRA", "KRI", "KRY", "KTY", "KUR", "KUW", "KZN", "LAB",
            "LAO", "LBR", "LBS", "LBY", "LDV", "LEE", "LOU", "MAG", "MAI",
            "MAN", "MAS", "MCS", "MDR", "MIC", "MIN", "MLI", "MLY", "MNT",
            "MON", "MOR", "MRN", "MRS", "MRY", "MSI", "MSO", "MTN", "MXC",
            "MXE", "MXG", "MXI", "MXN", "MXS", "MXT", "MYA", "NBR", "NCA",
            "NCB", "NCS", "NDA", "NEB", "NEP", "NET", "NEV", "NFL", "NGA",
            "NGR", "NIC", "NLA", "NNS", "NOR", "NSC", "NUN", "NWH", "NWJ",
            "NWM", "NWT", "NWY", "OGA", "OHI", "OKL", "OMA", "ONT", "ORE",
            "PAK", "PAL", "PAN", "PEI", "PEN", "PHI", "POL", "POR", "PRM",
            "PUE", "QUE", "RHO", "ROM", "RUC", "RUE", "RUN", "RUS", "RUW",
            "SAK", "SAR", "SAS", "SAU", "SCA", "SCS", "SDA", "SEL", "SEN",
            "SIC", "SIE", "SIN", "SOC", "SPA", "SPA", "SRL", "SUD", "SUR",
            "SVA", "SWC", "SWE", "SWI", "TAI", "TCI", "TCS", "TEN", "TEX",
            "THA", "TKM", "TOG", "TRT", "TUE", "TUN", "TUR", "TVA", "TZK",
            "UKR", "UTA", "UZB", "VEN", "VER", "VIE", "VNA", "VRG", "WAK",
            "WAS", "WDC", "WHM", "WIN", "WIS", "WSA", "WSB", "WVA", "WYO",
            "YAK", "YEM", "YUG", "YUK")
  shi <-  c("AGE", "AGS", "AGW", "ALD", "ANG", "ANT", "ASC", "ASP", "ATP",
            "BIS", "BOL", "BOR", "BOT", "BOU", "BUR", "BZC", "BZE", "BZL",
            "BZN", "BZS", "CAB", "CGS", "CKI", "CLC", "CLM", "CLN", "CLS",
            "COM", "CON", "COO", "CPP", "CPV", "CRZ", "CTM", "DSV", "EAS",
            "ECU", "FAL", "FIJ", "GAB", "GAL", "GGI", "GIL", "HMD", "JAW",
            "JNF", "KEG", "KEN", "KER", "LES", "LIN", "LSI", "MAQ", "MAU",
            "MCI", "MDG", "MDV", "MLW", "MOL", "MOZ", "MPE", "MRQ", "NAM",
            "NAT", "NFK", "NRU", "NSW", "NTA", "NUE", "NWC", "NWG", "NZN",
            "NZS", "OFS", "PAR", "PER", "PHX", "PIT", "QLD", "REU", "ROD",
            "RWA", "SAM", "SCI", "SCZ", "SEY", "SGE", "SOA", "SOL", "SOM",
            "SSA", "STH", "SUL", "SUM", "SWZ", "TAN", "TAS", "TDC", "TOK",
            "TON", "TUA", "TUB", "TUV", "TVL", "UGA", "URU", "VAN", "VIC",
            "WAL", "WAU", "XMS", "ZAI", "ZAM", "ZIM")
  she <-  c("AGE", "AGS", "AGW", "ALD", "ANG", "ANT", "ASC", "ASP", "ATP",
            "BIS", "BOL", "BOT", "BOU", "BUR", "BZC", "BZE", "BZL", "BZS",
            "CAB", "CGS", "CKI", "CLC", "CLN", "CLS", "COM", "COO", "CPP",
            "CPV", "CRZ", "CTM", "DSV", "EAS", "FAL", "FIJ", "HMD", "JAW",
            "JNF", "KEG", "KER", "LES", "LSI", "MAQ", "MAU", "MCI", "MDG",
            "MLW", "MOZ", "MPE", "MRQ", "NAM", "NAT", "NFK", "NRU", "NSW",
            "NTA", "NUE", "NWC", "NWG", "NZN", "NZS", "OFS", "PAR", "PER",
            "PHX", "PIT", "QLD", "REU", "ROD", "RWA", "SAM", "SCI", "SCZ",
            "SEY", "SGE", "SOA", "SOL", "SSA", "STH", "SWZ", "TAN", "TAS",
            "TDC", "TOK", "TON", "TUA", "TUB", "TUV", "TVL", "URU", "VAN",
            "VIC", "WAL", "WAU", "XMS", "ZAM", "ZIM")

 if(identical(area.codes[order(area.codes)],nhi)) return( "Northern Hemisphere (incl. equatorial Level 3 areas)")
 if(identical(area.codes[order(area.codes)],nhe)) return( "Northern Hemisphere (excl. equatorial Level 3 areas)")
 if(identical(area.codes[order(area.codes)],shi)) return( "Southern Hemisphere (incl. equatorial Level 3 areas)")
 if(identical(area.codes[order(area.codes)],she)) return( "Southern Hemisphere (excl. equatorial Level 3 areas)")
 if(identical(area.codes[order(area.codes)],global)) return( "Global")

  levelnames <- c(LEVEL3_NAM = "Area (Level 3)",
                  COUNTRY = "Country (Gallagher)",
                  LEVEL2_NAM = "Region (Level 2)",
                  LEVEL1_NAM = "Continent (Level 1)",
                  HEMISPHERE = "Hemisphere"
  )

  levelvals <- wgsrpd_mapping %>%
    dplyr::filter(.data$LEVEL3_COD %in% area.codes) %>%
    dplyr::summarise(dplyr::across(1:8, function(x){length(unique(x))})) %>%
    t() %>%
    data.frame() %>%
    purrr::set_names("n") %>%
    dplyr::filter(n==1)

  bestlevel <- names(levelnames)[which(names(levelnames) %in% rownames(levelvals))][1]
  if(is.na(bestlevel)) {
    cli_alert_info("No higher level name found. Returning original vector of area codes.")
    return(area.codes)
  }

  bestlevelval <- wgsrpd_mapping %>%
    dplyr::filter(.data$LEVEL3_COD %in% area.codes) %>%
    dplyr::select(dplyr::any_of(bestlevel)) %>%
    unique() %>%
    dplyr::pull()

 allcodesforname <-  wgsrpd_mapping %>%
    dplyr::filter(dplyr::if_any(bestlevel) %in% bestlevelval) %>%
    dplyr::arrange(.data$LEVEL3_COD) %>%
    dplyr::pull(.data$LEVEL3_COD)


  if(identical(allcodesforname, area.codes[order(area.codes)])) {
    return(bestlevelval)
  } else {
    cli_alert_info("No higher level name found. Returning original area codes as string.")
    return(paste(area.codes, collapse = "-"))
  }
}




