## code to prepare `wgsrpd3` and derived datasets goes here
library(sf)
library(lwgeom)
library(dplyr)

# wgsrpd3 ----
shp_url <- "https://github.com/tdwg/wgsrpd/archive/refs/heads/master.zip"

# download and extract the folder
temp <- tempfile()
download.file(shp_url, temp)
unzip(temp)

# load level 3 shape file
wgsrpd3_orig <- st_read("wgsrpd-master/level3/level3.shp")

# clean up directory
unlink(temp)
unlink("wgsrpd-master", recursive = TRUE)

# before fixing the shape file doesn't work with spherical coordinates
sf_use_s2(FALSE)

wgsrpd3 <- st_buffer(wgsrpd3_orig, 0)
wgsrpd3 <- st_crop(wgsrpd3, st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)))

usethis::use_data(wgsrpd3, overwrite = TRUE)

# hemisphere codes ----

north_with_eq <- c(
  "ABT", "AFG", "ALA", "ALB", "ALG", "ALT", "ALU", "AMU", "AND",
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
  "YEM", "YUG", "YUK", "ZAI"
)
north_without_eq <- c(
  "ABT", "AFG", "ALA", "ALB", "ALG", "ALT", "ALU", "AMU", "AND",
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
  "YAK", "YEM", "YUG", "YUK"
)
south_with_eq <- c(
  "AGE", "AGS", "AGW", "ALD", "ANG", "ANT", "ASC", "ASP", "ATP",
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
  "WAL", "WAU", "XMS", "ZAI", "ZAM", "ZIM"
)
south_without_eq <- c(
  "AGE", "AGS", "AGW", "ALD", "ANG", "ANT", "ASC", "ASP", "ATP",
  "BIS", "BOL", "BOT", "BOU", "BUR", "BZC", "BZE", "BZL", "BZS",
  "CAB", "CGS", "CKI", "CLC", "CLN", "CLS", "COM", "COO", "CPP",
  "CPV", "CRZ", "CTM", "DSV", "EAS", "FAL", "FIJ", "HMD", "JAW",
  "JNF", "KEG", "KER", "LES", "LSI", "MAQ", "MAU", "MCI", "MDG",
  "MLW", "MOZ", "MPE", "MRQ", "NAM", "NAT", "NFK", "NRU", "NSW",
  "NTA", "NUE", "NWC", "NWG", "NZN", "NZS", "OFS", "PAR", "PER",
  "PHX", "PIT", "QLD", "REU", "ROD", "RWA", "SAM", "SCI", "SCZ",
  "SEY", "SGE", "SOA", "SOL", "SSA", "STH", "SWZ", "TAN", "TAS",
  "TDC", "TOK", "TON", "TUA", "TUB", "TUV", "TVL", "URU", "VAN",
  "VIC", "WAL", "WAU", "XMS", "ZAM", "ZIM"
)

hemispheres <- list(
  north_with_eq = north_with_eq,
  north_without_eq = north_without_eq,
  south_with_eq = south_with_eq,
  south_without_eq = south_without_eq
)

usethis::use_data(hemispheres, internal = TRUE, overwrite = TRUE)
