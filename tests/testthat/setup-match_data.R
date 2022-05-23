library(dplyr)

match_data <- tibble(
  taxonId=c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5", "taxon6", "taxon7"),
  scientificName=c("Poa annua", "Myrcia acuminatissima",
                   "Abutilon angulatum var. angulatum", "Mercia almasensis",
                   "Myrcia almosensis", "× Tromostapelia mutabilis var americana",
                   "Astragalus canadense"),
  genus=c("Poa", "Myrcia", "Abutilon", "Mercia", "Myrcia", "Tromostapelia",
          "Astragalus"),
  species=c("annua", "acuminatissima", "angulatum", "almasensis", "almosensis",
            "mutabilis", "canadense"),
  infra_rank=c(NA_character_, NA_character_, "var.", NA_character_, NA_character_,
               "var", NA_character_),
  infra=c(NA_character_, NA_character_, "angulatum", NA_character_, NA_character_,
          "americana", NA_character_),
  authority=c("L.", "O.Berg", NA_character_, NA_character_, "NicLug", NA_character_,
              NA_character_)
)

lookup_data <- tibble(
  plant_name_id=c("435194-wcs", "130999-wcs", "130998-wcs", "534944-wcs",
                  "1261494-az", "610452-az", "131016-wcs", "512990-wcs",
                  "658189-az", "662570-az"),
  ipni_id=c("320035-2", "598579-1", "598578-1", "77199392-1", "77227834-1",
            "1125427-2", "304073-2", "970028-1", "24595-2", "479631-1"),
  taxon_name=c("Poa annua", "Myrcia acuminatissima", "Myrcia acuminatissima",
               "Myrcia acutissima", "Abutilon angulatum var. angulatum",
               "Abutilon angulatum", "Myrcia almasensis",
               "× Tromostapelia mutabilis var. americana", "Astragalus canadensis",
               "Astragalus saguntinus"),
  taxon_rank=c("Species", "Species", "Species", "Species", "Variety", "Species",
               "Species", "Variety", "Species", "Species"),
  taxon_status=c("Accepted", "Unplaced", "Synonym", "Accepted", "Accepted",
                 "Accepted", "Accepted", "Synonym", "Accepted", "Invalid"),
  taxon_authors=c("L.", "Hieron.", "O.Berg", "(Urb.) K.Campbell & K.Samra",
                  "", "(Guill. & Perr.) Mast.", "NicLugh.", "P.V.Heath", "L.", "Pau"),
  family=c("Poaceae", "Myrtaceae", "Myrtaceae", "Myrtaceae", "Malvaceae",
           "Malvaceae", "Myrtaceae", "Apocynaceae", "Fabaceae", "Fabaceae"),
  genus=c("Poa", "Myrcia", "Myrcia", "Myrcia", "Abutilon", "Abutilon", "Myrcia",
          "Tromostapelia", "Astragalus", "Astragalus"),
  accepted_plant_name_id=c("435194-wcs", NA_character_, "131542-wcs", "534944-wcs",
                           "1261494-az", "610452-az", "131016-wcs", "512989-wcs",
                           "658189-az", "662772-az"),
  homotypic_synonym=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
)
