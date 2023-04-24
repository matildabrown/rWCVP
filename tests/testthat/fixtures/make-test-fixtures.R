library(rWCVPdata)
library(dplyr)

fixture_dir <- "tests/testthat/fixtures"

# wcvp extracts ----
## names ----
names <- rWCVPdata::wcvp_names

wcvp_names_extract <- names |>
  filter(genus == "Poa" | genus == "Myrcia")

saveRDS(wcvp_names_extract, file = file.path(fixture_dir, "wcvp_names_extract.rds"))

## distributions ----
distributions <- rWCVPdata::wcvp_distributions

wcvp_distributions_extract <- distributions |>
  filter(plant_name_id %in% wcvp_names_extract$plant_name_id)

saveRDS(wcvp_distributions_extract, file = file.path(fixture_dir, "wcvp_distributions_extract.rds"))

# match data ----

match_data <- tibble(
  taxonId = c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5", "taxon6", "taxon7"),
  scientificName = c(
    "Poa annua", "Myrcia acuminatissima",
    "Abutilon angulatum var. angulatum", "Mercia almasensis",
    "Myrcia almosensis", "× Tromostapelia mutabilis var americana",
    "Astragalus canadense"
  ),
  genus = c(
    "Poa", "Myrcia", "Abutilon", "Mercia", "Myrcia", "Tromostapelia",
    "Astragalus"
  ),
  species = c(
    "annua", "acuminatissima", "angulatum", "almasensis", "almosensis",
    "mutabilis", "canadense"
  ),
  infra_rank = c(
    NA_character_, NA_character_, "var.", NA_character_, NA_character_,
    "var", NA_character_
  ),
  infra = c(
    NA_character_, NA_character_, "angulatum", NA_character_, NA_character_,
    "americana", NA_character_
  ),
  authority = c(
    "L.", "O.Berg", NA_character_, NA_character_, "NicLug", NA_character_,
    NA_character_
  ),
  assessmentId = 1:7
)

saveRDS(match_data, file = file.path(fixture_dir, "match_data.rds"))


lookup_data <- names %>% filter(taxon_name %in% lookup_data$taxon_name) %>%
  select(plant_name_id, ipni_id, taxon_name, taxon_rank, taxon_status,
         taxon_authors, family, genus, accepted_plant_name_id, homotypic_synonym) %>%
  filter(taxon_name != "Poa annua" | taxon_authors != "Schltdl. & Cham.")

saveRDS(lookup_data, file = file.path(fixture_dir, "lookup_data.rds"))
