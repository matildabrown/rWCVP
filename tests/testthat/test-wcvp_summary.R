test_that("total species number is the same regardless of grouping", {
  taxon <- "Angiosperms"
  taxon_rank <- "higher"
  wcvp_names <- wcvp_names_extract
  wcvp_distributions <- wcvp_distributions_extract

  taxonomic_groups <- wcvp_summary(taxon=taxon, taxon_rank=taxon_rank,
                                   grouping_var="family",
                                   wcvp_names=wcvp_names,
                                   wcvp_distributions=wcvp_distributions)
  area_groups <- wcvp_summary(taxon=taxon, taxon_rank=taxon_rank,
                              grouping_var="area_code_l3",
                              wcvp_names=wcvp_names,
                              wcvp_distributions=wcvp_distributions)

  expect_equal(taxonomic_groups$Total_number_of_species, area_groups$Total_number_of_species)
  expect_equal(taxonomic_groups$Number_of_regionally_endemic_species,
               area_groups$Number_of_regionally_endemic_species)
})

test_that("regional endemics is sum of endemics column for taxonomic groups", {
  taxon <- "Angiosperms"
  taxon_rank <- "higher"
  wcvp_names <- wcvp_names_extract
  wcvp_distributions <- wcvp_distributions_extract

  summary <- wcvp_summary(taxon=taxon, taxon_rank=taxon_rank,
                                   grouping_var="family",
                                   area_codes=get_wgsrpd3_codes("Asia-Tropical"),
                                   wcvp_names=wcvp_names,
                                   wcvp_distributions=wcvp_distributions)

  expect_equal(summary$Number_of_regionally_endemic_species,
               sum(summary$Summary$Endemic))
})
