test_that("Error if no occurrences found", {
  taxon <- "Myrcia"
  taxon_rank <- "genus"
  area_codes <- c("GRB", "FRA")

  expect_error(
    wcvp_occ_mat(taxon,
      taxon_rank = taxon_rank, area_codes = area_codes,
      introduced = FALSE, wcvp_names = wcvp_names_extract,
      wcvp_distributions = wcvp_distributions_extract
    ),
    regexp = "No occurrences"
  )
})

test_that("All area codes in output", {
  taxon <- "Myrcia"
  taxon_rank <- "genus"
  area_codes <- c("BZE", "BZC", "BZL", "BZN", "BZS", "GRB", "FRA")

  occ_mat <- wcvp_occ_mat(taxon,
    taxon_rank = taxon_rank, area_codes = area_codes,
    introduced = FALSE, wcvp_names = wcvp_names_extract,
    wcvp_distributions = wcvp_distributions_extract
  )
  all_codes <- rWCVP::wgsrpd3$LEVEL3_COD
  returned_codes <- intersect(all_codes, colnames(occ_mat))

  expect_setequal(area_codes, returned_codes)
})
