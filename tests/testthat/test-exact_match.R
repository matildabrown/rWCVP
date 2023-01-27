test_that("exact match with author returns expected output", {
  matches <- wcvp_match_exact(match_data, lookup_data, name_col = "scientificName", author_col = "authority", id_col = "assessmentId")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), nrow(match_data))
  expect_equal(sum(matches$multiple_matches), 0)
  expect_equal(sum(matches$match_type == "Exact (with author)", na.rm = TRUE), 2)
})

test_that("exact match without author returns expected output", {
  matches <- wcvp_match_exact(match_data, lookup_data, name_col = "scientificName", author_col = NULL, id_col = "assessmentId")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), nrow(match_data) + 1)
  expect_equal(sum(matches$multiple_matches), 2)
  expect_equal(sum(matches$match_type == "Exact (without author)", na.rm = TRUE), 5)
})
