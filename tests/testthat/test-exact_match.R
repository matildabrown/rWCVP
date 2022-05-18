test_that("exact match with author returns expected output", {
  matches <- exact_match(match_data, lookup_data, name_col="scientificName", author_col="authority")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 5)
  expect_equal(sum(matches$match_type == "Multiple matches found"), 0)
  expect_equal(sum(matches$match_type == "Matched in WCVP (with author)"), 2)
})

test_that("exact match without author returns expected output", {
  matches <- exact_match(match_data, lookup_data, name_col="scientificName", author_col=NULL)

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 6)
  expect_equal(sum(matches$match_type == "Multiple matches found"), 2)
  expect_equal(sum(matches$match_type == "Matched in WCVP (without author)"), 2)
})
