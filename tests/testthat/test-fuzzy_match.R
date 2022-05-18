test_that("edit match returns name within correct distance", {
  name <- "Myrcia almosensis"

  match <- edit_match_name_(name, lookup_data)
  expect_equal(match$taxon_name, "Myrcia almasensis")
  expect_equal(match$match_edit_distance, 1)
})

test_that("edit match returns name within correct distance when genus mispelled", {
  name <- "Mercia almasensis"

  match <- edit_match_name_(name, lookup_data)
  expect_equal(match$taxon_name, "Myrcia almasensis")
  expect_equal(match$match_edit_distance, 1)
})

test_that("edit match returns expected output", {
  matches <- edit_match(match_data, lookup_data, name_col="scientificName")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 6)
  expect_equal(sum(matches$match_type == "Multiple matches found"), 2)
  expect_equal(sum(matches$match_type == "Fuzzy matched (edit distance)"), 4)
})

test_that("phonetic match returns expected output", {
  matches <- phonetic_match(match_data, lookup_data, name_col="scientificName")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 6)
  expect_equal(sum(matches$match_type == "Multiple matches found"), 2)
  expect_equal(sum(matches$match_type == "Fuzzy matched (phonetically)"), 4)
})

test_that("fuzzy match returns expected output", {
  matches <- fuzzy_match(match_data, lookup_data, name_col="scientificName")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 6)
  expect_equal(sum(matches$match_type == "Multiple matches found"), 2)
  expect_equal(sum(matches$match_type == "Fuzzy matched (phonetically)"), 4)
})

