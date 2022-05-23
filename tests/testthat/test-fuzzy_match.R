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
  expect_equal(nrow(matches), nrow(match_data)+1)
  expect_equal(sum(matches$multiple_matches), 2)
  expect_equal(sum(matches$match_type == "Fuzzy (edit distance)", na.rm=TRUE), 8)
})

test_that("phonetic match returns expected output", {
  matches <- phonetic_match(match_data, lookup_data, name_col="scientificName")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), nrow(match_data)+1)
  expect_equal(sum(matches$multiple_matches), 2)
  expect_equal(sum(matches$match_type == "Fuzzy (phonetic)", na.rm=TRUE), 7)
})

test_that("fuzzy match returns expected output", {
  matches <- fuzzy_match(match_data, lookup_data, name_col="scientificName")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), nrow(match_data)+1)
  expect_equal(sum(matches$multiple_matches), 2)
  expect_equal(sum(matches$match_type == "Fuzzy (phonetic)"), 7)
  expect_equal(sum(matches$match_type == "Fuzzy (edit distance)"), 1)
})

test_that("phonetic match returns NA if match is too disimilar", {
  phonetic <- phonetic_match(tail(match_data, 1), lookup_data, name_col="scientificName")
  expect_true(is.na(phonetic$match_type))
  expect_true(is.na(phonetic$wcvp_name))
})
