test_that("match names with author returns expected output", {
  matches <- match_names(match_data, lookup_data, id_col="taxonId",
                         name_col="scientificName", author_col="authority")

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 5)
  expect_equal(sum(matches$multiple_matches, na.rm=TRUE), 0)
  expect_equal(sum(matches$match_type == "Exact (with author)"), 2)
  expect_equal(sum(matches$match_type == "Exact (without author)"), 1)
  expect_equal(sum(matches$match_type == "Fuzzy (phonetic)"), 2)
})

test_that("match names without author returns expected output", {
  matches <- match_names(match_data, lookup_data, id_col="taxonId",
                         name_col="scientificName", author_col=NULL)

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 6)
  expect_equal(sum(matches$multiple_matches, na.rm=TRUE), 2)
  expect_equal(sum(matches$match_type == "Exact (with author)"), 0)
  expect_equal(sum(matches$match_type == "Exact (without author)"), 4)
  expect_equal(sum(matches$match_type == "Fuzzy (phonetic)"), 2)
})

test_that("match names without fuzzy returns expected output", {
  matches <- match_names(match_data, lookup_data, id_col="taxonId", name_col="scientificName",
                         author_col="authority", fuzzy=FALSE)

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 5)
  expect_equal(sum(matches$multiple_matches, na.rm=TRUE), 0)
  expect_equal(sum(matches$match_type == "Exact (with author)", na.rm=TRUE), 2)
  expect_equal(sum(matches$match_type == "Exact (without author)", na.rm=TRUE), 1)
  expect_equal(sum(matches$match_type == "Fuzzy (phonetic)", na.rm=TRUE), 0)
})

test_that("match names without taxon name parts returns expected output", {
  matches <- match_names(match_data, lookup_data, id_col="taxonId",
                         join_cols=c("genus", "species", "infra_rank", "infra"))

  expect_true(all(c("taxon1", "taxon2", "taxon3", "taxon4", "taxon5") %in% matches$taxonId))
  expect_equal(nrow(matches), 6)
  expect_equal(sum(matches$multiple_matches, na.rm=TRUE), 2)
  expect_equal(sum(matches$match_type == "Exact (with author)"), 0)
  expect_equal(sum(matches$match_type == "Exact (without author)"), 4)
  expect_equal(sum(matches$match_type == "Fuzzy (phonetic)"), 2)
})

test_that("match names returns original columns", {
  matches <- match_names(match_data, lookup_data, id_col="taxonId", name_col="scientificName",
                         author_col="authority")

  expect_true(all(colnames(match_data) %in% colnames(matches)))
})
