test_that("fuzzy match returns name within correct distance", {
  name <- "Myrcia almosensis"
  lookup <- data.frame(
    genus=c("Myrcia", "Poa"),
    taxon_name=c("Myrcia almasensis", "Poa annua"),
    plant_name_id=c("131016-wcs", "435194-wcs")
  )

  match <- fuzzy_match_name_(name, lookup)
  expect_equal(match$match_name, "Myrcia almasensis")
  expect_equal(match$match_edit_distance, 1)
})

test_that("fuzzy match returns name within correct distance when genus mispelled", {
  name <- "Mercia almasensis"
  lookup <- data.frame(
    genus=c("Myrcia", "Poa"),
    taxon_name=c("Myrcia almasensis", "Poa annua"),
    plant_name_id=c("131016-wcs", "435194-wcs")
  )

  match <- fuzzy_match_name_(name, lookup)
  expect_equal(match$match_name, "Myrcia almasensis")
  expect_equal(match$match_edit_distance, 1)
})
