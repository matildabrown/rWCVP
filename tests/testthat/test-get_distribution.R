test_that("error if unrecognised rank used", {

  expect_error(get_distribution("Myrcia guianensis", "Species"))
})

test_that("correct geography for example", {
  example <- "Myrcia guianensis"
  example_rank <- "species"

  distribution <- get_distribution(example, example_rank, introduced=FALSE,
                                   extinct=FALSE, location_doubtful=FALSE)

  expect_true("BZN" %in% distribution$LEVEL3_COD)
})

test_that("returns a spatial dataframe", {
  distribution <- get_distribution("Myrcia guianensis", "species")
  expect_s3_class(distribution, "sf")
})
