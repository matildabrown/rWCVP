test_that("error if unrecognised rank used", {
  expect_error(wcvp_distribution("Myrcia guianensis", "Species"))
})

test_that("correct geography for example", {
  example <- "Myrcia guianensis"
  example_rank <- "species"

  distribution <- wcvp_distribution(
    example,
    example_rank,
    wcvp_names = wcvp_names_extract,
    wcvp_distributions = wcvp_distributions_extract,
    introduced = FALSE,
    extinct = FALSE,
    location_doubtful = FALSE
  )

  expect_true("BZN" %in% distribution$LEVEL3_COD)
})

test_that("returns a spatial dataframe", {
  distribution <- wcvp_distribution(
    "Myrcia guianensis",
    "species",
    wcvp_names = wcvp_names_extract,
    wcvp_distributions = wcvp_distributions_extract
  )
  expect_s3_class(distribution, "sf")
})

test_that("returns expected distribution for Poa annua", {
  taxon <- "Poa annua"
  distribution <- wcvp_distribution(
    taxon,
    "species",
    wcvp_names = wcvp_names_extract,
    wcvp_distributions = wcvp_distributions_extract
  )
  n_introduced <- sum(distribution$occurrence_type == "introduced")
  n_native <- sum(distribution$occurrence_type == "native")

  expect_equal(n_introduced, 151)
  expect_equal(n_native, 113)
})

test_that("genus results are same as in WCVP", {
  taxon <- "Clethra"
  taxon_id <- 43184
  distribution <- wcvp_distribution(
    taxon,
    "genus",
    wcvp_names = wcvp_names_extract,
    wcvp_distributions = wcvp_distributions_extract
  )

  ref_distribution <- wcvp_distributions_extract |>
    filter(plant_name_id == taxon_id) |>
    group_by(area_code_l3) |>
    summarise(occurrence_type=determine_occurrence_type_(introduced, extinct, location_doubtful))

  expect_equal(distribution$LEVEL3_COD, ref_distribution$area_code_l3)
  expect_equal(distribution$occurrence_type, ref_distribution$occurrence_type)
})
