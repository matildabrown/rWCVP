test_that("hybrid removal removes hybrid markers", {
  name <- "x genus species"
  expect_equal(remove_hybrid_(name), "genus species")
  name <- "× genus species"
  expect_equal(remove_hybrid_(name), "genus species")
  name <- "genus x species"
  expect_equal(remove_hybrid_(name), "genus species")
  name <- "genus × species"
  expect_equal(remove_hybrid_(name), "genus species")
})

test_that("hybrid removal keeps valid letters", {
  name <- "genux specix"
  expect_equal(remove_hybrid_(name), name)
})

test_that("infra standardisation works", {
  names <- c(
    "genus species ssp thing", "genus species var thing",
    "genus species forma thing"
  )
  correct <- c(
    "genus species subsp. thing", "genus species var. thing",
    "genus species f. thing"
  )
  expect_true(all(standardise_infras_(names) == correct))
})


test_that("sanitation removes hybrids and standardises infras", {
  name <- "x genus species var thing"
  expect_equal(sanitise_names_(name), "genus species var. thing")
})


test_that("sanitation removes special hybrids and standardises infras", {
  name <- c("Quercus × kinselae", "Sarracenia × readii", "Asplenium × waikomoi")
  expect_equal(sanitise_names_(name), c("Quercus kinselae", "Sarracenia readii", "Asplenium waikomoi"))
})
