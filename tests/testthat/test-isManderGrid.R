test_that("isManderGrid correctly identifies Mander grids", {
  good1 <-
    tibble::tibble(
      p0 = NA,
      p1 = NA,
      nStage1 = NA,
      nTotal = NA,
      rFutility = NA,
      rSuccess = NA,
      rTotal = NA
    )
  good2 <-
    tibble::tibble(
      p0 = NA,
      p1 = NA,
      nStage1 = NA,
      nTotal = NA,
      rFutility = NA,
      rSuccess = NA,
      rTotal = NA,
      somethingElse = NA
    )

  bad1 <-
    tibble::tibble(
      p1 = NA,
      nStage1 = NA,
      nTotal = NA,
      rFutility = NA,
      rSuccess = NA,
      rTotal = NA
    )
  bad2 <-
    tibble::tibble(
      p0 = NA,
      nStage1 = NA,
      nTotal = NA,
      rFutility = NA,
      rSuccess = NA,
      rTotal = NA
    )
  bad3 <-
    tibble::tibble(
      p0 = NA,
      p1 = NA,
      nTotal = NA,
      rFutility = NA,
      rSuccess = NA,
      rTotal = NA
    )
  bad4 <-
    tibble::tibble(
      p0 = NA,
      p1 = NA,
      nStage1 = NA,
      rFutility = NA,
      rSuccess = NA,
      rTotal = NA
    )
  bad5 <-
    tibble::tibble(
      p0 = NA,
      p1 = NA,
      nStage1 = NA,
      nTotal = NA,
      rSuccess = NA,
      rTotal = NA
    )
  bad6 <-
    tibble::tibble(
      p0 = NA,
      p1 = NA,
      nStage1 = NA,
      nTotal = NA,
      rFutility = NA,
      rSuccess = NA
    )
  bad7 <-
    tibble::tibble(
      p0 = NA,
      p1 = NA,
      nStage1 = NA,
      nTotal = NA,
      rFutility = NA,
      rTotal = NA
    )

  expect_true(isManderGrid(good1))
  expect_true(isManderGrid(as.data.frame(good1)))
  expect_true(isManderGrid(good2))
  expect_false(isManderGrid(bad1))
  expect_false(isManderGrid(bad2))
  expect_false(isManderGrid(bad3))
  expect_false(isManderGrid(bad4))
  expect_false(isManderGrid(bad5))
  expect_false(isManderGrid(bad6))
  expect_false(isManderGrid(bad7))
})
