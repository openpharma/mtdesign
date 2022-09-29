test_that("isManderGrid correctly identifies Mander grids", {
  good1 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rSuccess = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  good2 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rSuccess = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA, somethingElse = NA
  )

  bad1 <- tibble::tibble(
    p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rSuccess = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad2 <- tibble::tibble(
    p0 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rSuccess = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad3 <- tibble::tibble(
    p0 = NA, p1 = NA, nTotal = NA, rFutility = NA, rSuccess = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad4 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, rFutility = NA, rSuccess = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad5 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rSuccess = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad6 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rSuccess = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad7 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad8 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA, Type1 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad9 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETAlt = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad10 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, AveSizeNull = NA,
    AveSizeAlt = NA
  )
  bad11 <- tibble::tibble(
    p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA,
    AveSizeAlt = NA
  )
  bad12 <- tibble::tibble(p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA, Type1 = NA, Type2 = NA, PETNull = NA, PETAlt = NA, AveSizeNull = NA)

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
  expect_false(isManderGrid(bad8))
  expect_false(isManderGrid(bad9))
  expect_false(isManderGrid(bad10))
  expect_false(isManderGrid(bad11))
  expect_false(isManderGrid(bad12))
})
