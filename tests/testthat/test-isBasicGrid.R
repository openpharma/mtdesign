test_that("isBasicGrid correctly identifies basic grids", {
  good1 <- tibble::tibble(p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA)
  good2 <- tibble::tibble(p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA, somethingElse = NA)

  bad1 <- tibble::tibble(p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA)
  bad2 <- tibble::tibble(p0 = NA, nStage1 = NA, nTotal = NA, rFutility = NA, rTotal = NA)
  bad3 <- tibble::tibble(p0 = NA, p1 = NA, nTotal = NA, rFutility = NA, rTotal = NA)
  bad4 <- tibble::tibble(p0 = NA, p1 = NA, nStage1 = NA, rFutility = NA, rTotal = NA)
  bad5 <- tibble::tibble(p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rTotal = NA)
  bad6 <- tibble::tibble(p0 = NA, p1 = NA, nStage1 = NA, nTotal = NA, rFutility = NA)

  expect_true(isBasicGrid(good1))
  expect_true(isBasicGrid(as.data.frame(good1)))
  expect_true(isBasicGrid(good2))
  expect_false(isBasicGrid(bad1))
  expect_false(isBasicGrid(bad2))
  expect_false(isBasicGrid(bad3))
  expect_false(isBasicGrid(bad4))
  expect_false(isBasicGrid(bad5))
  expect_false(isBasicGrid(bad6))
})
