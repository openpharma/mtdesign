test_that("isAugmented correctly identifies augmented grids", {
  good1 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rSuccess=NA, rTotal=NA, Type1=NA, Type2=NA, PETNull=NA, PETAlt=NA, AveSizeNull=NA,
                          AveSizeAlt=NA)
  good2 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rSuccess=NA,rTotal=NA, Type1=NA, Type2=NA, PETNull=NA, PETAlt=NA, AveSizeNull=NA,
                          AveSizeAlt=NA, somethingElse=NA)


  bad6 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rSuccess=NA, Type1=NA, Type2=NA, PETNull=NA, PETAlt=NA, AveSizeNull=NA,
                         AveSizeAlt=NA)
  bad7 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rTotal=NA, Type2=NA, PETNull=NA, PETAlt=NA, AveSizeNull=NA,
                         AveSizeAlt=NA)
  bad8 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rTotal=NA, Type1=NA, PETNull=NA, PETAlt=NA, AveSizeNull=NA,
                         AveSizeAlt=NA)
  bad9 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rTotal=NA, Type1=NA, Type2=NA, PETAlt=NA, AveSizeNull=NA,
                         AveSizeAlt=NA)
  bad10 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rTotal=NA, Type1=NA, Type2=NA, PETNull=NA, AveSizeNull=NA,
                         AveSizeAlt=NA)
  bad11 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rTotal=NA, Type1=NA, Type2=NA, PETNull=NA, PETAlt=NA,
                         AveSizeAlt=NA)
  bad12 <- tibble::tibble(p0=NA, p1=NA, nStage1=NA, nTotal=NA, rFutility=NA, rTotal=NA, Type1=NA, Type2=NA, PETNull=NA, PETAlt=NA, AveSizeNull=NA)

  expect_true(isAugmented(good1))
  expect_true(isAugmented(as.data.frame(good1)))
  expect_true(isAugmented(good2))
  expect_false(isAugmented(bad7))
  expect_false(isAugmented(bad8))
  expect_false(isAugmented(bad9))
  expect_false(isAugmented(bad10))
  expect_false(isAugmented(bad11))
  expect_false(isAugmented(bad12))
  expect_equal(isAugmented(tibble::tibble()), NA)
})