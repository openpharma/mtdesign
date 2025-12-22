test_that("createGrid works", {
  x <- createGrid(p0 = 0.1, p1 = 0.5, alpha = 0.05, beta = 0.2)

  # Return type
  expect_true(is(x, "tbl"))
  expect_true(isBasicGrid(x))
  expect_true(isManderGrid(x))
  expect_false(isAugmented(x))
  # Constant values
  expect_true(x %>% dplyr::distinct(Alpha) %>% length() == 1)
  expect_true(min(x$Alpha) == 0.05)
  expect_true(x %>% dplyr::distinct(Beta) %>% length() == 1)
  expect_true(min(x$Beta) == 0.2)
  expect_true(x %>% dplyr::distinct(p0) %>% length() == 1)
  expect_true(min(x$p0) == 0.1)
  expect_true(x %>% dplyr::distinct(p1) %>% length() == 1)
  expect_true(min(x$p1) == 0.5)
  # Impossible combinations
  expect_true(x %>% dplyr::filter(nStage1 >= nTotal) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rFutility >= rTotal) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rSuccess > rTotal) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rFutility >= nStage1) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rSuccess > nStage1) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rTotal >= nTotal) %>% nrow() == 0)
  # All rows unique
  expect_identical(x, x %>% dplyr::distinct())

  x <- createGrid(p0 = 0.1, p1 = 0.5, alpha = 0.05, beta = 0.2, mander = FALSE)

  # Return type
  expect_true(is(x, "tbl"))
  expect_true(isBasicGrid(x))
  expect_false(isManderGrid(x))
  expect_false(isAugmented(x))
  # Constant values
  expect_true(x %>% dplyr::distinct(Alpha) %>% length() == 1)
  expect_true(min(x$Alpha) == 0.05)
  expect_true(x %>% dplyr::distinct(Beta) %>% length() == 1)
  expect_true(min(x$Beta) == 0.2)
  expect_true(x %>% dplyr::distinct(p0) %>% length() == 1)
  expect_true(min(x$p0) == 0.1)
  expect_true(x %>% dplyr::distinct(p1) %>% length() == 1)
  expect_true(min(x$p1) == 0.5)
  # Impossible combinations
  expect_true(x %>% dplyr::filter(nStage1 >= nTotal) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rFutility >= rTotal) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rFutility >= nStage1) %>% nrow() == 0)
  expect_true(x %>% dplyr::filter(rTotal >= nTotal) %>% nrow() == 0)
  # All rows unique
  expect_identical(x, x %>% dplyr::distinct())
})
