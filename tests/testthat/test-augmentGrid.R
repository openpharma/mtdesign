test_that("augmentGrid works", {
  x <- createGrid(p0 = 0.1, p1 = 0.5, alpha = 0.05, beta = 0.2)
  y <- x %>% augmentGrid(parallel = FALSE)

  # Return value
  expect_true(isAugmented(y))
  expect_identical(x, y %>% dplyr::select(1:9))

  # Impossible combinations
  expect_true(y %>% dplyr::filter(AveSizeNull > nTotal) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(AveSizeNull < nStage1) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(AveSizeAlt > nTotal) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(AveSizeAlt < nStage1) %>% nrow() == 0)

  # Impossible values
  expect_true(y %>% dplyr::filter(Type1 < 0) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(Type1 > 1) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(Type2 < 0) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(Type2 > 1) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(PETNull < 0) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(PETNull > 1) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(PETAlt < 0) %>% nrow() == 0)
  expect_true(y %>% dplyr::filter(PETAlt > 1) %>% nrow() == 0)
})
