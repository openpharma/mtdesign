test_that("searchBounds works", {
  expect_equal(
    searchBounds(p0=0.1, p1=0.5, alpha=0.10, beta=0.1),
    searchBounds(p0=0.1, p1=0.5, alpha=0.05, beta=0.1, twoSided=FALSE)
  )
})
