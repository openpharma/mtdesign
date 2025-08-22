test_that("multiplication works", {
  manderGrid <- obtainDesign(
    p0=0.1,
    p1=0.5,
    alpha=0.1,
    beta=0.1,
    parallel = FALSE
  )
  simonGrid <- obtainDesign(
    p0=0.1,
    p1=0.5,
    alpha=0.1,
    beta=0.1,
    mander=FALSE,
    parallel = FALSE
  )

  expect_s3_class(powerPlot(manderGrid), "ggplot")
  expect_s3_class(powerPlot(simonGrid), "ggplot")
})
