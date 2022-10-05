test_that("multiplication works", {
  manderGrid <- obtainDesign(p0=0.1, p1=0.5, alpha=0.1, beta=0.1)
  simonGrid <- obtainDesign(p0=0.1, p1=0.5, alpha=0.1, beta=0.1, mander=FALSE)

  expect_equal(class(powerPlot(manderGrid)), c("gg", "ggplot"))
  expect_equal(class(powerPlot(simonGrid)), c("gg", "ggplot"))
})
