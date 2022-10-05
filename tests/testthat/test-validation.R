test_that("createGrid returns appropriate error with bad input", {
  expect_error(createGrid(p0=NA, p1=0.4, alpha=0.1, beta=0.1, mander=FALSE))
  expect_error(createGrid(p0=NULL, p1=0.4, alpha=0.1, beta=0.1, mander=FALSE))
  expect_error(createGrid(p0=2, p1=0.4, alpha=0.1, beta=0.1, mander=FALSE))
  expect_error(createGrid(p0=-1, p1=0.4, alpha=0.1, beta=0.1, mander=FALSE))
  expect_error(createGrid(p0=-1, p1=0.4, alpha=0.1, beta=NA, power=NA, mander=FALSE))

  expect_error(createGrid(p0 = 0.2, p1 = NA, alpha = 0.1, beta = 0.1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = NULL, alpha = 0.1, beta = 0.1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 7, alpha = 0.1, beta = 0.1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = -0.1, alpha = 0.1, beta = 0.1, mander = FALSE))

  expect_error(createGrid(p0 = 0.2, p1 = 0.2, alpha = 0.1, beta = 0.1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.1, alpha = 0.1, beta = 0.1, mander = FALSE))

  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 1, beta = 0.1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0, beta = 0.1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = -1, beta = 0.1, mander = FALSE))

  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, beta = 1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, beta = 0, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, beta = -1, mander = FALSE))

  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, power = 1, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, power = 0, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, power = -1, mander = FALSE))

  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, beta = 0.1, power = 0.8, mander = FALSE))

  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, beta = NULL, power = NULL, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, beta = NA, power = NA, mander = FALSE))
  expect_error(createGrid(p0 = 0.2, p1 = 0.4, alpha = 0.1, beta = NULL, power = NA, mander = FALSE))
})

test_that("obtainDesign returns appropriate error with bad input", {
  expect_error(obtainDesign(grid=NULL, p0=NA, p1=0.45, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=NA, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=NA, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=0.05, beta=NA, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=NULL, p1=0.45, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=NULL, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=NULL, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=0.05, beta=NULL, fullGrid=TRUE))

  expect_error(obtainDesign(grid=NULL, p0=0, p1=0.45, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=1, p1=0.45, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=-1, p1=0.45, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=6, p1=0.45, alpha=0.05, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=0, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=1, beta=0.2, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=0.05, beta=0, fullGrid=TRUE))
  expect_error(obtainDesign(grid=NULL, p0=0.25, p1=0.45, alpha=0.05, beta=1, fullGrid=TRUE))

  expect_error(obtainDesign(grid=tibble::tibble()))
})

test_that("powerPlot returns appropriate error with bad input", {
  expect_error(powerPlot(grid=NA))
  expect_error(powerPlot(grid=tibble::tibble()))
})

