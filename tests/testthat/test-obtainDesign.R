futile.logger::flog.threshold(futile.logger::ERROR)

test_that("obtainDesign works", {
  # Mander & Thompson Section 3.2
  observed <- obtainDesign(
    p0 = 0.1,
    p1 = 0.3,
    alpha = 0.05,
    beta = 0.15,
    mander = FALSE,
    parallel = FALSE
  )
  expected <- structure(
    list(
      nTotal = c(35L, 27L),
      nStage1 = c(11L, 18L),
      rTotal = 6:5,
      rFutility = 1:2,
      p0 = c(0.1, 0.1),
      p1 = c(0.3, 0.3),
      Alpha = c(0.05, 0.05),
      Beta = c(0.15, 0.15),
      Type1 = c(0.0422347997307906, 0.0444178417084868),
      Type2 = c(0.148976337280657, 0.149452890006293),
      PETNull = c(0.6973568802, 0.733795994785329),
      PETAlt = c(0.1129900996, 0.0599522067475602),
      AveSizeNull = c(18.2634348752, 20.395836046932),
      AveSizeAlt = c(32.2882376096, 26.460430139272),
      Criterion = c("optimal", "minimax")
    ),
    row.names = c(NA, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(observed, expected)

  # Mander & Thompson Section 4
  observed <- obtainDesign(
    p0 = 0.2,
    p1 = 0.4,
    alpha = 0.1,
    beta = 0.1,
    mander = FALSE,
    parallel = FALSE
  )
  expected <- structure(
    list(
      nTotal = 37:36,
      nStage1 = c(17L, 19L),
      rTotal = c(10L, 10L),
      rFutility = c(3L, 3L),
      p0 = c(0.2, 0.2),
      p1 = c(0.4, 0.4),
      Alpha = c(0.1, 0.1),
      Beta = c(0.1, 0.1),
      Type1 = c(0.0947843743314956, 0.0860944606833201),
      Type2 = c(0.0967257134075589, 0.0976469860520819),
      PETNull = c(0.548876204585779, 0.455088742345789),
      PETAlt = c(0.0464229308104704, 0.0229593208708399),
      AveSizeNull = c(26.0224759082844, 28.2634913801216),
      AveSizeAlt = c(36.0715413837906, 35.6096915451957),
      Criterion = c("optimal", "minimax")
    ),
    row.names = c(NA, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(observed, expected)

  expected <- structure(
    list(
      nTotal = c(41L, 35L, 38L, 35L),
      nStage1 = c(16L, 27L, 15L, 27L),
      rTotal = c(11L, 10L, 11L, 10L),
      rFutility = c(3L, 5L, 1L, 5L),
      rSuccess = c(6, 8, 5, 8),
      p0 = c(0.2, 0.2, 0.2, 0.2),
      p1 = c(0.4, 0.4, 0.4, 0.4),
      Alpha = c(0.1, 0.1, 0.1, 0.1),
      Beta = c(0.1, 0.1, 0.1, 0.1),
      Type1 = c(0.0959982365130688, 0.0994811689904694, 0.0998619264781272, 0.0994811689904694),
      Type2 = c(0.0984812292666625, 0.0970384917936091, 0.0951223618482245, 0.0970384917936091),
      PETNull = c(0.62479165702144, 0.612310129358628, 0.228177197039616, 0.612310129358628),
      PETAlt = c(0.537972628160512, 0.83154847959435, 0.601956484415488, 0.83154847959435),
      AveSizeNull = c(25.380208574464, 30.101518965131, 32.7519244680888, 30.101518965131),
      AveSizeAlt = c(27.5506842959872, 28.3476121632452, 24.1550008584438, 28.3476121632452),
      Criterion = c("optimalNull", "minimaxNull", "optimalAlt", "minimaxAlt")
    ),
    row.names = c(NA, -4L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  observed <- obtainDesign(
    p0 = 0.2,
    p1 = 0.4,
    alpha = 0.1,
    beta = 0.1,
    cores = 2
  )
  expect_equal(observed, expected)
})
