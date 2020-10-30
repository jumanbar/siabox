test_that("iet: PT > 0", {

  PT <- c(1, 10, 100, 1000)
  y <- c(33.9406808282664, 45.8996219698609,
         57.8585631114554, 69.8175042530499)
  testthat::expect_equal(iet(PT), y)
})

test_that("iet: PT < 0", {
  testthat::expect_warning(iet(-1), "NaNs produced")
})

test_that("iet: PT == 0", {
  testthat::expect_equal(iet(0), -Inf)
})
