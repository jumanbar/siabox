test_that("raiz: n par y x positivo", {
  testthat::expect_equal(raiz(4,  2), 4 ^ (1 / 2)) # raiz: n par y x positivo
})

test_that("raiz: n par y x negativo", {
  testthat::expect_equal(raiz(-4, 2), NaN)         # raiz: n par y x negativo
})

test_that("raiz: n impar y x positivo", {
  testthat::expect_equal(raiz(4,  3), 4 ^ (1/3))   # raiz: n impar y x positivo
})

test_that("raiz: n impar y x negativo", {
  testthat::expect_equal(raiz(-4, 3), -(4 ^ (1/3)))# raiz: n impar y x negativo
})

