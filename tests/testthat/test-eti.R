test_that("eti: valor sin t_eti", {
  testthat::expect_equal(eti(2005, t_eti_base), "AlcT (mg CaCO3/L)")
})

test_that("eti: warning sin t_eti", {
  testthat::expect_warning(
    eti(2005),
    "Argumento t_eti faltante: se usa la tabla base \\(ver \\?t_eti_base\\)"
    )
})

test_that("eti: error por parámetro no encontrado", {
  x <- t_eti_add()
  testthat::expect_error(
    eti(5000, x),
    "No se encontró el parámetro \\(id = 5000\\) en la tabla t_eti"
  )
})
