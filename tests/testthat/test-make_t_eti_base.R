test_that("make_t_eti_base: crea tibble", {
  testthat::expect_true(tibble::is_tibble(make_t_eti_base()))
})

test_that("make_t_eti_base: nombres de columnas", {
  x <- names(make_t_eti_base())
  testthat::expect_equal(x, c("id_parametro", "etiqueta"))
})

test_that("make_t_eti_base: clases", {
  x <- make_t_eti_base()
  testthat::expect_type(x[[1]], "integer")
  testthat::expect_type(x[[2]], "character")
})

test_that("make_t_eti_base: sin parametros repetidos", {
  conteos <- table(make_t_eti_base()[[1]])
  testthat::expect_false(any(conteos > 1))
})
