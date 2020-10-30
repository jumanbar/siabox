test_that("t_eti_add: crea tibble", {
  testthat::expect_true(tibble::is_tibble(t_eti_add()))
})

test_that("t_eti_add: id_parametro no integer (warning)", {
  testthat::expect_warning(t_eti_add(2098, "PT (ug/L)"),
                           "se coerciona a integer")
})

test_that("t_eti_add: nombres de columnas", {
  x <- names(t_eti_add(2098L, "PT (ug/L)"))
  testthat::expect_equal(x, c("id_parametro", "etiqueta"))
})

test_that("t_eti_add: clases sin expressiones", {
  x <- t_eti_add(2098L, "PT (ug/L)")
  testthat::expect_type(x[[1]], "integer")
  testthat::expect_type(x[[2]], "character")
})

test_that("t_eti_add: clases con expressiones", {
  x <- t_eti_add(2005L, expression('Alcalinidad Total (mg CaCO' [3] * ' /L)'))
  testthat::expect_type(x[[1]], "integer")
  testthat::expect_type(x[[2]], "list")
})

test_that("t_eti_add: sin parametros repetidos", {
  conteos <- table(t_eti_add(2098L, "PT (ug/l)")[[1]])
  testthat::expect_false(any(conteos > 1))
})

test_that("t_eti_add: agrega parámetro nuevo correctamente", {
  x <- t_eti_add()
  y <- t_eti_add(as.integer(max(x[[1]]) + 1), expression('CH' [4] * '(ug /L)'))
  conteos <- table(y[[1]])
  testthat::expect_equal(names(y), c("id_parametro", "etiqueta"))
  testthat::expect_type(y[[1]], "integer")
  testthat::expect_type(y[[2]], "list")
  testthat::expect_false(any(conteos > 1))
  testthat::expect_true(nrow(x) < nrow(y))
})
