x <-
  datos_sia %>%
  dplyr::group_by(id_programa) %>%
  dplyr::sample_n(4) %>%
  dplyr::ungroup() %>%
  dplyr::filter(id_programa == 4L)
# test_that("Error si hay más de un id_matriz", {
#   testthat::expect_error(ancho(x),  
#                          "^Los datos tienen más de un valor de id_matriz")
# })

x <- x %>%
  dplyr::mutate(param = nombre_clave)
test_that("Columnas de sobra", {
  testthat::expect_warning(
    ancho(x),
    'Se eliminan automáticamente las columnas'
    )
})

x <- dplyr::select(x, -param, -parametro, -id_tipo_dato,
                   -grupo, -codigo_nuevo)

test_that("Sin columna param", {
  testthat::expect_warning(
    ancho(x), 'Se creó automáticamente la columna "param"'
    )
})

x <- dplyr::mutate(x, param = nombre_clave) %>%
  dplyr::select(-valor)

test_that("Sin columna valor", {
  testthat::expect_warning(
    ancho(x), 'Se creó automáticamente la columna\\: valor = valor_minimo_str'
    )
})

# x$valor <- x$valor_minimo_str
