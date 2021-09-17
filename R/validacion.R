
#' Evaluación de valores
#'
#' @param .data
#'
#' @return Tabla con varias columnas booleanas que indican resultados de varias
#'   evaluaciones:
#'
#'   - Si hay valores menores al mínimo del rango normal esperado.
#'   - Si hay valores mayores al máximo del rango normal esperado.
#'   - Si hay valores menores al límite de detección.
#'   - Si hay valores menores al límite de cuantificación.
#'   - Si hay límites de detecciones mayores a los límites de cuantificaciones.
#'
#' @export
#'
#' @examples
#' alt <- siabox::datos_sia %>%
#'   dplyr::mutate(
#'     limite_deteccion = as.numeric(limpia_num(limite_deteccion)),
#'     limite_cuantificacion = as.numeric(limpia_num(limite_cuantificacion))
#'   ) %>%
#'   dplyr::left_join(
#'     dplyr::select(siabox::sitio,
#'                   id_sitio, id_matriz, id_estacion = id_interno),
#'     by = c("id_matriz", "id_estacion")
#'   ) %>%
#'   dplyr::select(id_sitio, codigo_pto, fecha_muestra, id_matriz, id_parametro,
#'                 param, valor, limite_deteccion, limite_cuantificacion) %>%
#'   v_valor %>%
#'   dplyr::filter(menor_minimo | mayor_maximo | valor_menor_ld |
#'                   valor_menor_lc | ld_mayor_lc) %>%
#'   dplyr::filter(mayor_maximo) %>%
#'   dplyr::select(codigo_pto, param, fecha_muestra,
#'                 valor_max, valor_max_sitio, vsup, valor)
v_valor <- function(.data) {
  require(magrittr)

  col_enc <- names(.data)

  col_esp <- c("valor", "limite_deteccion", "limite_cuantificacion",
               "id_parametro", "id_matriz", "id_sitio")

  w <- which(!(col_esp %in% col_enc))
  if (length(w)) stop("No se encontraron una o más columnas en .data: ",
                      colapsar_secuencia(col_esp[w]))

  if (!is.numeric(.data$valor))
    stop(".data$valor no es numérico (", typeof(.data$valor), ")")

  if (!is.numeric(.data$limite_deteccion))
    stop(".data$limite_deteccion no es numérico (",
         typeof(.data$limite_deteccion), ")")

  if (!is.numeric(.data$limite_cuantificacion))
    stop(".data$limite_cuantificacion no es numérico (",
         typeof(.data$limite_cuantificacion), ")")

  out <- NULL
  djoins <- .data %>%
    dplyr::left_join(siabox::rango_param,
                     by = c("id_parametro", "id_matriz")) %>%
    dplyr::left_join(siabox::rango_param_sitio,
                     by = c("id_sitio", "id_parametro"))

  out <- djoins %>%
    dplyr::mutate(
      vinf = dplyr::if_else(
        is.na(valor_min_sitio),
        dplyr::if_else(is.na(valor_min), -Inf, valor_min),
        valor_min_sitio
      ),
      vsup = dplyr::if_else(
        is.na(valor_max_sitio),
        dplyr::if_else(is.na(valor_max),  Inf, valor_max),
        valor_max_sitio
      ),
      menor_minimo = valor <= vinf,
      mayor_maximo = valor >= vsup,
      valor_menor_ld = valor < limite_deteccion,
      valor_menor_lc = valor < limite_cuantificacion,
      ld_mayor_lc = limite_deteccion >= limite_cuantificacion
    ) %>%
    tidyr::replace_na(list(valor_menor_ld = FALSE,
                           valor_menor_lc = FALSE,
                           ld_mayor_lc = FALSE))

  return(out)
}
