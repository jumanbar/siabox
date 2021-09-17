
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
#' lista_pd <- readRDS("~/R/sia_apps/vSIA/tmp/lista_pd.rds")
#' dl <- siabox::largo.planilla(lista_pd$datos, lista_pd$ppd)
#' dl$id_programa <- 13L
#' dl$id_matriz <- 6L
#' sitio_ <- dplyr::filter(sitio,
#'                         id_fuente == 1L,
#'                         id_matriz == 6L) %>%
#'   dplyr::select(id_sitio, id_interno)
#'
#' datos <- siabox::sia_estacion %>%
#'   dplyr::filter(prog_monitoreo == 13L) %>%
#'   dplyr::select(codigo_pto, id_interno = id) %>%
#'   dplyr::left_join(sitio_, by = "id_interno") %>%
#'   dplyr::right_join(dl, by = c("codigo_pto" = "Estacion")) %>%
#'   valores_numericos(metodo = "basico")
#' valida_eval(datos)
valida_eval <- function(.data) {
  require(magrittr)

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
