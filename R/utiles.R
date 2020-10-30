#' Tabla de etiquetas base
#'
#' Crea una tabla de base para hacer etiquetas para gráficos u otros usos,
#' basándose en las tablas del SIA (infambientalbd).
#'
#' @return Una tibble (\code{tbl_df}) con dos columnas: \code{id_parametro} y
#'   \code{etiqueta}
#'
#' @examples
#' make_t_eti_base()
make_t_eti_base <- function() {
  out <-
    sia_parametro %>%
    dplyr::left_join(dplyr::select(codigos_param, id_parametro, codigo_nuevo),
                     by = "id_parametro") %>%
    dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
                                         nombre_clave, codigo_nuevo)) %>%
    dplyr::left_join(sia_param_unidad, by = "id_parametro") %>%
    dplyr::filter(id_matriz == 6L) %>%
    dplyr::left_join(sia_unidad, by = c("id_unidad_medida" = "id")) %>%
    dplyr::transmute(
      id_parametro,
      etiqueta = paste0(param, " (", uni_nombre, ")")
    )
  return(out)
}
