# @ t_eti_base ----
t_eti_base <-
  sia_parametro %>%
  dplyr::left_join(dplyr::select(codigos_param, id_parametro, codigo_nuevo),
                   by = "id_parametro") %>%
  dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
                                       nombre_clave, codigo_nuevo)) %>%
  dplyr::left_join(sia_param_unidad, by = "id_parametro") %>%
  dplyr::filter(id_matriz == 6L) %>%
  dplyr::left_join(sia_unidad, by = c("id_unidad_medida" = "id")) %>%
  dplyr::transmute(id_parametro,
                   etiqueta = paste0(param, " (", uni_nombre, ")"))

# save(t_eti_base, file = 'data/t_eti_base.rda')
