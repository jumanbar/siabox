categoria_mensaje <- structure(
  list(id_tipo_msj = 1:7,
       categ_msj = c("Valor menor al rango normal",
                     "Valor menor al rango normal para el sitio",
                     "Valor mayor al rango normal",
                     "Valor mayor al rango normal para el sitio",
                     "Valor menor al LD",
                     "Valor menor al LC",
                     "LD mayor o igual al LC")),
  row.names = c(NA, -7L),
  class = c("tbl_df", "tbl", "data.frame"))

# save(categoria_mensaje, file = "data/categoria_mensaje.rda")
