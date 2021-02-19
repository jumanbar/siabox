sia_matriz <- structure(list(
  id_matriz = 6:20, 
  nombre = c("Aguas superficiales", "Aguas subterr\u00e1neas", 
             "Aguas de lluvia", 
             "Efluentes l\u00edquidos industriales y aguas contaminadas", 
             "Aguas potables", "Sedimentos", "Suelos", "Residuos S\u00f3lidos",
             "Arena", "Aire", "Emisiones a la atm\u00f3sfera", "Flora", "Fauna",
             "Otros", "Emisiones Aire"), 
  codigo = c("AG_SUP", "AG_SUB", "AG_LLU", "EFLUENTE", "AG_POT", "SEDIMENTO", 
             "SUELO", "RES_SOL", "ARENA", "AIRE", "EMISION", "FLORA", "FAUNA", 
             "OTRO", "EMISIONES"), 
  vigente = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
              TRUE, TRUE, TRUE, FALSE), 
  fca_habilitado = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                     TRUE, TRUE, TRUE, TRUE, FALSE)
), row.names = c(NA, 15L), class = c("tbl_df", "tbl", "data.frame"))
save(sia_matriz, file="data/sia_matriz.rda")