sia_tipo_punto_estacion <- 
  structure(list(id = 1:2, 
                 tip_pun_est_descripcion = c("SUPERFICIE", 
                                             "FONDO"), 
                 tip_pun_est_codigo = c("S", "F")), 
            row.names = 1:2, class = c("tbl_df", "tbl", "data.frame"))
save(sia_tipo_punto_estacion, file = 'data/sia_tipo_punto_estacion.rda')
