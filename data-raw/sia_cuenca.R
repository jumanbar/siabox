# Atención: le quité los \\n al final de cada entrada de cue_nombre
sia_cuenca <- structure(list(id = c(1L, 3L, 4L, 5L, 6L, 0L, 10L, 2L), 
                             cue_nombre = c("R\u00cdO URUGUAY", 
                                            "OC\u00c9ANO ATL\u00c1NTICO",
                                            "LAGUNA MERIN",
                                            "R\u00cdO NEGRO", 
                                            "R\u00cdO SANTA LUC\u00cdA", 
                                            "FALTA CUENCA", "DATO MIGRADO", 
                                            "R\u00cdO DE LA PLATA")), 
                        row.names = c(NA, -8L), 
                        class = c("tbl_df", "tbl", "data.frame"))

save(sia_cuenca, file = 'data/sia_cuenca.rda')
