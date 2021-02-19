sia_departamento <- 
  structure(list(
    id = 1:21, 
    dep_nombre = c("ARTIGAS", "CANELONES", "CERRO LARGO", "COLONIA", "DURAZNO", 
                   "FLORES", "FLORIDA", "LAVALLEJA", "MALDONADO", "MONTEVIDEO", 
                   "PAYSAND\u00da", "R\u00cdO NEGRO", "RIVERA", "ROCHA", "SALTO", 
                   "SAN JOS\u00c9", "SORIANO", "TACUAREMB\u00d3", "TREINTA Y TRES", 
                   "DATO MIGRADO", "---"), 
    dep_codigo = c("AR", "CA", "CL", "CO", "DU", "FS", "FD", "LA", "MA", "MO", 
                   "PA", "RN", "RV", "RO", "SA", "SJ", "SO", "TA", "TT", "XX", 
                   "NN")
  ), row.names = c(NA, 21L), class = c("tbl_df", "tbl", "data.frame"))
save(sia_departamento, file="data/sia_departamento.rda")