# INFAMBIENTAL ----
#
# Para que no cambien los datos, sería necesario hacer esto desde mi computadora
# perosnal. JMB.
# INFAMBIENTAL ----
con <- siabox:::con_sia()

trae_tabla <- function(tabla_string) {
  dplyr::collect(dplyr::tbl(con, tabla_string))
}

# @ siabox.extraction.meta -----
siabox.extraction.meta <- c(Sys.info(),
                            extraction.date = as.character(Sys.Date()))
# save(siabox.extraction.meta, file = "data/siabox.extraction.meta.rda")

# @ sia_parametro ----
sia_parametro <- trae_tabla("parametro")
# save(sia_parametro, file = "data/sia_parametro.rda")

# @ sia_matriz ----
sia_matriz <- trae_tabla("matriz")
# save(sia_matriz, file = "data/sia_matriz.rda")

# @ sia_param_unidad ----
sia_param_unidad <- trae_tabla("param_unidad")
# save(sia_param_unidad, file = "data/sia_param_unidad.rda")

# APUNTES:
# Tiene algunas diferencias con sia_param_unidad que tenía yo grabada. Estas
# diferencias pueden ver con anti_join: 21 casos presentes en db_ pero no en
# sia_, y 10 casos presentes en sia_ que no están en db_.
#
# anti_join(db_param_unidad, sia_param_unidad) # 21 filas
# anti_join(sia_param_unidad, db_param_unidad) # 10 filas

# @ sia_unidad ----
sia_unidad <- trae_tabla("unidad")
# save(sia_unidad, file = "data/sia_unidad.rda")

# @ sia_programa ----
sia_programa <-
  trae_tabla("programa") %>%
  arrange(id_programa)
# save(sia_programa, file = "data/sia_programa.rda")

# @ sia_departamento ----
sia_departamento <- trae_tabla("departamento")
# save(sia_departamento, file = "data/sia_departamento.rda")

# @ sia_estacion ----
sia_estacion <- trae_tabla("estacion")
# save(sia_estacion, file = "data/sia_estacion.rda")

# @ sia_tipo_punto_estacion ----
sia_tipo_punto_estacion <- trae_tabla("tipo_punto_estacion")
# save(sia_tipo_punto_estacion, file = "data/sia_tipo_punto_estacion.rda")

# @ sia_institucion ----
sia_institucion <- trae_tabla("institucion")
# save(sia_institucion, file = "data/sia_institucion.rda")

# @ sia_programa_parametro ----
sia_programa_parametro <- trae_tabla("programa_parametro")
# save(sia_programa_parametro, file = "data/sia_programa_parametro.rda")

# @ usuarios ----
usuarios <-
  dplyr::tbl(con, "muestra") %>%
  dplyr::distinct(usuario) %>%
  dplyr::arrange(1L) %>%
  dplyr::collect()
# save(usuarios, file = "data/usuarios.rda")

# @ sia_muestra ----
sia_muestra <- trae_tabla("muestra")
# save(sia_muestra, file = "data/sia_muestra.rda")

# @ sia_datos_muestra_parametros -----
sia_datos_muestra_parametros <- trae_tabla("datos_muestra_parametros")
# save(sia_datos_muestra_parametros,
#      file = "data/sia_datos_muestra_parametros.rda")

# @ sia_sub_cuenca -----
sia_sub_cuenca <- trae_tabla("sub_cuenca")
# save(sia_sub_cuenca, file = "data/sia_sub_cuenca.rda")

# @ sia_cuenca -----
sia_cuenca <- trae_tabla("cuenca")
# save(sia_cuenca, file = "data/sia_cuenca.rda")
