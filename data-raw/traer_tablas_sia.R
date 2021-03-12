# INFAMBIENTAL ----
#
# Para que no cambien los datos, sería necesario hacer esto desde mi computadora
# perosnal. JMB.
drv <- DBI::dbDriver("PostgreSQL")
con <- RPostgreSQL::dbConnect(drv, dbname = 'infambientalbd',
                              host = 'localhost', port = 5432, user = 'juan',
                              password = 'shiny')
# pw <- {
#   "shiny_passwd"
# }
#
# nodo <- Sys.info()["nodename"]
# prueba <- grepl("DINAMA", nodo, ignore.case = TRUE)
#
# if (prueba) {
#   con <- RPostgreSQL::dbConnect(drv, dbname = "infambientalbd",
#                                 host = "172.20.0.34", port = 5432,
#                                 user = "shiny_usr", password = pw)
#   # Para trabajar en la máquina windows de la oficina, en donde ya sé que la
#   # codificación de caracteres es WIN1252 (en verdad Sys.getlocale() dice
#   # "Spanish_Uruguay.1252", pero asumo que es lo mismo y en las pruebas que hice
#   # anduvo bien):
#   if (grepl("DINAMA-OAN11", nodo, ignore.case = TRUE)) {
#     # dbExecute(con, "SET CLIENT_ENCODING TO 'WIN1252';")
#     RPostgreSQL::dbExecute(con, "SET NAMES 'WIN1252';")
#   }
#
# } else {
#   # En caso de estar en una computadora sin conexión a la base de datos, se usa
#   # un respaldo de las tablas importantes.
#   # load('data/liteTablas.RData')
#   con <- RPostgreSQL::dbConnect(drv, dbname = 'infambientalbd',
#                                 host = 'localhost', port = 5432, user = 'juan',
#                                 password = 'shiny')
#
# }

# @ sia_parametro ----
# sia_parametro <- dplyr::tbl(con, "parametro")
# if (exists("todos_param")) {
#   if (!todos_param) {
#     muestras_param <-
#       dplyr::tbl(con, "datos_muestra_parametros") %>%
#       dplyr::select(id_parametro, valor_minimo_str,
#              limite_deteccion, limite_cuantificacion) %>%
#       dplyr::collect() %>%
#       valores_numericos(metodo = "simple", filtrar_no_num = TRUE) %>%
#       dplyr::count(id_parametro)
#
#     sia_parametro <-
#       sia_parametro %>%
#       dplyr::filter(id_parametro %in% !!muestras_param$id_parametro)
#   }
# }
# sia_parametro <- dplyr::collect(sia_parametro)

# @ sia_matriz ----
# sia_matriz <- dplyr::collect(dplyr::tbl(con, "matriz"))

# @ sia_param_unidad ----
#
# Tiene algunas diferencias con sia_param_unidad que tenía yo grabada. Estas
# diferencias pueden ver con anti_join: 21 casos presentes en db_ pero no en
# sia_, y 10 casos presentes en sia_ que no están en db_.
#
# anti_join(db_param_unidad, sia_param_unidad) # 21 filas
# anti_join(sia_param_unidad, db_param_unidad) # 10 filas
#
# sia_param_unidad <- dplyr::collect(dplyr::tbl(con, "param_unidad"))

# @ sia_unidad ----
#
# anti_join(db_unidad, sia_unidad)
# anti_join(sia_unidad, db_unidad) # 2 filas
# sia_unidad <- dplyr::collect(dplyr::tbl(con, "unidad"))

# @ sia_programa ----
# sia_programa <-
#   dplyr::tbl(con, "programa") %>%
#   dplyr::arrange(id_programa) %>%
#   dplyr::collect()

# @ sia_departamento ----
# sia_departamento <- dplyr::collect(dplyr::tbl(con, "departamento"))

# @ sia_estacion ----
# sia_estacion <-
#   dplyr::tbl(con, "estacion") %>%
#   dplyr::arrange(id) %>%
#   dplyr::collect()

# @ sia_tipo_punto_estacion ----
# sia_tipo_punto_estacion <-
#   dplyr::tbl(con, "tipo_punto_estacion") %>%
#   dplyr::arrange(id) %>%
#   dplyr::collect()

# @ sia_institucion ----
# sia_institucion <- dplyr::collect(dplyr::tbl(con, "institucion"))

# @ sia_programa_parametro ----
# sia_programa_parametro <- dplyr::collect(dplyr::tbl(con, "programa_parametro"))

# @ usuarios ----
# usuarios <-
#   dplyr::tbl(con, "muestra") %>%
#   dplyr::distinct(usuario) %>%
#   dplyr::arrange(1L) %>%
#   dplyr::collect()

# @ sia_muestra ----
# sia_muestra <- dplyr::collect(dplyr::tbl(con, "muestra"))

# @ sia_datos_muestra_parametros -----
sia_datos_muestra_parametros <-
  dplyr::collect(dplyr::tbl(con, "datos_muestra_parametros"))
save(sia_datos_muestra_parametros, file="data/sia_datos_muestra_parametros.rda")

# @ sia_sub_cuenca -----
# sia_sub_cuenca <- dplyr::collect(dplyr::tbl(con, "sub_cuenca"))

# @ sia_cuenca -----
# sia_cuenca <- dplyr::collect(dplyr::tbl(con, "cuenca"))

# @ t_eti_base ----
# t_eti_base <-
#   sia_parametro %>%
#   dplyr::left_join(dplyr::select(codigos_param, id_parametro, codigo_nuevo),
#                    by = "id_parametro") %>%
#   dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
#                                        nombre_clave, codigo_nuevo)) %>%
#   dplyr::left_join(sia_param_unidad, by = "id_parametro") %>%
#   dplyr::filter(id_matriz == 6L) %>%
#   dplyr::left_join(sia_unidad, by = c("id_unidad_medida" = "id")) %>%
#   dplyr::transmute(id_parametro,
#                    etiqueta = stringi::stri_escape_unicode(
#                      paste0(param, " (", uni_nombre, ")"))
#   )
# save(t_eti_base, file = 'data/t_eti_base.rda')

# @ datos_sia ----
cp <- codigos_param %>%
  dplyr::filter(!is.na(id_parametro)) %>%
  dplyr::select(grupo, codigo_nuevo, id_parametro)

datos_sia <-
  consulta_muestras(con, id_matriz = 6L) %>%
  valores_numericos(metodo = "informe", filtrar_no_num = TRUE) %>%
  dplyr::left_join(cp, by = "id_parametro") %>%
  dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
                                       nombre_clave,
                                       codigo_nuevo),
                anio = as.integer(anio),
                mes = as.integer(mes)) %>%
  dplyr::left_join(
    dplyr::select(cuencas_informes, nombre_subcuenca_informes, id_estacion),
    by = "id_estacion") %>%
  dplyr::select(-id_estado)

save(datos_sia, file = "data/datos_sia.rda")

# @ datos_sia_sed ----
datos_sia_sed <-
  consulta_muestras(con, id_matriz = 11L) %>%
  valores_numericos(metodo = "informe", filtrar_no_num = TRUE) %>%
  dplyr::left_join(cp, by = "id_parametro") %>%
  dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
                                       nombre_clave,
                                       codigo_nuevo),
                anio = as.integer(anio), mes = as.integer(mes)) %>%
  dplyr::left_join(
    dplyr::select(cuencas_informes, nombre_subcuenca_informes, id_estacion),
    by = "id_estacion") %>%
  dplyr::select(-id_estado)

save(datos_sia_sed, file = "data/datos_sia_sed.rda")
