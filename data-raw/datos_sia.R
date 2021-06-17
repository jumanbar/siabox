out <- 
  dplyr::rename(sia_datos_muestra_parametros, id_dato = id) %>% 
  dplyr::left_join(sia_parametro, 
                   by = 'id_parametro') %>% 
  dplyr::left_join(dplyr::select(sia_muestra, -usuario),
                   by = 'id_muestra') %>% 
  dplyr::left_join(sia_institucion,
                   by = 'id_institucion') %>% 
  dplyr::inner_join(dplyr::filter(sia_estacion, matriz_estacion == 6L),
                    by = c('id_estacion' = 'id')) %>% 
  dplyr::left_join(sia_tipo_punto_estacion,
                   by = c('tipo_punto_id' = 'id')) %>% 
  dplyr::left_join(sia_sub_cuenca,
                   by = c('sub_cuenca' = 'id')) %>% 
  dplyr::left_join(sia_cuenca,
                   by = c('sub_cue_cuenca_id' = 'id')) %>% 
  dplyr::left_join(sia_departamento,
                   by = c('departamento' = 'id')) %>% 
  dplyr::select(-version) %>% 
  dplyr::left_join(sia_programa,
                   by = c('prog_monitoreo' = 'id_programa')) %>% 
  dplyr::left_join(sia_param_unidad,
                   by = c('id_parametro', 'matriz_estacion' = 'id_matriz')) %>% 
  dplyr::left_join(sia_unidad,
                   by = c('id_unidad_medida' = 'id')) %>% 
  dplyr::transmute(
    id_dato, id_muestra, nro_muestra, id_estado, nombre_programa,
    id_programa = prog_monitoreo, cue_nombre, 
    id_cuenca = sub_cue_cuenca_id, sub_cue_nombre, 
    id_sub_cuenca = sub_cuenca, codigo_pto, id_estacion, 
    tipo_punto_id, tip_pun_est_descripcion, id_depto = departamento,
    departamento = dep_nombre, id_institucion, institucion = nombre,
    usuario, periodo, 
    anio = as.integer(lubridate::year(fecha_muestra)),
    mes = as.integer(lubridate::month(fecha_muestra)),
    anio_mes = paste0(anio, "_", stringr::str_pad(mes, pad = '0', width = 2)),
    fecha_muestra, fecha_hora = paste(fecha_muestra, hora_muestra),
    observaciones = paste0(observacion, '. ', observaciones),
    id_matriz = matriz_estacion, id_parametro, parametro,
    nombre_clave, id_unidad = id_unidad_medida, uni_nombre, 
    valor_minimo_str, limite_deteccion, limite_cuantificacion
  )

# Quitar datos repetidos según id_estado (1. pendientes, 2. original, 
# 3. aprobado):
if (any(out$id_estado == 3)) {
  repes <- out %>%
    dplyr::count(id_muestra, id_parametro) %>%
    dplyr::filter(n > 1)
  
  # A continuación: si es que hay repetidos, quedarme sólo con los que
  # figuran como aprobados...
  if (nrow(repes)) {
    for (i in 1:nrow(repes)) {
      w <- which(
        out$id_muestra == repes$id_muestra[i] &
          out$id_parametro == repes$id_parametro[i]
      )
      
      # id_estado = 1: pendiente
      # id_estado = 2: original
      # id_estado = 3: aprobado
      w_aprob <- which(out$id_estado[w] == 3)
      
      if (length(w_aprob)) {
        fila <- out[w,][w_aprob,]
        out <- out[-w,]
        out <- rbind(out, fila)
      }
    }
  }
}

# Casos en los que hay más de un dato para un id_muestra e id_parametro
# (siempre con TermoTMF, hasta el momento, 2020-07-29):
repes <- out %>% dplyr::count(id_muestra, id_parametro) %>% dplyr::filter(n > 1)
if (nrow(repes)) {
  for (i in 1:nrow(repes)) {
    w <- which(
      out$id_muestra == repes$id_muestra[i] &
        out$id_parametro == repes$id_parametro[i]
    )
    w_ultimo <- which.max(out$id_dato[w])
    
    out <- out[-w[-w_ultimo],]
  }
}

out <- dplyr::select(out, -id_dato)

# codigos_param <- readr::read_delim(
#   "data-raw/codigos_param.csv", ";", escape_double = FALSE, 
#   col_types = readr::cols(id_parametro = readr::col_integer()), 
#   locale = readr::locale(decimal_mark = ",", grouping_mark = "."), 
#   trim_ws = TRUE)
# 
# # Habría que inventar la tabla grupos y el id_grupo con ella...:
# grupos <- as.factor(codigos_param$grupo)
# levels(grupos) <- c("Par\u00e1metros de Biol\u00f3gicos", 
#                     "Par\u00e1metros de Ecotoxicidad", 
#                     "Par\u00e1metros F\u00edsico-Qu\u00edmicos Generales", 
#                     "Par\u00e1metros Inorg\u00e1nicos no Met\u00e1licos", 
#                     "Par\u00e1metros Met\u00e1licos", 
#                     "Par\u00e1metros Microbiol\u00f3gicos", 
#                     "Par\u00e1metros Org\u00e1nicos", 
#                     "Par\u00e1metros Org\u00e1nicos Generales", 
#                     "Par\u00e1metros Otros")
# 
# codigos_param$grupo <- as.character(grupos)

cp <- codigos_param %>%
  dplyr::filter(!is.na(id_parametro)) %>%
  dplyr::select(grupo, codigo_nuevo, id_parametro)

out <- out %>%
  siabox::valores_numericos(metodo = "informe", filtrar_no_num = TRUE) %>%
  dplyr::left_join(
    dplyr::select(cuencas_informes, nombre_subcuenca_informes, 
                  codigo_pto_mod, id_estacion),
    by = "id_estacion") %>%
  dplyr::left_join(cp, by = "id_parametro") %>%
  dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
                                       nombre_clave,
                                       codigo_nuevo),
                anio = as.integer(anio),
                mes = as.integer(mes)) %>%
  dplyr::select(-id_estado, -codigo_nuevo)

# dplyr::anti_join(
#   dplyr::select(out, id_muestra, id_parametro), 
#   dplyr::bind_rows(dplyr::select(datos_sia, id_muestra, id_parametro), 
#                    dplyr::select(datos_sia_sed, id_muestra, id_parametro)))
# 
# x <- dplyr::filter(out, id_muestra %in% c(3290616, 3290722, 3268834),
#               id_parametro == 304)

# PENDIENTE: HACER PRUEBAS CON ESTE NUEVO 'datos_sia' y 'datos_sia' sed, con 
# ejemplos, funciones, etc ....
# datos_sia     <- dplyr::filter(out, id_matriz == 6L)
# datos_sia_sed <- dplyr::filter(out, id_matriz == 11L)
# rm(out)
# 
# # Conteo original:
# > dplyr::count(out, id_matriz)
# # A tibble: 2 x 2
#   id_matriz      n
#       <int>  <int>
# 1         6 223112
# 2        11   3898

datos_sia <- out
# save(datos_sia, file = 'data/datos_sia.rda')
