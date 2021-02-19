## INFAMBIENTALBD ----

## . sia_muestra -----

#' Inventario de muestras de infambientalbd
#'
#' Esta tabla guarda los metadatos generales de cada muestra. Una muestra se
#' corresponde con el muestreo en campo en un sitio en particular (estación) y
#' una fecha determinada.
#'
#' Fecha de extraccion: 2020-10-29
#'
#' @format Tabla con `r nrow(sia_muestra)` filas y `r ncol(sia_muestra)`
#'   columnas:
#'
#'   \describe{
#'
#'   \item{id_muestra}{integer. Número único identificador de la muestra}
#'
#'   \item{id_institución}{integer. Identificador de la institución a la que
#'   pertenece el usuario que cargó los datos en el SIA}
#'
#'   \item{periodo}{character. Texto en formato MES AÑO (ej.: JUNIO 2012). No
#'   necesariamente se corresponde con fecha_muestra, ya que se trata de un
#'   campo de identificación de la campaña de muestreo}
#'
#'   \item{fecha_muestra}{date. Fecha en que se tomó la muestra.}
#'
#'   \item{hora_muestra}{character. Hora en que se tomó la muestra (en el
#'   campo).}
#'
#'   \item{usuario}{character. Usuario que ingresó los datos al SIA (no
#'   necesariamente corresponde con quien/es lo colectaron en campo)}
#'
#'   \item{fecha_ingreso}{datetime. Fecha y hora en que se ingresó la muestra al
#'   SIA.}
#'
#'   \item{observaciones}{character. Observaciones.}
#'
#'   \item{id_estacion}{integer. Identificador de la estación de monitoreo.}
#'
#'   \item{replica}{logical. ..completar..}
#'
#'   \item{nro_muestra}{integer. Número de muestra asignado por SILAD. No es
#'   único para esta tabla (no confundir con id_muestra).}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_muestra"

## . sia_datos_muestra_parametros -----

#' Tabla con datos de infambientalbd
#'
#' Esta tabla guarda los datos de cada muestra. Cada fila representa una
#' combinación única de parámetro, fecha, hora y sitio (estación).
#'
#' Fecha de extraccion: 2020-10-29.
#'
#' @section Atencion: La tabla puede tener datos repetidos. La función
#'   \code{\link{consulta_muestras}} se encarga de descartar repeticiones.
#'
#' @section Valores: Los valores almacenados en la tabla no necesariamente
#'   expresan valores numéricos. Las funciones \code{\link{clasif_tipo_dato}} y
#'
#' @format Tabla con `r nrow(sia_datos_muestra_parametros)` filas y
#'   `r ncol(sia_datos_muestra_parametros)` columnas:
#'
#'   \describe{
#'
#'   \item{id_muestra}{integer. Identificador de la muestra (ver sia_muestra)}
#'
#'   \item{id_parametro}{integer. Identificador del parámetro.}
#'
#'   \item{observacion}{character. Observaciones.}
#'
#'   \item{valor_minimo}{numeric. ..completar..}
#'
#'   \item{valor_maximo}{numeric. ..completar..}
#'
#'   \item{tipo_medicion}{integer ..completar..}
#'
#'   \item{id}{integer. Identificador único de cada entrada de la tabla.}
#'
#'   \item{id_estado}{integer. identificador que puede tomar 3 valores: 1.
#'   pendiente, 2. original, 3. aprobado}
#'
#'   \item{fecha_resolucion}{datetime. ..completar..}
#'
#'   \item{valor_minimo_str}{character. Valor ingresado para la muestra del
#'   parámetro. No necesariamente es una expresión numérica. Ver detalles.}
#'
#'   \item{valor_maximo_str}{character. ..completar..}
#'
#'   \item{ult_mod}{..completar..}
#'
#'   \item{ult_usu}{..completar..}
#'
#'   \item{limite_deteccion}{character. Límite de detección.}
#'
#'   \item{limite_cuantificacion}{character. Límite de cuantificación}
#'
#'   \item{id_equipo}{integer. ..completar..}
#'
#'   \item{usuario}{character. Usuario que ingresó los datos al SIA (no
#'   necesariamente corresponde con quien/es lo colectaron en campo)}
#'
#'   }
#'
#' @seealso \code{\link{consulta_muestras}}
#'
#' @source \code{infambientalbd}
"sia_datos_muestra_parametros"

## . sia_departamento ----

#' Lista de departamentos
#'
#' La tabla de departamentos tiene los 19 departamentos de Uruguay y 2 entradas
#' extra: "DATO MIGRADO" y "---" (codigos "XX" y "NN" respectivamente). Las
#' estaciones del programa de monitoreo "Laguna Garzón" presentan esta categoría
#' en la columna "departamento" (Se encuentran entre Maldonado y Rocha).
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_departamento)` filas y
#'   `r ncol(sia_departamento)` columnas:
#'
#'   \describe{
#'
#'   \item{id}{}
#'
#'   \item{dep_nombre}{Nombre del departamento}
#'
#'   \item{dep_codigo}{Código o abreviación del nombre del departamento}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_departamento"

## . sia_estacion ----

#' Estaciones de monitoreo
#'
#' Es la lista completa de estaciones de monitoreo para las matrices Aguas
#' superficiales y Sedimentos. Incluye las columnas \code{prog_monitoreo} y
#' \code{matriz_estacion}, las cuales son la razón por las cuales las estaciones
#' son usadas como referencia para adivinar si un conjunto de datos pertenece a
#' una determinada matriz y programa. También se usa para determinar la matriz
#' de cada programa de monitoreo.
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_estacion)` filas y `r ncol(sia_estacion)`
#'   columnas:
#'
#'   \describe{
#'
#'   \item{codigo_pto}{Código de letras y números para cada estación}
#'
#'   \item{estacion}{Nombre de la estación (largo; puede ser una descripción de
#'   cómo llegar, etc...)}
#'
#'   \item{latitud}{}
#'
#'   \item{longitud}{}
#'
#'   \item{gid}{Identificador único (número entero) para el contexto de trabajo
#'   con GIS}
#'
#'   \item{id_playa}{..completar..}
#'
#'   \item{prog_monitoreo}{Identificador único (id) del programa de monitoreo
#'   correspondiente a cada estación.}
#'
#'   \item{id}{Identificador único (número entero) de la estación.}
#'
#'   \item{version}{..completar..}
#'
#'   \item{tipo_punto_id}{Número que indica el id del tipo de muestras tomadas:
#'   superficie o fondo (1 o 2 respectivamente)}
#'
#'   \item{estacion_asociada_id}{..completar..}
#'
#'   \item{departamento}{Número que indica el id del departamento en el que se
#'   encuentra la estación}
#'
#'   \item{sub_cuenca}{Número que indica el id de la subcuenca en el que se
#'   encuentra la estación}
#'
#'   \item{orden_ingreso}{..completar..}
#'
#'   \item{ingreso_interno}{..completar..}
#'
#'   \item{matriz_estacion}{Número que indica el id de la matriz asociada a la
#'   estación (de momento: 6 = Aguas superficiales y 11 = Sedimentos;
#'   15/10/2020)}
#'
#'   \item{estacion_activa}{..completar..}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_estacion"

## . sia_institucion ----

#' Lista de instituciones
#'
#' Instituciones asociadas a los usuarios que suben los datos al SIA.
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_institucion)` filas y `r ncol(sia_institucion)`
#'   columnas:
#'
#'   \describe{
#'
#'   \item{id_institucion}{Número único (natural) para cada institución.}
#'
#'   \item{nombre}{Nombre de la institución (texto).}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_institucion"

## . sia_matriz ----

#' Lista de matrices ambientales
#'
#' La tabla tiene los nombres y abreviaciones de las matrices ambientales
#' definidas e infambientalbd, así como el id único de cada clase.
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_matriz)` filas y `r ncol(sia_matriz)` columnas:
#'
#' \describe{
#'
#'   \item{id_matriz}{..completar..}
#'
#'   \item{nombre}{Nombre de la matriz}
#'
#'   \item{codigo}{Código o nombre corto de la matriz}
#'
#'   \item{vigente}{..completar..}
#'
#'   \item{fca_habilitado}{..completar..}
#'
#' }
#'
#' @source \code{infambientalbd}
"sia_matriz"

## . sia_param_unidad ----

#' Lista de correspondencias entre parámetros y unidades
#'
#' Tabla que vincula cada parámetro con su unidad correspondiente, según la
#' matriz ambiental. Necesaria para conectar \code{\link{sia_parametro}} con
#' \code{\link{sia_unidad}}
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_param_unidad)` filas y
#'   `r ncol(sia_param_unidad)` columnas:
#'
#'   \describe{
#'
#'   \item{id}{identificador único para las entradas de esta tabla (i.e.:
#'   combinaciones de `id_unidad_medida`, `id_parametro` e `id_matriz`)}
#'
#'   \item{id_unidad_medida}{Número que indica el id de la unidad de medida
#'   correspondiente}
#'
#'   \item{id_parametro}{Número que indica el id del parámetro correspondiente}
#'
#'   \item{id_matriz}{Número que indica el id de la matriz ambiental
#'   correspondiente}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_param_unidad"

## . sia_parametro ----

#' Lista de parámetros de SIA
#'
#' Tabla con todo los parámetros contemplados por la base de datos
#' infambientalbd. Para conectarla con \code{\link{sia_unidad}}, es necesaria la
#' tabla \code{\link{sia_param_unidad}}.
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_parametro)` filas y `r ncol(sia_parametro)`
#'   columnas:
#'
#'   \describe{
#'
#'   \item{id_parametro}{id: número natural único para cada fila}
#'
#'   \item{parametro}{Nombre (largo) del parámetro. Puede ser una frase corta.
#'   Texto.}
#'
#'   \item{enumerado}{..completar..}
#'
#'   \item{nombre_clave}{Nombre corto o código creado para cada parámetro
#'   (debería ser único). Texto.}
#'
#'   \item{decimales}{..completar..}
#'
#'   \item{par_vigente}{..completar..}
#'
#'   \item{codigo_airviro}{..completar..}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_parametro"

## . sia_programa ----

#' Lista de programas de monitoreo
#'
#' Los programas de monitoreo registrados en el SIA (base de datos
#' infambientalbd). Corresponden a las matrices ambientales "Aguas
#' superficiales" y "Sedimentos"
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_programa)` filas y `r ncol(sia_programa)`
#'   columnas:
#'
#'   \describe{
#'
#'   \item{id_programa}{\code{integer}. Número único para cada programa.}
#'
#'   \item{nombre_programa}{Nombre (largo) del programa de monitoreo}
#'
#'   \item{codigo_programa}{Código (corto) que identifica a cada programa}
#'
#'   \item{visible_externos}{..completar..}
#'
#'   \item{version}{..completar..}
#'
#'   \item{id_programa_silad}{..completar..}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_programa"

##' sia_programa_parametro ----

#' Relación entre programas y parametros
#'
#' Tabla que (intenta) registrar los parametros que son monitoreados en cada
#' programa.
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_programa_parametro)` filas y
#'   `r ncol(sia_programa_parametro)` columnas:
#'
#'   \describe{
#'
#'   \item{id}{Identificador único para las entradas de esta tabla}
#'
#'   \item{id_programa}{Identificador de programa}
#'
#'   \item{id_parametro}{Identificador de parametro}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_programa_parametro"

## . sia_tipo_punto_estacion ----

#' Tipo de estacion
#'
#' Tipos de estaciones de monitoreo, según (la profundidad de) las muestras que
#' se toman
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_tipo_punto_estacion)` filas y
#'   `r ncol(sia_tipo_punto_estacion)` columnas:
#'
#'   \describe{
#'
#'   \item{id}{Identificador único para las entradas de esta tabla}
#'
#'   \item{tip_pun_est_descripcion}{Descripción del tipo de punto ("SUPERFICIE"
#'   o "FONDO")}
#'
#'   \item{tip_pun_est_codigo}{Identificador único (númerico) del tipo de punto:
#'   superficie o fondo (1 o 2 respectivamente)}
#'
#'   }
#'
#' @source \code{infambientalbd}
"sia_tipo_punto_estacion"

## . sia_unidad ----

#' Lista de las unidades de medidas contempladas por el SIA
#'
#' La tabla oficial de unidades de medida registradas en el SIA. Para vincular
#' las unidades con \code{\link{sia_parametro}} es necesaria la tabla
#' \code{\link{sia_param_unidad}}
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con `r nrow(sia_unidad)` filas y `r ncol(sia_unidad)` columnas:
#'
#'   \describe{
#'
#'   \item{id}{integer. Identificador único de cada unidad de medida.}
#'
#'   \item{uni_nombre}{Texto para cada unidad de medida (ej: "mg CaCO3/L")}
#'
#'   \item{uni_factor_conversion}{..completar..}
#'
#' }
#'
#' @source \code{infambientalbd}
"sia_unidad"

## AGREGADAS ----

# . cuencas_informes -----

#' Cuencas y subcuencas en informes
#'
#' @seealso \code{\link{sia_estacion}}
#'
#' @format Tabla con `r nrow(cuencas_informes)` filas y
#' `r ncol(cuencas_informes)` columnas:
#'
#'   \describe{
#'
#'   \item{nombre_subcuenca_informes}{}
#'
#'   \item{codigo_pto}{}
#'
#'   \item{codigo_pto_mod}{}
#'
#'   \item{id_estacion}{}
#'
#'   \item{id_cuenca}{}
#'
#'   \item{cue_nombre}{}
#'
#'   \item{id_programa}{}
#'
#'   \item{nombre_programa}{}
#'
#'   }
#'
"cuencas_informes"

# . usuarios -----

#' Tabla con lista de usuarios
#'
#' La lista de usuarios que han cargado datos de muestras a las tablas del SIA.
#'
#' @seealso \code{\link{sia_datos_muestra_parametros}}
#'
#' @format Tabla con `r nrow(usuarios)` filas y
#' `r ncol(usuarios)` columnas:
#'
#'   \describe{
#'
#'   \item{usuario}{character. Nombre de usuario}
#'
#'   }
#'
"usuarios"

# . programa_matriz ----

#' Matrices de programas
#'
#' Creada a partir de la tabla \code{\link{sia_estacion}}. Sirve de referencia
#' para asociar programas de monitoreo con matrices ambientales.
#'
#' @seealso \code{\link{sia_estacion}}
#'
#' @format Tabla con `r nrow(programa_matriz)` filas y `r ncol(programa_matriz)`
#'   columnas:
#'
#'   \describe{
#'
#'   \item{id_parametro}{integer. Identificador único de cada programa de
#'   monitoreo.}
#'
#'   \item{id_matriz}{Número que indica el id de la matriz asociada al programa
#'   de monitoreo (de momento: 6 = Aguas superficiales y 11 = Sedimentos;
#'   15/10/2020)}
#'
#'   }
#'
#' @source \code{infambientalbd}
#'
#' @examples
#' # Crear una versión actualizada de la tabla:
#' programa_matriz <-
#'   sia_estacion %>%
#'     dplyr::distinct(prog_monitoreo, matriz_estacion) %>%
#'     setNames(c("id_programa", "id_matriz")) %>%
#'     dplyr::filter_all(~ !is.na(.))
"programa_matriz"

## . t_eti_base ----

#' Tabla de etiquetas base
#'
#' Tabla de base para hacer etiquetas para gráficos u otros usos,
#' basándose en las tablas del SIA (infambientalbd).
#'
#' Fecha de creación: 2020-10-29
#'
#' @format Tabla con `r nrow(t_eti_base)` filas y `r ncol(t_eti_base)` columnas:
#'
#'   \describe{
#'
#'   \item{id_parametro}{integer. Identificador único de cada parámetro.}
#'
#'   \item{etiqueta}{character. Texto de etiqueta para gráficos, etc...}
#'
#' }
#'
#' @source \code{infambientalbd}
#'
#' @examples
#' # Código de creación:
#' t_eti_base <-
#'   sia_parametro %>%
#'   dplyr::left_join(dplyr::select(codigos_param, id_parametro, codigo_nuevo),
#'                    by = "id_parametro") %>%
#'   dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
#'                                        nombre_clave, codigo_nuevo)) %>%
#'   dplyr::left_join(sia_param_unidad, by = "id_parametro") %>%
#'   dplyr::filter(id_matriz == 6L) %>%
#'   dplyr::left_join(sia_unidad, by = c("id_unidad_medida" = "id")) %>%
#'   dplyr::transmute(id_parametro,
#'                    etiqueta = paste0(param, " (", uni_nombre, ")"))
"t_eti_base"

## . codigos_param ----

#' Tabla con códigos alternativos para parámetros
#'
#' Presenta códigos nuevos para parámetros, creados por Elena Rodó y Amelia
#' Fabre.
#'
#' Última modificación: 2020-09-03
#'
#' @format Tabla con `r nrow(codigos_param)` filas y `r ncol(codigos_param)`
#'   columnas:
#'
#'   \describe{
#'
#'   \item{grupo}{character. Grupo al que pertenece el parámetro}
#'
#'   \item{parametro}{character. Nombre (largo) del parámetro}
#'
#'   \item{codigo_anterior}{character. Nombre (largo) del parámetro, encontrado
#'   anteriormente}
#'
#'   \item{codigo_nuevo}{character. Abreviación nueva propuesta para el
#'   parámetro}
#'
#'   \item{codigo_nuevo_jm}{character. Abreviación nueva propuesta para el
#'   parámetro, sugerencias de Juan M. Barreneche}
#'
#'   \item{obs}{character. Observaciones}
#'
#'   \item{id_parametro}{integer. Identificador único de cada parámetro.}
#'
#'   }
#'
#' @source \code{infambientalbd}
"codigos_param"


## . datos_sia ----

#' Datos extraídos del SIA.
#'
#' La tibble `datos_sia` tiene datos de matriz Aguas superficiales y
#' `datos_sia_sed` de Sedimentos. Fecha de extracción: 2020-11-04.
#'
#' Código de creación de las tablas:
#'
#' ```
#' cp <- codigos_param %>%
#'   dplyr::filter(!is.na(id_parametro)) %>%
#'   dplyr::select(grupo, codigo_nuevo, id_parametro)
#' 
#' datos_sia <-
#'   consulta_muestras(con, id_matriz = 6L) %>%
#'   valores_numericos(metodo = "informe", filtrar_no_num = TRUE) %>%
#'   dplyr::left_join(cp, by = "id_parametro") %>%
#'   dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo),
#'                                        nombre_clave,
#'                                        codigo_nuevo),
#'                 anio = as.integer(anio),
#'                 mes = as.integer(mes)) %>%
#'   dplyr::left_join(
#'     dplyr::select(cuencas_informes, nombre_subcuenca_informes, id_estacion),
#'     by = "id_estacion") %>%
#'   dplyr::select(-id_estado)
#' 
#' save(datos_sia, file = "data/datos_sia.rda")
#' 
#' datos_sia_sed <- 
#'   consulta_muestras(con, id_matriz = 11L) %>%
#'   valores_numericos(metodo = "informe", filtrar_no_num = TRUE) %>%
#'   dplyr::left_join(cp, by = "id_parametro") %>% 
#'   dplyr::mutate(param = dplyr::if_else(is.na(codigo_nuevo), 
#'                                        nombre_clave, 
#'                                        codigo_nuevo), 
#'                 anio = as.integer(anio), mes = as.integer(mes)) %>%
#'   dplyr::left_join(
#'     dplyr::select(cuencas_informes, nombre_subcuenca_informes, id_estacion),
#'     by = "id_estacion") %>%
#'   dplyr::select(-id_estado)
#' 
#' save(datos_sia_sed, file = "data/datos_sia_sed.rda")
#' ```
#'
#' @format Tabla con `r nrow(datos_sia)` filas y `r ncol(datos_sia)` columnas:
#'
#'   \describe{
#'
#'   \item{id_muestra}{Número único que identifica cada muestra ingresada al
#'   banco de datos del SIA.}
#'
#'   \item{nro_muestra}{Número único que identifica cada muestra ingresada al
#'   banco de datos del SILAD.}
#'
#'   \item{nombre_programa}{Nombre del programa de monitoreo al que corresponde
#'   la muestra.}
#'
#'   \item{id_programa}{Número único que identifica al programa de monitoreo.}
#'
#'   \item{cue_nombre}{Nombre de la cuenca en la que se encuentra la estación en
#'   donde se tomó la muestra.}
#'
#'   \item{id_cuenca}{Número único que identifica la cuenca en la que se
#'   encuentra la estación en donde se tomó la muestra.}
#'
#'   \item{sub_cue_nombre}{Nombre de la sub-cuenca en la que se encuentra la
#'   estación en donde se tomó la muestra.}
#'
#'   \item{id_sub_cuenca}{Número único que identifica la sub-cuenca en la que se
#'   encuentra la estación en donde se tomó la muestra.}
#'
#'   \item{codigo_pto}{Código que identifica la estación o punto de monitoreo,
#'   lugar en donde se tomó la muestra.}
#'
#'   \item{id_estacion}{Número único que identifica a cada estación.}
#'
#'   \item{tipo_punto_id}{Número único que identifica a cada tipo de punto.}
#'
#'   \item{tip_pun_est_descripcion}{Texto descriptivo de cada tipo de punto.}
#'
#'   \item{id_depto}{Número único que identifica a cada departamento.}
#'
#'   \item{departamento}{Nombre de cada departamento.}
#'
#'   \item{id_institucion}{Número único que identifica a cada institución.}
#'
#'   \item{institucion}{Nombre e cada institución.}
#'
#'   \item{usuario}{Nombre de usuario que ingresó la muestra a la base de
#'   datos.}
#'
#'   \item{periodo}{Título que identifica a cada campaña de muestreo; se compone
#'   del nombre del mes y el año (ej.: JUNIO 2019).}
#'
#'   \item{anio}{Año en que fue tomada la muestra.}
#'
#'   \item{mes}{Mes en que fue tomada la muestra.}
#'
#'   \item{anio_mes}{Año y mes en que fue tomada la muestra.}
#'
#'   \item{fecha_muestra}{Fecha del día en que fue tomada la muestra.}
#'
#'   \item{fecha_hora}{Hora en que fue tomada la muestra.}
#'
#'   \item{observaciones}{Observaciones correspondientes a cada muestra o
#'   valor.}
#'
#'   \item{id_matriz}{Número único que identifica a cada matriz ambiental.}
#'
#'   \item{id_parametro}{Número único que identifica a cada parámetro.}
#'
#'   \item{parametro}{Descripción extendida de cada parámetro.}
#'
#'   \item{nombre_clave}{Código textual único que identifica a cada parámetro.}
#'
#'   \item{id_unidad}{Número único que identifica a cada unidad de medida.}
#'
#'   \item{uni_nombre}{Texto que describe las unidades de medida (ej.: `mg
#'   NO2-N/L`).}
#'
#'   \item{valor_minimo_str}{Texto con el valor ingresado para el parámetro en
#'   cada muestra.}
#'
#'   \item{limite_deteccion}{Texto con el valor ingresado para el límite de
#'   detección del parámetro en cada muestra.}
#'
#'   \item{limite_cuantificacion}{Texto con el valor ingresado para el límite de
#'   cuantificación del parámetro en cada muestra.}
#'
#'   \item{valor}{numeric. Valor ingresado para cada parámetro, obtenido a
#'   partir del uso de la función \code{link{valores_numericos}} con la opción
#'   `metodo = "informe"`.}
#'
#'   \item{id_tipo_dato}{Es un número asignado a cada valor. Se trata de una
#'   clasificación de cada dato según siete categorías (ver
#'   \code{\link{tipos_de_dato}})}
#'
#'   \item{tipo_dato}{Descripción, en texto, de la categoría correspondiente a
#'   `id_tipo_dato`.}
#'
#'   \item{grupo}{Grupo al que pertenece el parámetro (ej.: Parámetros de
#'   Biológicos, Parámetros de Ecotoxicidad). No todos los parámetros tienen un
#'   grupo asignado.}
#'
#'   \item{codigo_nuevo}{Nombre corto del parámetro segun
#'   \code{\link{codigos_param}}}
#'
#'   \item{param}{Nombre corto del parámetro (mejorado a partir de
#'   \code{\link{codigos_param}}; en caso de parámetros que no figuran en esa
#'   tabla, se usa el `nombre_clave` de \code{\link{sia_parametro}})}
#'
#'   \item{nombre_subcuenca_informes}{Nombre dado a cada subcuenca dentro de los
#'   informes anuales de los programas de monitoreo.}
#'
#'   }
#'
#' @seealso \code{\link{tipos_de_dato}}, \code{\link{consulta_muestras}},
#'   \code{\link{codigos_param}}
#'
#' @source \code{infambientalbd}
#'
#' @aliases datos_sia, datos_sia_sed
"datos_sia"

## . tipos_de_dato -----

#' Tabla tipos de dato
#'
#' Esta tabla registra los tipos de dato encontrados en infambientalbd
#' (específicamente, en la columna `valor_minimo_str` de la tabla
#' \code{\link{sia_datos_muestra_parametros}}).
#'
#' Se espera que los tipos de dato (columna `tipo_dato`) se autoexpliquen. En
#' particular, las categorías `<X` y `>X` refieren a casos en los que se X es un
#' valor numérico, como "<2.0" o ">5000".
#'
#' @seealso \code{\link{clasif_tipo_dato}}
#'
#' @format Tabla con 7 filas y 2 columnas:
#'
#'   \describe{
#'
#'   \item{tipo_dato}{character. Nombre de las categorías (tipos) de datos.}
#'
#'   \item{id_tipo_dato}{integer. Id, número único entero que identifica a cada
#'   tipo de dato}
#'
#'   }
#'
#' @examples
#' # Para crear la tabla:
#' tipos_de_dato <- tibble::tibble(
#'   tipo_dato = c("NUMERICO", "<LD", "<LC", "LD<X<LC", "<X", ">X", "OTRO"),
#'   id_tipo_dato = 1:7
#' )
"tipos_de_dato"

## . decreto -----

#' Valores establecidos por el Decreto 253/79
#'
#' Tabla con valores mínimos y máximos establecidos por el
#' \href{https://www.impo.com.uy/bases/decretos/253-1979}{decreto 253/79}.
#'
#' Los valores de `id_parametro`` en esta tabla se asignaron por aproximación.
#'
#' ## id_metodo
#'
#' La columna `id_metodo` refiere a distintas formas de definir si se cumple o
#' no con el criterio establecido por el decreto. La mayoría de los casos
#' corresponden al método 1 (muestra única), y la mayoría de las variantes se
#' incluyen en función de los diferentes métodos establecidos para el parámetro
#' "Bacterias Coliformes" (asociado al TermoTMF, `id_parametro` = 2111 en
#' \code{\link{sia_parametro}}).
#'
#' La idea es que cada combinación única de `id_parametro` x `id_metodo`
#' funcione, en los hechos, como un parámetro diferenciado. Por ejemplo, las
#' Bacterias Coliformes medidas en su media geométrica (método 2) sería un
#' parámetro diferente a Bacterias Coliformes medidas según si en el 80 \% de
#' los caso está por debajo del límite establecido. Esto es consistente con el
#' criterio del SIA de separar parámetros según las unidades de medida asociadas
#' (ej.: TermoTMF y TermoTTM, corresponden ambos a Bacterias Termotolerantes
#' medidas en Membrana Filtrante y Tubos Múltiples, respectivamente).
#'
#' Las opciones de `id_metodo` son:
#'
#' \enumerate{
#'
#' \item{Medición simple: el valor establecido por el decreto es comparado con
#' una única muestra. Coliformes: se incluyen casos en los que el decreto
#' requiere al menos 5 medidas, determinando que ninguna de estas puede superar
#' el valor establecido. Ejemplo: en Coliformes en clase de aguas 1, ninguna
#' medida puede superar las 2000 ufc/100mL.}
#'
#' \item{Media geométrica de al menos 5 muestras}
#'
#' \item{80% de los valores, en al menos 5 muestras, deben estar por debajo de
#' este límite.}
#'
#' \item{En este caso, se deben sumar los valores de varios parámetros. Ejs.:
#' Endosulfán alfa y beta, Clordano-cis y Clordano-trans, y otros casos en que
#' se trata de isómeros conformacionales.}
#'
#' }
#'
#' NOTAR: Que hasta el momento (2020-10-30) los métodos de evaluación
#' desarrollados por las aplicaciones Shiny
#' \href{http://dinama-shiny:3838/sia_apps}{sia_apps} no están capacitados para
#' evaluar parámetros que no utilicen el método 1. Ampliar estas capacidades
#' puede implicar múltiples evaluaciones por parámetro, como determinar si hay
#' efectivamente al menos 5 muestras, para luego determinar si cumplen con una
#' media geométrica adecuada.
#'
#' ## Respecto al método 4:
#'
#' Casos simples como alfa y beta endosulfan o clordano cis y trans, que son
#' isómeros conformacionales y sería correcto sumarlos, cosiderando el valor
#' suma, para comparar con el valor de la normativa.
#'
#' El caso del endosulfan sulfato, es un producto de degradación del endosulfán
#' y creo que no es correcto sumarlo. Es similar al AMPA, que es producto de la
#' degradación del Glifosato. Si se van a sumar, se tendría que aclarar, ya que
#' químicamente es bastante "turbio" hacer una suma. También está el caso del
#' DDT que tiene dos isomeros, op y pp. Que también tiene sus productos de
#' degradación DDD y DDE (cuando pierde 1 y 2 Cloros) con sus respectivos
#' isomeros op y pp. En este caso, cada par de isomeros se tendría que sumar,
#' pero cada par por separado. En el caso del heptaclor y heptaclor epóxido, se
#' pide la suma (253/79), asi que estaría sumando el plaguicida y su producto de
#' degradación.
#'
#' Otro ejemplo es el Lindano (que es el gamma HCH), el HCH tiene 4 isómeros
#' mas, alpha, beta, delta y epsilon, que en la práctica, comúnmente se analizan
#' por separado y el mas importante es el Lindano, seguramente por su mayor
#' toxicidad o persistencia en el ambiente. Este es un caso en que sus isómeros
#' no se suman. Creo que por convención...
#'
#' Cada normativa puede tener criterios distintos, por ejemplo, se puede poner
#' un límite para el Aldrin y Dieldrin  sumados, y dejar al Endrin afuera. Otra,
#' sumar todos los isómeros de DDT, DDD y DDE...
#'
#' En resumen, los isómeros se suman, los productos de degradación no. Salvo que
#' se aclare en la normativa o exista una convención al respecto.
#'
#' @format Tabla con `r nrow(decreto)` filas y `r ncol(decreto)` columnas:
#'
#'   \describe{
#'
#'   \item{param_decreto}{`character`. Nombre del parámetro tal como figuran en
#'   el texto original del decreto, incluyendo las unidades de medida}
#'
#'   \item{id_parametro}{`integer`. Id, número único entero que identifica a
#'   cada parámetro}
#'
#'   \item{id_metodo}{`integer`. Id, número único entero que identifica a cada
#'   método (ver detalles)}
#'
#'   \item{coef_conversion}{`numeric`. Coeficiente de conversión para convertir
#'   `valor` a las unidades de medida establecidas por SIA para el parámetro,
#'   multiplicando `valor` x `coef_conversion`.}
#'
#'   \item{obs}{`character`. Observaciones}
#'
#'   \item{clase}{`character`. Clase de agua definida en el decreto 253/79.}
#'
#'   \item{extremo}{`character`. Determina si se trata de el mínimo o el máximo
#'   permitido para el parámetro.}
#'
#'   \item{valor}{`numeric`. Valor original para el límite establecido por el
#'   decreto.}
#'
#'   }
#'
#' @examples
#' # Al momento se usan solamente los casos en los que id_metodo == 1, por lo
#' # que típicamente al inicio de scripts se agregan estas líneas:
#' decreto <-
#'   dplyr::mutate(decreto, valor = valor * coef_conversion) %>%
#'   dplyr::filter(id_parametro != 2111L | id_metodo == 1L)
"decreto"


## . reglas ----

#' Reglas de comparación para pares de parámetros
#'
#' Esta lista almacena reglas (funciones) que sirven para evaluar datos del SIA.
#'
#' @format Lista con `r length(reglas)` elementos, cada uno con la siguiente
#'   estructura:
#'
#'   \describe{
#'
#'   \item{regla}{character. Texto que describe la regla. Ej.: "`PT (ug P/L)` >
#'   `PO4 (ug PO4-P/L)`"}
#'
#'   \item{mensaje}{character. Frase que se utiliza en caso de que **no** se
#'   cumpla la regla.}
#'
#'   \item{id_parametro}{integer. Vector con 2 o más id de parámetros}
#'
#'   \item{fun}{function. Función que acepta una \code{\link[base]{data.frame}}
#'   como único argumento, y en la que las columnas se corresponden con los
#'   parámetros correspondientes (en el mismo orden)}
#'
#'   }
#'
#' @examples
#' # Veamos las reglas relativas a OD vs SatO2:
#' reglas$od_sat_min
#' reglas$od_sat_max
#'
#' # Evaluemos esta regla para todos los datos de infambientalbd:
#' d <- datos_sia %>%
#'   dplyr::filter(id_matriz == 6L, id_parametro %in% c(2017L, 2021L)) %>%
#'   ancho %>%
#'   # El orden de los parámetros es importante!:
#'   dplyr::select(OD, SatO)
#'
#' d$od_alto <- !reglas$od_sat_max$fun(d)
#' d$od_bajo <- !reglas$od_sat_min$fun(d)
#' # Voila!:
#' dplyr::filter(d, !is.na(od_alto), od_alto | od_bajo)
"reglas"

#' Tabla con rangos de valores normales para parámetros
"lim"
