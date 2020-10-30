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
#' @format Tabla con 17749 filas y 11 columnas:
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
#' @format Tabla con 237957 filas y 17 columnas:
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
#' @format Tabla con 21 filas y 3 columnas:
#'
#' \describe{
#'
#'   \item{id}{}
#'
#'   \item{dep_nombre}{Nombre del departamento}
#'
#'   \item{dep_codigo}{Código o abreviación del nombre del departamento}
#'
#' }
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
#' @format Tabla con 433 filas y 17 columnas:
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
#' }
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
#' @format Tabla con 27 filas y 2 columnas:
#'
#' \describe{
#'
#'   \item{id_institucion}{Número único (natural) para cada institución.}
#'
#'   \item{nombre}{Nombre de la institución (texto).}
#'
#' }
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
#' @format Tabla de 15 filas y 5 columnas:
#'
#' \describe {
#'
#'   \item{id_matriz}{}
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
#' @format Tabla de 1321 filas y 4 columnas:
#'
#' \describe {
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
#' }
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
#' @format Tabla de 356 filas y 7 columnas:
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
#' }
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
#' @format Tabla de 32 filas y 6 columnas:
#'
#' \describe{
#'
#' \item{id_programa}{\code{integer}. Número único para cada programa.}
#'
#' \item{nombre_programa}{Nombre (largo) del programa de monitoreo}
#'
#' \item{codigo_programa}{Código (corto) que identifica a cada programa}
#'
#' \item{visible_externos}{..completar..}
#'
#' \item{version}{..completar..}
#'
#' \item{id_programa_silad}{..completar..}
#'
#' }
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
#' @format Tabla con 1458 filas y 3 columnas:
#'
#'   \describe{
#'
#'   \item{id}{Identificador único para las entradas de esta tabla}
#'
#'   \item{id_programa}{Identificador de programa}
#'
#'   \item{id_parametro}{Identificador de parametro}
#'
#' }
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
#' @format Tabla con 2 entradas y 3 columnas:
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
#' }
#'
#' @source \code{infambientalbd}
"sia_tipo_punto_estacion"

## . t_eti_base ----

#' Lista de las unidades de medidas contempladas por el SIA
#'
#' La tabla oficial de unidades de medida registradas en el SIA. Para vincular
#' las unidades con \code{\link{sia_parametro}} es necesaria la tabla
#' \code{\link{sia_param_unidad}}
#'
#' Fecha de extraccion: 2020-10-22
#'
#' @format Tabla con 1321 filas y 4 columnas:
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

## . t_eti_base ----

#' Tabla de etiquetas base
#'
#' Creada con \code{\link{make_t_eti_base()}}
#'
#' Fecha de creación: 2020-10-29
#'
#' @format Tabla con 302 filas y 2 columnas:
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
"t_eti_base"

## . codigos_param ----

#' Tabla con códigos alternativos para parámetros
#'
#' Presenta códigos nuevos para parámetros, creados por Elena Rodó y Amelia
#' Fabre.
#'
#' Última modificación: 2020-09-03
#'
#' @format Tabla con 255 filas y 7 columnas:
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

#' Tabla con datos pre extraídos
#'
#' Última modificación: 2020-10-15
#'
#' @format Tabla con 231702 filas y 38 columnas:
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
#'   \item{valor}{Valor ingresado para cada parámetro. Si el método elegido es
#'   `sin_cambios`, la columna será idéntica a ´valor_minimo_str´ (y, por lo
#'   tanto, texto). En caso contrario, será una columna de valores numéricos,
#'   transformados de a partir de `valor_minimo_str` y de forma acorde a lo
#'   especificado en "Método".}
#'
#'   \item{id_tipo_dato}{Es un número asignado a cada valor. Se trata de una
#'   clasificación de cada dato según siete categorías:
#'
#'   \enumerate{
#'
#'     1. `Numérico`
#'
#'     2. `<LD`
#'
#'     3. `<LC`
#'
#'     4. `LD<X<LC`
#'
#'     5. `<X`
#'
#'     6. `>X`
#'
#'     7. `Otros`
#'
#'   }
#'
#'   }
#'
#'   \item{tipo_dato}{Descripción, en texto, de la categoría correspondiente a
#'   `id_tipo_dato`.}
#'
#'   \item{nombre_subcuenca_informes}{Nombre dado a cada subcuenca dentro de los
#'   informes anuales de los programas de monitoreo.}
#'
#'   \item{grupo}{Grupo al que pertenece el parámetro (ej.: Parámetros de
#'   Biológicos, Parámetros de Ecotoxicidad). No todos los parámetros tienen un
#'   grupo asignado.}
#'
#'   \item{codigo_nuevo}{Nombre (abreviado, o código) nuevo propuesto para el
#'   parámetro (ver [tabla de códigos nuevos](codigos_param.html)). No todos los
#'   parámetros tienen un código nuevo propuesto.}
#'
#'   }
"datos_sia"
