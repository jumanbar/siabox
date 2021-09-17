# README ----
#
# (PARA USAR EN LA VIÑETA)
#
# Las funciones aquí sirven para extraer datos de INFAMBIENTAL (o sea, datos del
# SIA correspondiente a matrices de Aguas) y hacer algunas manipulaciones
# simples. Las funciones consulta_muestras, filtrar_datos y valores_numericos
# hacen lo mismo que cuando se extraen datos de iSIA, si es que no son las
# mismas funciones (ahora no me acuerdo si es el caso, casi seguro que deben
# haber, al menos, diferencias sutiles.)
#
# La basica es:
#
# 1. Importar datos con consulta_muestras, si estás trabajando con una conexión
# a la base de datos (ver ejemplos en consulta_muestras).
#
# 2. En caso de no tener conexión con la base de datos INFAMBIENTAL, usar los
# datos ya extraidos, contenidos en datos_sia. En ese caso conviene usar
# filtrar_datos u otro método, ya que en datos_sia hay **de todo**.
#
# 3. Usar valores_numericos, que le agrega una columna con valores numéricos a
# una tabla que contenga la columna valor_minimo_str (tal como en la tabla
# datos_sia). NOTA: por defecto datos_sia ya tiene columna con valores
# numéricos, obtenidos con métedo "informe".
#
# Tener en cuenta:
#
# Los datos están por defecto en formato "alto" (ie: hay una sola columna con
# datos numéricos, mientras que información como parámetro, fecha, programa,
# unidaes, etc, está en otras columnas), pero a veces los queremos en formato
# "ancho" (ie: varias columnas con datos numéricos; por ejemplo, una columna
# para el parámetro A, otra columna para el parámetro B, y todo así). R tiene
# funciones para ese tipo de manipulaciones, como stats::reshape o
# tidyr::pivot_wider, tidyr::pivot_longer. La función ancho usa estas últimas
# para facilitar al usuario de datos del SIA: toma datos (como datos_sia), **con
# una única matriz ambiental**, y los "ensancha", de forma que se obtiene una
# tabla con varias columnas tipo "Parámetro A", "Parámetro B" y todo así, que es
# el caso típico de uso para analizar estos datos.
#
# Nota: para referencia de formatos "alto" y "ancho", es útil mirar el esquema
# en esta pregunta de StackOverflow:
# https://es.stackoverflow.com/questions/357580/c%C3%B3mo-puedo-convertir-columnas-a-filas-y-filas-a-columnas-en-r-es-decir-conver.
#
# En todo momento las funciones *_id (archivo "helpers.R") están para ayudar:
# permiten encontrar el número que identifica parámetros, unidades, programas,
# etc., usando una búsqueda de texto aproximada (ej: `par_id("fosfo")`).
#
# De manera similar, la función unipar sirve para buscar rápidamente las
# unidades de medida oficiales para un parámetro (y matriz) dado, usando id o
# nombre clave.

# . . . . . . . . .  . . . . .  .  . . . . . . . . -----
#
# MANIPULAR DATOS -----

#' Formato ancho
#'
#' Ensanchar datos provenientes de consulta_muestras. Espera la presencia de
#' ciertas columnas y una sóla matriz ambiental. Ver detalles.
#'
#' @param .data Tabla de datos obtenida con \code{\link{consulta_muestras}},
#'   posiblemente modificada con valores_numericos y con columnas agregadas por
#'   \code{\link[dplyr:mutate-joins]{left_join}}.
#'
#' @param unidades TRUE o FALSE. Determina si se agregan las unidades a las
#'   columnas de los parámetros (ej.: 'SatO (%)' en lugar de 'SatO')
#'
#' @details Espera que existan las columnas \code{valor} y \code{param}. En caso
#'   de no encontrarlas las creará a partir de las columnas
#'   \code{valor_minimo_str} y \code{nombre_clave}, respectivamente, si es que
#'   están presentes. La primera contiene los valores de los parámetros
#'   muestreados, ya sea en formato numérico o en texto. La segunda debería
#'   contener el código del parámetro.
#'
#'   Para facilitar la compatibilidad, esta función además verifica la presencia
#'   de otras columnas agregadas en el código de la app iSIA (en el reactive
#'   \code{datos_extraccion}, del server.R): parametro, grupo, id_tipo_dato y
#'   tipo_dato.
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' d <- filtrar_datos(datos_sia,
#'                    id_programa = 10L,
#'                    rango_fechas = c("2019-01-01", "2020-12-31"),
#'                    id_parametro = c(2099, 2098)) %>%
#'   dplyr::select(codigo_pto, fecha_muestra, id_parametro, param, id_unidad,
#'                 uni_nombre, limite_deteccion, limite_cuantificacion, valor)
#' print(d)
#' ancho(d)
#'
#' # Cómo usar ancho en lugar de ancho_old:
#' datos_sia %>%
#'   # Primero filtrar para tener sólo 2 parámetros:
#'   dplyr::filter(id_programa == 4, id_parametro %in% c(2017, 2021)) %>%
#'   ancho %>%
#'   dplyr::select(SatO, OD) %>%
#'   plot
ancho <- function(.data, unidades = FALSE) {

  matrices <- unique(.data$id_matriz)
  if (length(matrices) > 1) {
    stop('Los datos tienen m\u00e1s de un valor de id_matriz: ',
         colapsar_secuencia(matrices),
         '. Sugerencia: filtrar datos con dplyr::filter o "["')
  }

  if (!any(names(.data) == 'valor')) {
    warning('Se cre\u00f3 autom\u00e1ticamente la columna: ',
            'valor = valor_minimo_str.')
    .data$valor <- .data$valor_minimo_str
  }

  if (!any(names(.data) == 'param')) {
    wnc <- which(names(.data) == 'nombre_clave')
    if (!length(wnc))
      stop('No se encontr\u00f3 columna con nombres de par\u00e1metros: ',
           '"param" o "nombre_clave"')
    warning('Se cre\u00f3 autom\u00e1ticamente la columna "param".')
    .data$param <- paste0(.data$nombre_clave, ' (', .data$uni_nombre, ')')
  }

  # PERO QUÉ HAGO CON LAS OBSERVACIONES??
  #
  # Doy por sentado que está id_muestra? o que está observaciones?

  if ('observaciones' %in% names(.data)) {
    obs <- dplyr::distinct(.data, id_muestra, nombre_clave, observaciones)

    if (nrow(obs) > length(unique(.data$id_muestra))) {
      .data <- obs %>%
        dplyr::filter(!is.na(observaciones), observaciones != "") %>%
        # Capaz que en vez de usar id_muestra se puede usar any_of y poner todas
        # las columnas que podrían servir luego en el pivot wider para ser
        # id_cols... de esta forma la función sería relativamente flexible...
        # aunque no sé si vale la pena.
        dplyr::group_by(id_muestra) %>%
        dplyr::summarise(OBS = paste(paste0(nombre_clave, ": ", observaciones),
                                     collapse = '. ')) %>%
        dplyr::left_join(.data, ., by = 'id_muestra') %>%
        dplyr::mutate(observaciones = OBS) %>%
        dplyr::select(-OBS)
    }
  }

  columnasok <- c("id_muestra", "nro_muestra", "nombre_programa", "id_programa",
                  "cue_nombre", "id_cuenca", "sub_cue_nombre", "id_sub_cuenca",
                  "codigo_pto", "id_estacion", "tipo_punto_id",
                  "tip_pun_est_descripcion", "id_depto", "departamento",
                  "id_institucion", "institucion",
                  # "usuario", # Eliminada en commit del 2/7/2021, porque
                  # las muestras no tienen porqué tener a un único usuario
                  # para todos los parámetros.
                  "periodo", "anio",
                  "mes", "anio_mes", "fecha_muestra", "fecha_hora",
                  "observaciones", "id_matriz", "nombre_subcuenca_informes",
                  "codigo_pto_mod", "param", "LD", "LC", "valor"
                  )

  out <- .data %>%
    dplyr::mutate(LD = limite_deteccion %>%
             stringr::str_trim() %>%
             stringr::str_replace_all("[.,]+", ".") %>%
             as.numeric(),
           LC = limite_cuantificacion %>%
             stringr::str_trim() %>%
             stringr::str_replace_all("[.,]+", ".") %>%
             as.numeric()) %>%
    dplyr::select(tidyselect::any_of(columnasok)) %>%
    tidyr::pivot_wider(names_from = param, values_from = c(valor, LD, LC))

  nombres <- names(out)
  if ("id_muestra" %in% nombres) out <- dplyr::arrange(out, id_muestra)

  m <- matrix(c(grep("^valor_", nombres),
                grep("^LD_", nombres),
                grep("^LC_", nombres)),
              ncol = 3)

  i <- as.vector(t(m))

  out <- out[c(1:(i[1] - 1), i)]

  out <- out %>%
    dplyr::rename_at(dplyr::vars(tidyselect::starts_with('valor_')),
                     ~ stringr::str_remove_all(., '^valor_')) %>%
    dplyr::rename_at(dplyr::vars(tidyselect::matches('^L[DC]')),
                     ~ stringr::str_replace_all(., '(L[CD])_(.*)', '\\2_\\1'))

  # param, values_from = c(valor, LD, LC))
  # Si están, eliminar estas columnas innecesearias (para compatibilidad con
  # server.R de iSIA: ver reactive 'datos_extraccion'):
  # w <- grep('^codigo_nuevo$|^parametro$|^grupo$|tipo_dato$', names(.data),
  #           ignore.case = TRUE)
  # if (length(w)) {
  #   warning('Se eliminan autom\u00e1ticamente las columnas: ',
  #           colapsar_secuencia(names(.data)[w]))
  #   .data <- .data[-w]
  # }
  w <- which(!(names(.data) %in% names(out)) &
               !(names(.data) %in% c('param', 'valor',
                                     'limite_deteccion',
                                     'limite_cuantificacion')))
  if (length(w)) {
    adv <- paste('Se eliminaron autom\u00e1ticamente las columnas:',
                 colapsar_secuencia(names(.data)[w])) %>%
      stringr::str_wrap(80, indent = 1)
    warning(adv)
  }

  return(out)
}

#' Asignar categorías a los datos SIA
#'
#' Evalúa vector character que expresan valores numéricos, según las categorías
#' de la tabla \code{\link{tipos_de_dato}}.
#'
#' @param x Character. En principio puede ser cualquier vector de tipo
#'   character, pero está pensado específicamente para los valores de la columna
#'   \code{valor_minimo_str} de la tabla \code{datos_muestra_parametros} de la
#'   base de datos infambientalbd (SIA).
#'
#' @param metodo String. El valor de este argumento define la forma en que se
#'   clasifican los valores de X. Opciones: "simple" o "informe". Ver detalles.
#'
#' @details Números con comas (en vez de puntos) como indicador de decimales,
#'   son considerados numéricos.
#'
#'   El reconocimiento de valores equivalentes a "<LD" y "<LC" se basa en los
#'   datos encontrados en la base infambientalbd, por lo que contempla casos
#'   como "LOQ", "LOD" o "ND" (Limit of Quantification, Limit of Detection y No
#'   Detectado, respectivamente).
#'
#' @return Lista con dos elementos:
#'
#'   \describe{
#'
#'   \item{valores}{Valores originales "limpios" (puntos en vez de comas para
#'   decimales y sin espacios en blanco al inicio o al final.)}
#'
#'   \item{tipos}{Vector integer (de misma longitud que \code{x}) con los id de
#'   los tipos de datos, tal como se pueden encontrar en la tabla
#'   \code{\link{tipos_de_dato}} (por ahora no está presente en el SIA, sino en
#'   la carpeta \code{sia_apps/data}).}
#'
#'   }
#'
#' @export
#'
#' @examples
#' x <- c(" 32,87 ", "2.14", "5e-3", "<ld", ">LC", "ld<x<LC", " < 10", "L.O.Q",
#'        "ND", "L.O, D ", "LC>X>LD")
#' clasif_tipo_dato(x) %>% as.data.frame()
clasif_tipo_dato <- function(x) {

  # Cambiar comas, comas repetidas y puntos repetidos por un único punto:
  y    <- toascii(limpia_num(x))
  num  <- !is.na(as.numeric(y))
  sust <- TRUE

  ## El siguiente paso es importante, porque los "LD<x<LC" se convierten en
  ## "LDXLC". De esta forma, en pasos subsiguientes, no hay peligro de confundir
  ## "LD" con "LDXLC", ya que las expresiones regulares van a buscar siempre
  ## palabras completas:
  v <- if (sust) gsub("[^[:alpha:]]", "", y, useBytes = TRUE) else x

  tipos <- dplyr::case_when(
    num ~ 1L,
    grepl("(\\bLD\\b|\\bLOD\\b|\\bND\\b)", v, ignore.case = TRUE) ~ 2L,
    grepl("(\\bLC\\b|\\bLOQ\\b)", v, ignore.case = TRUE) ~ 3L,
    grepl("\\bL[DC]XL[DC]\\b", v, ignore.case = TRUE) ~ 4L,
    grepl("^\\s*<+\\s*\\.*\\s*[[:digit:]]+", y) ~ 5L,
    grepl("^\\s*>+\\s*\\.*\\s*[[:digit:]]+", y) ~ 6L,
    TRUE ~ 7L
    )

  return(list(valores = y, tipos = tipos))
}


#' Consultar muestras de parámetros de infambiental
#'
#' Trae datos de infambiental a través de una PostgreSQLConnection. Es el método
#' utilizado por las aplicaciones shiny. Normalmente en vez de usar esta
#' función, se usa el set \code{\link{datos_sia}}, que ya tiene datos extraidos
#' y preparados para ensayar ejemplos.
#'
#' @param con `PostgreSQLConnection`: objeto utilizado para conectarse con la
#'   base de datos. Ver details.
#' @param id_matriz integer. Valor único. Número de matriz.
#' @param id_programa integer. Vector con números id de programas. Si es `NULL`
#'   (valor por defecto), selecciona todos los programas.
#' @param id_cuenca integer. Vector con números de cuenca.
#' @param id_sub_cuenca integer. Vector con números de subcuenca.
#' @param id_estacion Integer. Vector con números id de estaciones. Si es `NULL`
#'   (valor por defecto), selecciona todas las estaciones.
#' @param id_tipo_punto integer. Vector con números que identifican el tipo de
#'   punto (1. SUPERFICIE, 2. FONDO).
#' @param id_depto Integer. Vector con números id de departamentos. Si es `NULL`
#'   (valor por defecto), selecciona todos los departamentos.
#' @param id_parametro integer. Vector con números id de parámetros. Si es
#'   `NULL` (valor por defecto), selecciona todos los parámetros.
#' @param id_institucion integer. Vector con números id de instituciones. Si es
#'   `NULL` (valor por defecto), selecciona todas las instituciones.
#' @param usuario character. Vector con nombres de usuarios encontrados en los
#'   datos. Ej.: 'jmartinez'.
#' @param anios integer. Vector con los años para los que se quiere filtrar el
#'   conjunto de datos. Ej.: 2017:2019.
#' @param meses integer. Vector con los números de meses para los que se quiere       filtrar el conjunto de datos. Ej.: 6:12.
#' @param fecha_ini `character`. Fecha en formato `AAAA-MM-DD`.
#' @param fecha_fin `character`. Fecha en formato `AAAA-MM-DD`.
#'
#' @return Una `tibble`, en formato largo, con las columnas:

#' \itemize{
#'
#' \item id_muestra: `integer` id único de cada muestra
#'
#' \item nro_muestra: `integer` nro único de para muestra provenientes del SILAD
#'
#' \item id_estado: `integer` id que identifica el "estado" del dato: 1.
#' pendiente, 2. original, 3. aprobado
#'
#' \item nombre_programa `character` nombre de cada programa
#'
#' \item id_programa: `integer` id único de cada programa
#'
#' \item cue_nombre: `character` nombre de la cuenca
#'
#' \item id_cuenca: `integer` id único de la cuenca
#'
#' \item sub_cue_nombre: `character` nombre de la subcuenca
#'
#' \item id_sub_cuenca: `integer` id único de la subcuenca
#'
#' \item codigo_pto: `character` nombre de cada estación
#'
#' \item id_estacion: `integer` id único de cada estación
#'
#' \item tipo_punto_id: `integer` id único del tipo de punto
#'
#' \item tip_pun_est_descripcion: `character` descripción del tipo de punto
#'
#' \item id_depto: `integer` id único del departamento
#'
#' \item departamento: `character` nombre del departamento
#'
#' \item id_institucion: `integer` id único de la institución
#'
#' \item institucion: `character` nombre de la institución que cargó el dato en
#' el SIA
#'
#' \item usuario: `character` nombre de usuario que cargó el dato en el SIA
#'
#' \item periodo: `character` asignación de mes y año para cada campaña de
#' muestreo.
#'
#' \item anio: `character` año en que fue tomada la muestra
#'
#' \item mes: `character` mes en que fue tomada la muestra
#'
#' \item anio_mes: `character` año+mes en que fue tomada la muestra
#'
#' \item fecha_muestra: `Date` fecha en que fue tomada la muestra
#'
#' \item fecha_hora: `character` fecha y hora en que fue tomada la muestra, en
#' formato "yyyy-mm-dd HH:MM:SS"
#'
#' \item observaciones: `character` observaciones para el dato
#'
#' \item id_matriz: `integer` id único de la matriz
#'
#' \item id_parametro: `integer` id único de cada parámetro
#'
#' \item parametro: `character` nombre extendido del parámetro
#'
#' \item nombre_clave: `character` código único del parámetro
#'
#' \item id_unidad: `integer` id único de las unidades de medida
#'
#' \item uni_nombre: `character` notación de las unidades de medida
#'
#' \item valor_minimo_str: `character` con el valor ingresado para el parámetro
#'
#' \item limite_deteccion: `character` con el valor del límite de detección
#' ingresado para el parámetro
#'
#' \item limite_cuantificacion: `character` con el valor del límite de
#' cuantificación ingresado para el parámetro
#'
#' }
#'
#' @details El parámetro `con` es un objeto utilizado para realizar la conexión
#'   con la base de datos. Específicamente, es llamado por la función
#'   DBI::dbGetQuery. La creación del objeto normalmente se hace al iniciar el
#'   servidor shiny de una aplicación (en el archivo `global.R`
#'   correspondiente), cuyo código está basado en [este
#'   ejemplo](https://tinyurl.com/yfl9kvol).
#'
#'   Los parámetros con el prefijo `id_` refieren al número de id presente en la
#'   base de datos del SIA infambientalbd.
#'
#'   Eliminación de datos repetidos:
#'
#'   En la base de datos original, hay algunos datos que se pueden encontrar
#'   repetidos, debido a que cuando se hacen sucesivos cambios, se mantienen los
#'   valores originales.
#'
#'   Una de estas situaciones es cuando cambia el id_estado: 1. pendiente, 2.
#'   original, 3. aprobado. En caso de encontrar un par id_muestra x
#'   id_parametro repetidos, la función elije aquel que tenga id_estado mayor.
#'
#'   En caso de que lo anterior no sea suficiente para desambigüar, se usa el id
#'   más reciente de la tabla `datos_muestra_parametros` y se descarta el resto.
#'
#' @seealso \code{\link{sia_datos_muestra_parametros}},
#'   \code{\link{sia_muestra}}, \code{\link{clasif_tipo_dato}},
#'   \code{\link{valores_numericos}},
#'   \code{\link{sia_datos_muestra_parametros}},
#'   \code{\link{sia_datos_muestra_parametros}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Conexión con la base de datos:
#' con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "infambientalbd",
#'                       host = "172.20.0.34", port = 5432,
#'                       user = "shiny_usr", password = "shiny_passwd")
#'
#' # Todas las muestras de todos los programas en el año 2019:
#' consulta_muestras(con, fecha_ini = "2019-12-24", fecha_fin = "2019-12-31")
#' consulta_muestras(con, id_programa = 1L,
#'                   id_estacion = c(100054L, 100061L, 100063L, 100172L),
#'                   id_parametro = c(2009L, 2020L),
#'                   fecha_ini = "2017-10-31", fecha_fin = "2019-10-31")
#' }
consulta_muestras <- function(con, id_matriz = 6L,
                              id_programa = NULL,
                              id_cuenca = NULL,
                              id_sub_cuenca = NULL,
                              id_estacion = NULL,
                              id_tipo_punto = NULL,
                              id_depto = NULL,
                              id_parametro = NULL,
                              id_institucion = NULL,
                              usuario = NULL,
                              anios = NULL,
                              meses = NULL,
                              fecha_ini = "1900-01-01",
                              fecha_fin = Sys.Date()) {

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("El paquete \"DBI\" es necesario para esta funci\u00f3n.",
         call. = FALSE)
  }

  fecha_ini <- as.character(fecha_ini)
  fecha_fin <- as.character(fecha_fin)

  x <- is.na(lubridate::ymd(c(fecha_ini, fecha_fin)))
  if (any(x))
    stop("El formato de las fechas parece estar mal (fecha_ini = ",
         fecha_ini, "; fecha_fin = ", fecha_fin, ")")

  cond_matriz <- if (is.null(id_matriz)) NULL else
    paste("pu.id_matriz =", id_matriz)

  cond_programa <- if (is.null(id_programa)) NULL else
    paste("pr.id_programa IN", parentesis(id_programa))

  cond_cuenca <- if (is.null(id_cuenca)) NULL else
    paste("c.id IN", parentesis(id_cuenca))

  cond_sub_cuenca <- if (is.null(id_sub_cuenca)) NULL else
    paste("sc.id IN", parentesis(id_sub_cuenca))

  cond_estacion <- if (is.null(id_estacion)) NULL else
    paste("e.id IN", parentesis(id_estacion))

  cond_tipo_punto <- if (is.null(id_tipo_punto)) NULL else
    paste("e.tipo_punto_id IN", parentesis(id_tipo_punto))

  cond_depto <- if (is.null(id_depto)) NULL else
    paste("e.departamento IN", parentesis(id_depto))

  cond_parametro <- if (is.null(id_parametro)) NULL else
    paste("dmp.id_parametro IN", parentesis(id_parametro))

  cond_institucion <- if (is.null(id_institucion)) NULL else
    paste("i.id_institucion IN", parentesis(id_institucion))

  cond_usuario <- if (is.null(usuario)) NULL else
    paste("m.usuario IN", parentesis(usuario, comillas = TRUE))

  cond_fechas <-  paste0("m.fecha_muestra >= '", fecha_ini, "'",
                         " and m.fecha_muestra <= '", fecha_fin, "'")

  condiciones <-
    c(c(cond_matriz,
        cond_programa,
        cond_cuenca,
        cond_sub_cuenca,
        cond_estacion,
        cond_tipo_punto,
        cond_depto,
        cond_parametro,
        cond_institucion,
        cond_usuario) %>%
        paste("and"),
      cond_fechas)

  consulta_sql <- c(
    "select
    dmp.id as id_dato,
    dmp.id_muestra,
    m.nro_muestra,
    dmp.id_estado,
    pr.nombre_programa,
    pr.id_programa,
    c.cue_nombre,
    c.id as id_cuenca,
    sc.sub_cue_nombre,
    sc.id as id_sub_cuenca,
    e.codigo_pto,
    e.id as id_estacion,
    e.tipo_punto_id,
    tpe.tip_pun_est_descripcion,
    e.departamento as id_depto,
    d.dep_nombre as departamento,
    i.id_institucion,
    i.nombre as institucion,
    m.usuario,
    m.periodo,
    to_char(fecha_muestra, 'YYYY') anio,
    to_char(fecha_muestra, 'MM') mes,
    to_char(fecha_muestra, 'YYYY-MM') anio_mes,
    m.fecha_muestra,
    cast(m.fecha_muestra as char(12)) || ' ' || ",
    "cast(m.hora_muestra as char(12)) as fecha_hora,
    dmp.observacion,
	  m.observaciones,
    pu.id_matriz,
    p.id_parametro,
    p.parametro,
    p.nombre_clave,
    u.id as id_unidad,
    u.uni_nombre,
    dmp.valor_minimo_str,
    dmp.limite_deteccion,
    dmp.limite_cuantificacion
    from datos_muestra_parametros dmp
    left join parametro p on dmp.id_parametro = p.id_parametro
    left join muestra m on dmp.id_muestra = m.id_muestra
    left join institucion i on m.id_institucion = i.id_institucion
    left join estacion e on m.id_estacion = e.id
    left join tipo_punto_estacion tpe on e.tipo_punto_id = tpe.id
    left join sub_cuenca sc on e.sub_cuenca = sc.id
    left join cuenca c on sc.sub_cue_cuenca_id = c.id
    left join departamento d on e.departamento = d.id
    left join programa pr on e.prog_monitoreo = pr.id_programa
    left join param_unidad pu on p.id_parametro = pu.id_parametro
    and e.matriz_estacion = pu.id_matriz
    left join unidad u on pu.id_unidad_medida = u.id",
    " where ", condiciones, ";"
  )

  # writeLines(consulta_sql, "consulta_sql.sql")

  consulta_sql <- stringr::str_squish(paste(consulta_sql, collapse = " "))

  out <- DBI::dbGetQuery(con, consulta_sql)

  if (!nrow(out)) return(NULL)

  out <- out %>%
    # set_utf8() %>%
    tibble::as_tibble()

  # out$obs_tmp <- pegar_obs(out$observacion, out$observaciones)
  out$observaciones <- pegar_obs(out$observacion, out$observaciones)
  out <- out[names(out) != "observacion"]

  if (!is.null(anios))
    out <- dplyr::filter(out, anio %in% anios)

  if (!is.null(meses)) {
    meses <- stringr::str_pad(meses, 2, side = "left", pad = "0")
    out <- dplyr::filter(out, mes %in% meses)
  }

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

  return(out)
}


#' Filtrar datos_sia
#'
#' Función que filtra la tabla \code{\link{datos_sia}} (u otra tabla con un
#' conjunto de columnas adecuado) de manera similar a la forma en que se extraen
#' los datos en la aplicación shiny iSIA.
#'
#' El uso del argumento `orden_est` equivale, salvo algunas excepciones (que son
#' anunciadas con warnings), a ejecutar:
#'
#' `.data$codigo_pto <- factor(.data$codigo_pto, levels = orden_est)`
#'
#' Nota: `codigo_pto` es la columna con los nombres de las estaciones.
#'
#' @param .data Tabla con datos extraidos del SIA (\code{\link{datos_sia}} en
#'   principio)
#' @param id_programa integer. Un sólo valor que identifica al programa.
#' @param rango_fechas character. Vector de dos valores (fecha inicial y fecha
#'   final), en formato \code{AAAA-MM-DD}.
#' @param id_matriz integer. Un sólo valor que identifica a la matriz.
#' @param id_parametro integer. Vector de valores que identifican los
#'   parámetros.
#' @param id_estacion integer. Vector de valores que identifican las estaciones
#'   (sitios) de monitoreo.
#' @param orden_est character. Vector con los nombres de las estaciones en el
#'   orden deseado para las gráficas u otros usos.
#' @param tipo_punto_id integer. Vector que define los tipos de punto usados: 1
#'   = `SUPERFICIE`, 2 = `FONDO`, 1:2 = Ambos tipos.
#'
#' @return
#'
#' @export
#'
#' @examples
#' filtrar_datos(datos_sia, 5, 6)
#' filtrar_datos(datos_sia, 5, 11)
filtrar_datos <- function(.data,
                          id_programa, # Not NULL!
                          id_matriz = 6L,
                          rango_fechas = NULL,
                          id_parametro = NULL,
                          id_estacion = NULL,
                          orden_est = NULL,
                          tipo_punto_id = 1L) {

  if (missing(id_programa))
    stop("id_programa espera un \u00fanico n\u00famero entero positivo.")

  if (length(id_programa) > 1) {
    id_programa <- id_programa[[1]]
    warning("id_programa espera un \u00fanico n\u00famero entero positivo,",
            " por lo que se us\u00f3 solamente el primer elemento: ",
            id_programa)
  }

  id_programa <- abs(as.integer(id_programa))

  mat_e <-
    dplyr::filter(siabox::programa_matriz,
                  id_programa == !!id_programa)$id_matriz

  if (id_matriz != mat_e) {
    warning("No hay datos de esa matriz ambiental (id_matriz = ", id_matriz,
            ") para el programa de monitoreo solicitado (id_programa = ",
            id_programa, ")")
    return(.data[0,])
  }

  if (is.null(rango_fechas)) {
    rango_fechas <- c("1900-01-01", as.character(Sys.Date() + 1))
  } else if (length(rango_fechas) == 1L) {

    rf_orig <- rango_fechas
    if (grepl("^[12][0-9]{3}$", rango_fechas)) {
      rango_fechas <- paste0(rep.int(rango_fechas, 2), c("-01-01", "-12-31"))
    } else {
      anio <- lubridate::year(as.Date(rango_fechas))
      rango_fechas <- c(rango_fechas,
                        gsub("^[12][0-9]{3}", anio + 1L, rango_fechas))
    }

    warning("rango_fechas ingresado (", rf_orig,
            ") tiene un solo valor: se filtran fechas en el",
            " rango de ", rango_fechas[1], " a ", rango_fechas[2])

  } else if (length(rango_fechas) != 2L) {

    stop("rango_fechas debe ser un vector con dos fechas en formato AAAA-MM-DD")

  } else if (all(grepl("^[12][0-9]{3}$", rango_fechas))) {
    rango_fechas <- paste0(rango_fechas, c("-01-01", "-12-31"))
    warning("rango_fechas se modific\u00f3. Filtrando desde ",
            rango_fechas[1], " a ", rango_fechas[2])
  }

  if (is.null(id_parametro)) {
    id_parametro <- siabox::sia_parametro$id_parametro
    warning("id_parametro no especificado, se seleccionan ",
            "todos los par\u00e1metros por defecto")
  }

  if (is.null(id_estacion)) {
    id_estacion <-
      dplyr::filter(siabox::sia_estacion, prog_monitoreo == !!id_programa)$id

    warning("id_estacion no especificado, se seleccionan por defecto ",
            "las estaciones correspondientes al programa seleccionado (",
            "id_programa = ", id_programa, ")")
  } else {
    est_e <- dplyr::filter(siabox::sia_estacion, prog_monitoreo == id_programa)$id
    w <- id_estacion %in% est_e
    if (!all(w)) {
      id_estacion <- id_estacion[w]
      warning("Se descartaron las estaciones con id ",
              colapsar_secuencia(id_estacion[!w]),
              ", por no pertenecer al programa de monitoreo (id_programa = ",
              id_programa, ")")
    }
  }

  if (is.null(tipo_punto_id)) {
    tipo_punto_id <- 1:2
    warning("tipo_punto_id no especificado. Los datos incluyen estaciones de",
            " tipo SUPERFICIE y FONDO mezcladas (tipo_punto_id = 1 y 2 ",
            "respectivamente)")
  }

  out <- dplyr::filter(.data,
                       id_programa == !!id_programa,
                       id_matriz == !!id_matriz,
                       fecha_muestra >= rango_fechas[[1]],
                       fecha_muestra <= rango_fechas[[2]],
                       id_parametro %in% !!id_parametro,
                       id_estacion %in% !!id_estacion,
                       tipo_punto_id %in% !!tipo_punto_id)

  esperados <-
    tibble::tibble(id = id_estacion) %>%
    dplyr::left_join(siabox::sia_estacion, by = "id") %>%
    dplyr::pull(codigo_pto)

  if (is.null(orden_est)) {
    orden_est <- stringr::str_sort(esperados, numeric = TRUE)
    warning("orden_est no especificado. Se usa orden ',
            'alfab\u00e9tico & num\u00e9rico: ",
            colapsar_secuencia(orden_est))
  } else {
    w <- esperados %in% orden_est
    if (!all(w)) {
      we <- which(sia_estacion$codigo_pto %in% orden_est)
      orden_est <- c(orden_est, esperados[!w])
      if (!length(we)) {
        warning("Los nombres de estaciones indicados en el argumento ",
                "orden_est no se corresponden a estaciones conocidas")
      } else {
        id_e <- sia_estacion$id[we]
      }
      warning("Se agregaron las estaciones ", colapsar_secuencia(esperados[!w]),
              " al final de orden_est. Para evitar agregar m\u00e1s estaciones",
              " incluya el argumento:\n\tid_estacion = c(",
              paste(id_e, collapse = ", "), ")")
    }
  }
  out$codigo_pto <- factor(out$codigo_pto, levels = orden_est)

  return(out)
}

#' Pasar a formato largo
#'
#' Los valores vienen en character cuando se importan tablas de campo o
#' laboratorio (clase "planilla"), en el contexto de la app vSIA, y son
#' equivalentes a la columna valor_minimo_str de los datos provenientes del SIA.
#' De hecho, los valores así como están (a menos que tengan errores y sean
#' modificados por un usuarie antes de ser ingresados), van directo a esa misma
#' columna de la tabla datos_muestra_parametros de infambientalbd.
#'
#' En cambio, cuando se trata de datos de origen "sia", se asume que están en el
#' formato de \code{\link[siabox]{datos_sia}} (previamente transformados con la
#' función \code{\link[siabox]{ancho}}), de forma que se espera que los valores
#' sean numéricos.
#'
#' @param .data data.frame. Tabla de datos, con formatos específicos según su
#'   clase.
#'
#' @return Devuelve los mismos datos pero en formato largo: los nombres del
#'   parámetro van en una columna y en otras columnas: valor, LD y LC. Esto
#'   puede variar según los distintos métodos.
#' @export
#'
#' @examples
#' #' d <- filtrar_datos(datos_sia,
#'                    id_programa = 10L,
#'                    rango_fechas = c("2019-01-01", "2020-12-31"),
#'                    id_parametro = c(2099, 2098)) %>%
#'   dplyr::select(codigo_pto, fecha_muestra, id_parametro, param, id_unidad,
#'                 uni_nombre, limite_deteccion, limite_cuantificacion, valor)
#' da <- ancho(d)
#' (dal <- largo(da))
#'
#' dim(d)
#' dim(dal)
#'
#' datos_sia %>%
#'   dplyr::filter(id_programa == 4, id_parametro %in% c(2017, 2021)) %>%
#'   ancho %>%
#'   largo
largo <- function(.data, ...) {
  UseMethod("largo")
}

#' Formato largo
#'
#' Pensada para ser la contraparte de \code{link[siabox]{ancho}}.
#'
#' @param .data data.frame. Formato similar al creado con
#'   \code{link[siabox]{ancho}} a partir de \code{link[siabox]{datos_sia}} o
#'   afines
#'
#' @return data.frame en formato largo.
#' @export largo.default
#' @export
#'
#' @examples
#' d <- filtrar_datos(datos_sia,
#'                    id_programa = 10L,
#'                    rango_fechas = c("2019-01-01", "2020-12-31"),
#'                    id_parametro = c(2099, 2098)) %>%
#'   dplyr::select(codigo_pto, fecha_muestra, id_parametro, param, id_unidad,
#'                 uni_nombre, limite_deteccion, limite_cuantificacion, valor)
#' da <- ancho(d)
#' (dal <- largo(da))
#'
#' dim(d)
#' dim(dal)
#'
#' datos_sia %>%
#'   dplyr::filter(id_programa == 4, id_parametro %in% c(2017, 2021)) %>%
#'   ancho %>%
#'   largo
largo.default <- function(.data) {
  # Presupone que .data es una tabla creada con un comando tipo
  # siabox::ancho(x), en donde x tiene el mismo set de columnas que
  # siabox::datos_sia
  d <- dplyr::mutate(.data, nfila = dplyr::row_number())
  s <- c(names(siabox::datos_sia), "nfila")
  s <- s[!(s %in% c(
    "valor", "usuario", "id_parametro", "parametro", "nombre_clave",
    "id_unidad", "uni_nombre", "valor_minimo_str", "id_tipo_dato", "grupo",
    "limite_deteccion", "limite_cuantificacion", "param"))]

  ld_cols <- grep("[\\s+_]LD", names(d), ignore.case = TRUE)
  lc_cols <- grep("[\\s+_]LC", names(d), ignore.case = TRUE)

  no_lim_cols <- !grepl("[\\s+_]L[CD]", names(d), ignore.case = TRUE)

  largo_val <- d[no_lim_cols] %>%
    tidyr::pivot_longer(-tidyselect::all_of(s),
                        names_to = "param",
                        values_to = "valor")

  largo_ld <-
    # ncol(d) = nfila
    d[c(ncol(d), ld_cols)] %>%
    tidyr::pivot_longer(-1L,
                        names_to = "param",
                        values_to = "limite_deteccion",
                        names_pattern = "(.*)_[Ll][Dd]$")

  largo_lc <-
    # ncol(d) = nfila
    d[c(ncol(d), lc_cols)] %>%
    tidyr::pivot_longer(-1L,
                        names_to = "param",
                        values_to = "limite_cuantificacion",
                        names_pattern = "(.*)_[Ll][Cc]$")

  out <- largo_val %>%
    dplyr::left_join(largo_ld, by = c("nfila", "param")) %>%
    dplyr::left_join(largo_lc, by = c("nfila", "param"))

  out <- dplyr::filter(out, !(is.na(valor) &
                                is.na(limite_deteccion) &
                                is.na(limite_cuantificacion)))

  return(out)
}

#' Planilla a formato largo
#'
#' Planilla refiere a planilla con formato SILAD o al template para datos de
#' campo del vSIA.
#'
#' Asume que siempre que hay LD también hay valores de LC.
#'
#' Importante: si `tcols` es `NULL`, se asume que las columnas parametro, LD y
#' LC (si es que los datos incluyen límites de detección o cuantificación),
#' vienen siempre en ese orden, para todos los parámetros y que todos los
#' parámetros tienen nombres únicos que los distinguen (cosa que tengo patente
#' que no es necesariamente cierto). O sea que si están los parámetros AlcT y
#' STF, entonces las columnas estarán en el orden: AlcT (mg CaCO3/L), AlcT LD,
#' AlcT LC, STF (mg/L), STF LD, STF LC (aunque sí podrían estar todas las
#' columnas STF antes que las de AlcT; lo que importa es que siempre venga LC
#' luego de LD y este luego del parámetro en sí.).
#'
#' Por esta razón, en vSIA, al hacer largo con el paquete de datos (PD), el cual
#' puede incluir parámetros de campo sin LD o LC, al mismo tiempo que parámetros
#' de laboratorio que sí incluye esos límites, es necesario que `tcols` no sea
#' `NULL`.
#'
#' @param .data data.frame con los datos en formato ancho (planilla SILAD o
#'   template de campo de vSIA)
#' @param tcols data.frame opcional: la tabla de columnas creada durante el
#'   proceso de vSIA.
#'
#' @return Devuelve los mismos datos pero en formato largo, con la columna
#'   `nfila` agregada, valores (character) en la columna `valor_minimo_str`
#'   (nombre usado para que coincida con `datos_muestras_parametros` de
#'   infambietnalbd). El parámetro se indica en la columna `nombre_clave`,
#'   cuando `tcols` es null, o `id_parametro`, en caso contrario.
#'
#'   Debe tenerse en cuenta que se eliminan las entradas sin valores (i.e.:
#'   `NA`). En caso de que en los datos incluyan LD o LC, las entradas elminadas
#'   son aquellas que no tengan valores en `valor_minimo_str`, ni en
#'   `limite_deteccion`, ni en `limite_cuantificacion`
#'
#' @export largo.planilla
#' @export
#'
#' @examples
#' lista_campo <- readRDS("tmp/campo.rds")
#' largo(lista_campo$datos)
#' largo(lista_campo$datos, lista_campo$columnas)
#' lista_lab <- readRDS("tmp/lab.rds")
#' largo(lista_lab$datos)
#' largo(lista_lab$datos, lista_lab$columnas$tabla_columnas)
#' lista_pd <- readRDS("~/R/sia_apps/vSIA/tmp/lista_pd.rds")
#' largo.planilla(lista_pd$datos, lista_pd$ppd)
#' datos_sin_ppd <-
#'   lista_pd$datos[- lista_pd$ppd$ncol[lista_pd$ppd$tabla == "Campo"]]
#' largo.planilla(datos_sin_ppd)
largo.planilla <- function(.data, tcols = NULL) {
  datadim <- dim(.data)
  d <- dplyr::mutate(.data, nfila = 1:datadim[1])
  nc <- 1:ncol(d)

  if (is.null(tcols)) {
    nombres_a <- "nombre_clave"
    patron_ld = "(.*)[[:space:]_]LD"
    patron_lc = "(.*)[[:space:]_]LC"

    # meta
    metacols <- c("campana", "estacion", "fecha", "fecha muestra", "hora",
                  "nro. muestra", "observaciones","parametro", "parametro:",
                  "replica", "nfila")
    met_l <- tolower(toascii(names(d))) %in% metacols
    ld_l  <- grepl("[[:space:]_]LD$", names(d), ignore.case = TRUE)
    lc_l  <- grepl("[[:space:]_]LC$", names(d), ignore.case = TRUE)
    par_l <- !met_l & !ld_l & !lc_l
    if (sum(par_l) != sum(ld_l) || sum(par_l) != sum(lc_l))
      stop("Todos los parámetros deben presentar columnas de Valor, LD y LC")
    s <- names(d)[met_l]
    met_i <- nc[met_l]
    par_i <- nc[par_l]

    ld_i <- nc[ld_l]
    lc_i <- nc[lc_l]

    if (sum(ld_l)) {
      parnames <- gsub("(.*)[[:space:]_]LD", "\\1", names(d)[ld_l])
      names(d)[par_l] <- parnames

      ncol_par <- tibble::tibble(nombre_clave = parnames,
                                 ncol = par_i,
                                 columna = names(d)[par_i],
                                 ncol_ld = ld_i,
                                 columna_ld = names(d)[ld_i],
                                 ncol_lc = lc_i,
                                 columna_lc = names(d)[lc_i])
    }
  } else {
    nombres_a <- "id_parametro"
    patron_ld <- "([0-9]+)_LD"
    patron_lc <- "([0-9]+)_LC"
    par_l <- !is.na(tcols$id_parametro) & tcols$tipo == "Valor"
    ld_l  <- !is.na(tcols$id_parametro) & tcols$tipo == "LD"
    lc_l  <- !is.na(tcols$id_parametro) & tcols$tipo == "LC"
    ld_i  <- tcols$ncol[ld_l]
    lc_i  <- tcols$ncol[lc_l]

    met_i <- nc[!(nc %in% tcols$ncol[!is.na(tcols$id_parametro)])]
    par_i <- tcols$ncol[par_l]

    s <- names(d)[met_i]
    names(d)[par_i]  <- tcols$id_parametro[par_l]
    names(d)[ld_i]   <- paste0(tcols$id_parametro[ld_l], "_LD")
    names(d)[lc_i]   <- paste0(tcols$id_parametro[lc_l], "_LC")

    ncol_par <-
      data.frame(id_parametro = tcols$id_parametro[par_l],
                 ncol = tcols$ncol[par_l],
                 columna = tcols$columna[par_l]) %>%
      dplyr::left_join(data.frame(id_parametro = tcols$id_parametro[ld_l],
                                  ncol_ld = tcols$ncol[ld_l],
                                  columna_ld = tcols$columna[ld_l],
                                  ncol_lc = tcols$ncol[lc_l],
                                  columna_lc = tcols$columna[lc_l]),
                       by = "id_parametro")
    # Equivalente aprox.:
    # dplyr::select(tcols, id_parametro, tipo, ncol, columna) %>%
    #   tidyr::pivot_wider(id_cols = id_parametro,
    #                      values_from = c(ncol, columna),
    #                      names_from = tipo) %>%
    #   magrittr::set_names(tolower(names(.)))
  }

  largo_val <- d[c(met_i, par_i)] %>%
    tidyr::pivot_longer(-tidyselect::all_of(s),
                        names_to = nombres_a,
                        values_to = "valor_minimo_str")

  if (nombres_a == "id_parametro")
    largo_val$id_parametro <- as.integer(largo_val$id_parametro)

  if (sum(ld_l) && sum(lc_l)) {

    largo_ld <- d[c(ncol(d), ld_i)] %>%
      tidyr::pivot_longer(-1L,
                          names_to = nombres_a,
                          values_to = "limite_deteccion",
                          names_pattern = patron_ld)

    largo_lc <- d[c(ncol(d), lc_i)] %>%
      tidyr::pivot_longer(-1L,
                          names_to = nombres_a,
                          values_to = "limite_cuantificacion",
                          names_pattern = patron_lc)

    if (nombres_a == "id_parametro") {
      largo_ld$id_parametro <- as.integer(largo_ld$id_parametro)
      largo_lc$id_parametro <- as.integer(largo_lc$id_parametro)
    }

    out <- largo_val %>%
      dplyr::left_join(largo_ld, by = c("nfila", nombres_a)) %>%
      dplyr::left_join(largo_lc, by = c("nfila", nombres_a)) %>%
      dplyr::left_join(ncol_par, by = nombres_a) %>%
      dplyr::arrange(nfila, ncol)

    # El siguiente código tiene sentido si el formato largo se usa
    # exclusivamente para analizar los valores numéricos + LD y LC:
    out <- dplyr::filter(out, !(is.na(valor_minimo_str) &
                                  is.na(limite_deteccion) &
                                  is.na(limite_cuantificacion)))
  } else  {
    out <- dplyr::filter(largo_val, !is.na(valor_minimo_str))
  }

  class(out) <- c("planilla_larga", class(out))
  return(out)
}


#' Convertir valores del SIA en numéricos
#'
#' Agrega una columna,llamada \code{valor}, de clase numeric, a una tabla con
#' datos del SIA,  con los valores originales (`valor_minimo_str`) convertidos a
#' numéricos. Los requisitos se exponen en detalles.
#'
#' @param .data `data.frame` con datos provenientes de la base de datos del SIA
#'   (infambientalbd), con al menos tres columnas: `valor_minimo_str`,
#'   `limite_deteccion` y `limite_cuantificacion` (ver detalles).
#' @param filtrar_no_num `logical` (bandera). ¿Eliminar los valores que no se
#'   pudieron convertir en numéricos?
#' @param metodo character. Opciones: "sin_cambios", "basico", "simple",
#'   "informe". Ver \code{\link{clasif_tipo_dato}}
#'
#' @return `tibble` con datos originales y una columna numérica extra, `valor`,
#'   cuyos valores son el resultado de sustitución realizadas con expresiones
#'   regulares. Ver detalles.
#'
#' @export
#'
#' @details Esta función se creó en el contexto de analizar datos numéricos para
#'   validación, pero puede usarse potencialmente para otras tareas, tales como
#'   análisis y visualización de datos provenientes del SIA.
#'
#'   Requiere que .data incluya columnas con valores y límites (detección y
#'   cuantificación), con los mismos nombres que usa la tabla
#'   \code{\link{sia_datos_muestra_parametros}} de la base de datos
#'   infambientalbd (SIA).:
#'
#'   - \code{valor_minimo_str} (character)
#'
#'   - \code{limite_deteccion} (character)
#'
#'   - \code{limite_cuantificacion} (character)
#'
#'   Nota: esto implica que .data tiene formato "largo" (ver
#'   \code{\link[tidyr]{pivot_longer}}), es decir, que en lugar de una columna
#'   para cada parámetro, se incluye una (o más) columna con el nombre del
#'   parámetro correspondiente a cada fila.
#'
#'   En la columna `valor` de la salida, se encuentran los valores de los
#'   parámetros, convertidos en numéricos. Sin importar el método elegido, la
#'   modificación mínima, además de aplicar
#'   \code{\link[base:numeric]{as.numeric}}, es
#'   cambiar comas, comas repetidas y puntos repetidos por un único punto
#'   (marcador de decimales).
#'
#'   Los métodos contemplados implican las siguientes conversiones (X representa
#'   un valor numérico):
#'
#'   \describe{
#'
#'   \item{sin_cambios}{No se aplican cambios, de forma que la columna `valor`
#'   resultante es character e idéntica a `valor_minimo_str`}
#'
#'   \item{basico}{Se aplica \code{\link[base:numeric]{as.numeric}} a la columna
#'   `valor_minimo_str`, luego de hacer unos cambios mínimos (corrigiendo comas
#'   por puntos, etc...). El resultado concreto es que todo lo que es
#'   reconocible como valor numérico, se mantiene en `valor`, mientras que el
#'   resto serán `NA`s}
#'
#'   \item{simple}{
#'
#'   \itemize{
#'
#'   \item <LD = LD
#'
#'   \item <LC = LC
#'
#'   \item <X = X
#'
#'   \item >X = X
#'
#'   }
#'
#'   }
#'
#'   \item{informe}{
#'
#'   \itemize{
#'
#'   \item <LD = LD
#'
#'   \item <LC = LC/2
#'
#'   \item LD<X<LC = (LD + LC) / 2
#'
#'   \item <X = X
#'
#'   \item >X = X
#'
#'   }
#'
#'   }
#'
#'   }
#'
#' @examples
#' # Ejemplo con datos del programa Laguna Merin:
#' d <- datos_sia %>%
#'   dplyr::filter(id_programa == 10L) %>%
#'   dplyr::select(id_parametro, valor_minimo_str,
#'                 limite_deteccion, limite_cuantificacion)
#'
#' valores_numericos(d)
#' valores_numericos(d, metodo = "informe")
#' valores_numericos(d, metodo = "simple")
#' # Porcentajes de algunos tipos de dato:
#' valores_numericos(d, metodo = "informe") %>%
#'   dplyr::group_by(id_parametro) %>%
#'   dplyr::summarise(
#'     porcentaje_numerico = sum(id_tipo_dato == 1L) / dplyr::n(),
#'     porcentaje_menor_lim = sum(id_tipo_dato %in% 2:4) / dplyr::n()
#'     )
#'
#' # Ejemplo con datos del programa Santa Lucía:
#' filtrar_datos(datos_sia, id_programa = 3) %>%
#'   valores_numericos(metodo = "basico")
valores_numericos <- function(.data,
                              filtrar_no_num = FALSE,
                              metodo = "simple") {

  sinc <- grepl("sin_cambios|basico", metodo, ignore.case = TRUE)
  sust <- grepl("simple|informe", metodo, ignore.case = TRUE)
  info <- grepl("informe", metodo, ignore.case = TRUE)

  out <- .data
  clasif <- clasif_tipo_dato(out$valor_minimo_str)
  out$valor <- clasif$valores

  # Casos de >X o <X:
  if (sust) {
    i <- clasif$tipos %in% 5:6
    if (any(i)) {
      out$valor[i] <- stringr::str_replace_all(out$valor[i],
                                               "^[^[:digit:]]+([[:digit:]]+)",
                                               "\\1")
    }
  }

  vnum <- as.numeric(out$valor)

  # Casos de <LD:
  if (sust) {
    ### Otras expresiones regulares probadas:
    # "^\\s*[<>]+\\s*L[,.]*(D|[,.]*O[,.]*D)[,.]*\\s*$"
    # "^\\s*N[,./]*D[,.]*\\s*$"
    ld <- clasif$tipos == 2L

    if (any(ld)) {
      vnum[ld] <-
        out$limite_deteccion[ld] %>%
        stringr::str_trim() %>%
        stringr::str_replace_all("[.,]+", ".") %>%
        as.numeric()
    }
  }

  # Casos de <LC:
  if (sust) {
    ### Otras expresiones regulares probadas:
    # "^\\s*[<>]+\\s*L[,.]*(C|O[,.]*Q)[,.]*\\s*$"
    # "^\\s*L[,.]*[DC][,.]*\\s*[<>]*\\s*X\\s*[<>]*\\s*L[,.]*[CD][,.]*\\s*$"

    lc <- clasif$tipos == 3L

    if (any(lc)) {
      lcnum <-
        out$limite_cuantificacion[lc] %>%
        stringr::str_trim() %>%
        stringr::str_replace_all("[.,]+", ".") %>%
        as.numeric()

      vnum[lc] <- if (info) lcnum / 2 else lcnum
    }
  }

  # Casos de LD<X<LC:
  if (info) {
    ldxlc <- clasif$tipos == 4L

    if (any(ldxlc)) {
      lcnum <-
        out$limite_cuantificacion[ldxlc] %>%
        stringr::str_trim() %>%
        stringr::str_replace_all("[.,]+", ".") %>%
        as.numeric()

      ldnum <-
        out$limite_deteccion[ldxlc] %>%
        stringr::str_trim() %>%
        stringr::str_replace_all("[.,]+", ".") %>%
        as.numeric()

      vnum[ldxlc] <- (ldnum + lcnum) / 2
    }
  }

  out$valor <- if (metodo == "sin_cambios") {
    warning('La opci\u00f3n "filtrar_no_num" es ignorada debido a que fue ',
            'seleccionada la opci\u00f3n "sin_cambios"')
    filtrar_no_num <- FALSE
    out$valor_minimo_str
  } else vnum

  out$id_tipo_dato <- clasif$tipos

  if (!any(names(out) == "id_tipo_dato"))
    out <- dplyr::left_join(out, siabox::tipos_de_dato, by = "id_tipo_dato")

  if (filtrar_no_num) out <- dplyr::filter(out, id_tipo_dato != 7L)

  return(out)
}
