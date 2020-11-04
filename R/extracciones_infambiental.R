#' Formato ancho
#'
#' Ensanchar datos provenientes de consulta_muestras. Espera la presencia de
#' ciertas columnas y una sóla matriz ambiental. Ver detalles.
#'
#' @param .data Tabla de datos obtenida con \code{\link{consulta_muestras}},
#'   posiblemente modificada con valores_numericos y con columnas agregadas por
#'   \code{\link[dplyr]{left_join}}.
#'
#' @details Espera que existan las columnas 'valor' y 'param'. En caso de no
#'   encontrarlas las creará. La primera contiene los valores de los parámetros
#'   muestreados, ya sea en formato numérico o en texto. La segunda debería
#'   contener el código del parámetro junto a las unidades de medida entre
#'   paréntesis (ej.: \code{'OD (mg/L)'}). En caso de haber una columna llamada
#'   'codigo_nuevo', será utilizada para crear 'param', de lo contrario usará la
#'   columna 'nombre_clave' (de la tabla parametro, de infambientalbd).
#'
#'   Para facilitar la compatibilidad, esta función además verifica la presencia
#'   de otras columnas agregadas en el código de la app iSIA (en el reactive
#'   \code{datos_extraccion}, del server.R): codigo_nuevo, parametro, grupo,
#'   id_tipo_dato y tipo_dato.
#'
#'
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' d <- dplyr::filter(datos_sia, id_matriz == 6L) %>%
#'   dplyr::select(-parametro, -id_tipo_dato, -tipo_dato, -grupo, -codigo_nuevo)
#' ancho(d)
#'
#' # Cómo usar ancho en lugar de ancho_old:
#' datos_sia %>%
#'   # Primero filtrar para tener sólo 2 parámetros:
#'   dplyr::filter(id_programa == 4, id_parametro %in% c(2017, 2021)) %>%
#'   ancho %>%
#'   select(matches("^SatO |^OD ")) %>%
#'   plot
ancho <- function(.data) {

  matrices <- unique(.data$id_matriz)
  if (length(matrices) > 1) {
    stop('Los datos tienen más de un valor de id_matriz: ',
         colapsar_secuencia(matrices),
         '. Sugerencia: filtrar datos con dplyr::filter o "["')
  }

  if (!any(names(.data) == 'valor')) {
    warning('Se creó automáticamente la columna: valor = valor_minimo_str.')
    .data$valor <- .data$valor_minimo_str
  }

  if (!any(names(.data) == 'param')) {
    warning('Se creó automáticamente la columna "param".')
    .data$param <- if (any(names(.data) == 'codigo_nuevo'))
      paste0(.data$codigo_nuevo, ' (', .data$uni_nombre, ')') else
        paste0(.data$nombre_clave, ' (', .data$uni_nombre, ')')
  }

  # Si están, eliminar estas columnas innecesearias (para compatibilidad con
  # server.R de iSIA: ver reactive 'datos_extraccion'):
  w <- grep('^codigo_nuevo$|^parametro$|^grupo$|tipo_dato$', names(.data),
            ignore.case = TRUE)
  if (length(w)) {
    warning('Se eliminan automáticamente las columnas: ',
            colapsar_secuencia(names(.data)[w]))
    .data <- .data[-w]
  }

  out <- .data %>%
    dplyr::mutate(LD = limite_deteccion %>%
             stringr::str_trim() %>%
             stringr::str_replace_all("[.,]+", ".") %>%
             as.numeric(),
           LC = limite_cuantificacion %>%
             stringr::str_trim() %>%
             stringr::str_replace_all("[.,]+", ".") %>%
             as.numeric()) %>%
    dplyr::select(-id_parametro:-limite_cuantificacion, valor) %>%
    tidyr::pivot_wider(names_from = param, values_from = c(valor, LD, LC)) %>%
    dplyr::arrange(id_muestra)

  nombres <- names(out)
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
                     ~ stringr::str_remove_all(., '\\s+\\(.*\\)$') %>%
                       stringr::str_replace_all('(L[CD])_(.*)', '\\2_\\1'))

  return(out)
}

#' Formato ancho para 2 parámetros
#'
#' **OBSOLETA** (aunque se sigue usando de momento). Usar en cambio
#' \code{\link{ancho}}, habiendo previamente filtrado los datos a 2 parámetros.
#'
#' Aplicar pivot_wider para datos provenientes de consulta_muestras, pero con
#' ciertas precauciones...
#'
#' @param .data data.frame: Datos como vienen del SIA
#' @param id_x integer: id_parametro "X". Ejemplo: 2098 (PT)
#' @param id_y integer: id_parametro "Y". Ejemplo: 2097 (PO4)
#' @param par_x integer: id_parametro "X". Ejemplo: 2098 (PT)
#' @param par_y integer: id_parametro "Y". Ejemplo: 2097 (PO4)
#' @param e_sel logical: corresponde considerar la columna est_sel
#'
#' @return data.frame ancha.
#' @export
#'
#' @examples
#' # Cómo usar ancho en lugar de ancho_old:
#' datos_sia %>%
#'   # Primero filtrar para tener sólo 2 parámetros:
#'   dplyr::filter(id_programa == 4, id_parametro %in% c(2017, 2021)) %>%
#'   ancho %>%
#'   select(matches("^SatO |^OD ")) %>%
#'   plot
ancho_old <- function(.data, id_x, id_y,
                      par_x = NULL, par_y  = NULL,
                      e_sel = TRUE) {

  repes <- .data %>%
    dplyr::count(id_muestra, id_parametro) %>%
    dplyr::filter(n > 1)

  if (is.null(par_x)) {
    par_x <- sia_parametro %>%
      dplyr::filter(id_parametro == id_x) %>%
      dplyr::pull(nombre_clave)
  }

  if (is.null(par_y)) {
    par_y <- sia_parametro %>%
      dplyr::filter(id_parametro == id_y) %>%
      dplyr::pull(nombre_clave)
  }

  # A continuación: si es que hay repetidos, quedarme sólo con los que
  # figuran como aprobados...
  # Esto tal vez lo debería hacer con todas las muestras, no?
  for (i in 1:nrow(repes)) {
    w <- which(
      .data$id_muestra == repes$id_muestra[i] &
        .data$id_parametro == repes$id_parametro[i]
    )

    # id_estado = 1: pendiente
    # id_estado = 2: original
    # id_estado = 3: aprobado
    w_aprob <- which(.data$id_estado[w] == 3)

    if (length(w_aprob)) {
      fila <- .data[w,][w_aprob,]
      .data <- .data[-w,]
      .data <- rbind(.data, fila)
    }
  }

  .data_x <- .data %>%
    dplyr::filter(id_parametro == id_x) %>%
    dplyr::rename(!!dplyr::sym(par_x) := valor) %>%
    dplyr::select(-id_estado, -nombre_clave, -id_parametro, -observaciones,
                  -valor_minimo_str, -limite_deteccion, -limite_cuantificacion)

  .data_y <- .data %>%
    dplyr::filter(id_parametro == id_y) %>%
    dplyr::rename(!!sym(par_y) := valor) %>%
    dplyr::select(-id_estado, -nombre_clave, -id_parametro, -observaciones,
                  -valor_minimo_str, -limite_deteccion, -limite_cuantificacion)

  v_by <- c("id_muestra", "nombre_programa", "id_programa",
            "codigo_pto", "id_estacion", "id_depto", "departamento",
            "id_institucion", "institucion", "usuario", "anio", "mes",
            "anio_mes", "fecha_muestra", "fecha_hora")

  if (e_sel) v_by <- c(v_by, "est_sel")

  .data_final <-
    dplyr::full_join(.data_x, .data_y, by = v_by) %>%
    dplyr::arrange(id_muestra, fecha_hora)

  return(.data_final)
}

#' Asignar categorías a los datos SIA
#'
#' Evalúa vector character que expresan valores numéricos, según las categorías
#' de la tabla \code{\link{tipos_de_dato}}.
#'
#' @param x Character. Específicamente, espera los valores de la columna
#'   \code{valor_minimo_str} de la tabla \code{datos_muestra_parametros} de la
#'   base de datos infambientalbd (SIA).
#' @param metodo String. El valor de este argumento define la forma en que se
#'   clasifican los valores de X. Ver detalles.
#'
#' @details El método por defecto, "simple", no diferencia "<LC" de "LD<X<LC",
#'   cosa que sí ocurre cuando el método es "informe". En cualquiera de estos
#'   casos se incluyen los tipos "NUMERICO", "<LD", "<LC", "<X", ">X" y "OTRO"
#'   (ids: 1:3 y 5:7, tal como están definidos en la tabla
#'   \code{tipos_de_dato}). Cualquier valor diferente de "simple" o "informe"
#'   clasificará los datos en dos categorías: "NUMERICO" (id = 1) u "OTRO" (id =
#'   7). Números con comas (en vez de puntos) como indicador de decimales, son
#'   considerados numéricos.
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
#'   \code{tipos_de_dato} (por ahora no está presente en el SIA, sino en la
#'   carpeta \code{sia_apps/data}).}
#'
#'   }
#'
#' @export
#'
#' @examples
#' x <- c(" 32,87 ", "2.14", "5e-3", "<ld", ">LC", "ld<x<LC", " < 10", "L.O.Q",
#'        "ND", "L.O, D ", "LC>X>LD")
#' clasif_tipo_dato(x)
#' clasif_tipo_dato(x, "informe")
#' clasif_tipo_dato(x, "")
clasif_tipo_dato <- function(x, metodo = "simple") {

  if (is.null(metodo)) metodo <- ""

  sust <- grepl("simple|informe", metodo, ignore.case = TRUE)
  info <- grepl("informe", metodo, ignore.case = TRUE)

  # Cambiar comas, comas repetidas y puntos repetidos por un único punto:
  y <- limpia_num(x)
  num <- !is.na(as.numeric(y))

  ## El siguiente paso es importante, porque los "LD<x<LC" se convierten en
  ## "LDXLC". De esta forma, en pasos subsiguientes, no hay peligro de confundir
  ## "LD" con "LDXLC", ya que las expresiones regulares van a buscar siempre
  ## palabras completas:
  v <- if (sust) gsub("[^[:alpha:]]", "", y, useBytes = TRUE) else x

  ld <- if (sust)
    grepl("(\\bLD\\b|\\bLOD\\b|\\bND\\b)", v, ignore.case = TRUE) else
      logical(length(x))

  if (sust) {
    menorX <- grepl("^\\s*<+\\s*\\.*\\s*[[:digit:]]+", y)
    mayorX <- grepl("^\\s*>+\\s*\\.*\\s*[[:digit:]]+", y)
  } else {
    menorX <- mayorX <- ld
  }

  if (info) {
    lc <- grepl("(\\bLC\\b|\\bLOQ\\b)", v, ignore.case = TRUE)
    ldxlc <- grepl("\\bL[DC]XL[DC]\\b", v, ignore.case = TRUE)
  } else if (sust) {
    lc <- grepl("(\\bLC\\b|\\bLOQ\\b|\\bL[DC]XL[DC]\\b)", v, ignore.case = TRUE)
    ldxlc <- logical(length(x))
  } else {
    ldxlc <- lc <- ld
  }

  tipos <- dplyr::case_when(num    ~ 1L,
                            ld     ~ 2L,
                            lc     ~ 3L,
                            ldxlc  ~ 4L,
                            menorX ~ 5L,
                            mayorX ~ 6L,
                            TRUE   ~ 7L)

  return(list(valores = y, tipos = tipos))
}


#' Consultar muestras de parámetros de infambiental
#'
#' Trae datos de infambiental a través de una PostgreSQLConnection.
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
#' @seealso \code{\link{sia_datos_muestra_parametros}}, \code{\link{sia_muestra}}, \code{\link{clasif_tipo_dato}}, \code{\link{valores}}, \code{\link{sia_datos_muestra_parametros}}, \code{\link{sia_datos_muestra_parametros}}
#' @export
#'
#' @examples
#' # Conexión con la base de datos:
#' drv <- DBI::dbDriver("PostgreSQL")
#' con <- DBI::dbConnect(drv, dbname = "infambientalbd",
#'                       host = "172.20.0.34", port = 5432,
#'                       user = "shiny_usr", password = "shiny_passwd")
#' # Para trabajar en la máquina windows de la oficina, en donde ya sé que la
#' # codificación de caracteres es WIN1252 (en verdad Sys.getlocale() dice
#' # "Spanish_Uruguay.1252", pero asumo que es lo mismo y en las pruebas que hice
#' # anduvo bien):
#' if (grepl("DINAMA-OAN11", Sys.info()["nodename"], ignore.case = TRUE)) {
#'   # DBI::dbExecute(con, "SET CLIENT_ENCODING TO 'WIN1252';")
#'   DBI::dbExecute(con, "SET NAMES 'WIN1252';")
#' }
#'
#' # Todas las muestras de todos los programas en el año 2019:
#' consulta_muestras(con, fecha_ini = "2019-12-24", fecha_fin = "2019-12-31")
#' consulta_muestras(con, id_programa = 1L,
#'                   id_estacion = c(100054L, 100061L, 100063L, 100172L),
#'                   id_parametro = c(2009L, 2020L),
#'                   fecha_ini = "2017-10-31", fecha_fin = "2019-10-31")
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
    dmp.id,
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
    and pu.id_matriz = e.matriz_estacion
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

  if (!is.null(anios))
    out <- dplyr::filter(out, anio %in% anios)

  if (!is.null(meses)) {
    meses <- stringr::str_pad(meses, 2, side = "left", pad = "0")
    out <- dplyr::filter(out, mes %in% meses)
  }

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
      w_ultimo <- which.max(out$id[w])

      out <- out[-w[-w_ultimo],]
    }
  }

  out <- select(out, -id)

  return(out)
}


#' Filtrar datos_sia
#'
#' Función que filtra la tabla \code{\link{datos_sia}} (u otra tabla con un
#' conjunto de columnas adecuado) de manera similar a la forma en que se extraen
#' los datos en la aplicación shiny iSIA.
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
#'
#' @return
#'
#' @export
#'
#' @examples
#' filtrar_datos(datos_sia, 5, 6)
#' filtrar_datos(datos_sia, 5, 11)
filtrar_datos <- function(.data,
                          id_programa = 4L, # Not NULL!
                          id_matriz = 6L,
                          rango_fechas = NULL,
                          id_parametro = NULL,
                          id_estacion = NULL,
                          orden_est = NULL) {

  if (missing(id_programa))
    stop("id_programa espera un único número entero positivo.")

  if (length(id_programa) > 1) {
    id_programa <- id_programa[[1]]
    warning("id_programa espera un único número entero positivo, por lo que ",
            "se usó solamente el primer elemento: ", id_programa)
  }

  id_programa <- abs(as.integer(id_programa))

  mat_e <-
    dplyr::filter(programa_matriz, id_programa == !!id_programa)$id_matriz

  if (id_matriz != mat_e) {
    warning("No hay datos de esa matriz ambiental (id_matriz = ", id_matriz,
            ") para el programa de monitoreo solicitado (id_programa = ",
            id_programa, ")")
    return(.data[0,])
  }

  if (is.null(rango_fechas)) {
    rango_fechas <- c("1900-01-01", as.character(Sys.Date() + 1))
  } else if (length(rango_fechas) == 1L) {

    if (grepl("^[12][0-9]{3}$", rango_fechas)) {
      rango_fechas <- paste0(rep.int(rango_fechas, 2), c("-01-01", "-12-31"))
    } else {
      anio <- lubridate::year(as.Date(rango_fechas))
      rango_fechas <- c(rango_fechas,
                        gsub("^[12][0-9]{3}", anio + 1L, rango_fechas))
    }



    warning("rango_fechas tiene un solo valor: se filtran fechas en el",
            " rango de ", rango_fechas[1], " a ", rango_fechas[2])

  } else if (length(rango_fechas) != 2L) {

    stop("rango_fechas debe ser un vector con dos fechas en formato AAAA-MM-DD")

  } else if (all(grepl("^[12][0-9]{3}$", rango_fechas))) {
    rango_fechas <- paste0(rango_fechas, c("-01-01", "-12-31"))
    warning("rango_fechas se modificó. Filtrando desde ",
            rango_fechas[1], " a ", rango_fechas[2])
  }

  if (is.null(id_parametro)) {
    id_parametro <- sia_parametro$id_parametro
    warning("id_parametro no especificado, se seleccionan",
            "todos los parámetros por defecto")
  }

  if (is.null(id_estacion)) {
    id_estacion <-
      dplyr::filter(sia_estacion, prog_monitoreo == !!id_programa)$id

    warning("id_estacion no especificado, se seleccionan por defecto ",
            "las estaciones correspondientes al programa seleccionado (",
            "id_programa = ", id_programa, ")")
  } else {
    est_e <- dplyr::filter(sia_estacion, prog_monitoreo == id_programa)$id
    w <- id_estacion %in% est_e
    if (!all(w)) {
      warning("Se descartaron las estaciones con id ",
              colapsar_secuencia(id_estacion[!w]),
              ", por no pertenecer al programa de monitoreo (id_programa = ",
              id_programa, ")")
      id_estacion <- id_estacion[w]
    }
  }

  out <- dplyr::filter(.data,
                       id_programa == !!id_programa,
                       id_matriz == !!id_matriz,
                       fecha_muestra >= rango_fechas[[1]],
                       fecha_muestra <= rango_fechas[[2]],
                       id_parametro %in% !!id_parametro,
                       id_estacion %in% !!id_estacion)

  esperados <-
    tibble(id = id_estacion) %>%
    left_join(sia_estacion, by = "id") %>%
    pull(codigo_pto)

  if (is.null(orden_est)) {
    orden_est <- stringr::str_sort(esperados, numeric = TRUE)
    warning("orden_est no especificado. Se usa orden alfabético & numérico: ",
            colapsar_secuencia(orden_est))
  } else {
    w <- esperados %in% orden_est
    if (!all(w)) {
      orden_est <- c(orden_est, esperados[!w])
      warning("Se agregaron las estaciones ",
              colapsar_secuencia(esperados[!w]),
              " al final de orden_est")
    }
  }
  out$codigo_pto <- factor(out$codigo_pto, levels = orden_est)

  tp <- unique(out$tipo_punto_id)
  if (all(tp %in% 1:2))
    warning("El conjunto de datos tiene estaciones de tipo SUPERFICIE y FONDO ",
            "mezcladas (tipo_punto_id = 1 y 2 respectivamente)")

  return(out)
}


#' Convertir valores del SIA en numéricos
#'
#' Agrega una columna,llamada \code{valor}, de clase numeric, a una tabla con
#' datos del SIA,  con los valores originales convertidos a numéricos. Los
#' requisitos se exponen en la sección "Details".
#'
#' @param .data `data.frame` con datos provenientes de la base de datos del SIA
#'   (infambientalbd), con al menos tres columnas: `valor_minimo_str`,
#'   `limite_deteccion` y `limite_cuantificacion` (ver detalles).
#'
#' @param filtrar_no_num `logical` (bandera). ¿Conservar los valores que no se
#'   pudieron convertir en numéricos?
#' @param inheritParams clasif_tipo_dato
#'
#' @return `tibble` con datos originales y una columna numérica extra, `valor`,
#'   cuyos valores son el resultado de sustitución realizadas con expresiones
#'   regulares. Ver detalles.
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
#'   modificación mínima, además de aplicar \code{\link[base]{as.numeric}}, es
#'   cambiar comas, comas repetidas y puntos repetidos por un único punto
#'   (marcador de decimales).
#'
#'   Los métodos contemplados implican las siguientes conversiones (X representa
#'   un valor numérico):
#'
#'   \describe{
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
#' d <- datos_sia %>%
#'   dplyr::select(id_parametro, valor_minimo_str,
#'                 limite_deteccion, limite_cuantificacion)
#'
#' valores_numericos(d)
#' valores_numericos(d, metodo = "informe")
#' # Porcentajes de algunos tipos de dato:
#' valores_numericos(d, metodo = "informe") %>%
#'   dplyr::group_by(id_parametro) %>%
#'   dplyr::summarise(
#'     porcentaje_numerico = sum(id_tipo_dato == 1L) / dplyr::n(),
#'     porcentaje_menor_lim = sum(id_tipo_dato %in% 2:4) / dplyr::n()
#'     )
valores_numericos <- function(.data,
                              filtrar_no_num = FALSE,
                              metodo = "simple") {

  sinc <- grepl("sin_cambios|basico", metodo, ignore.case = TRUE)
  sust <- grepl("simple|informe", metodo, ignore.case = TRUE)
  info <- grepl("informe", metodo, ignore.case = TRUE)

  out <- .data
  clasif <- clasif_tipo_dato(out$valor_minimo_str, metodo = metodo)
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

  if (sinc) {
    warning('La opción "filtrar_no_num" es ignorada debido a que fue ',
            'seleccionada la opción "sin_cambios"')
    filtrar_no_num <- FALSE
    out$valor <- out$valor_minimo_str
  } else out$valor <- vnum

  out$id_tipo_dato <- clasif$tipos

  out <- dplyr::left_join(out, tipos_de_dato, by = "id_tipo_dato")

  if (filtrar_no_num) out <- dplyr::filter(out, !is.na(valor))

  return(out)
}
