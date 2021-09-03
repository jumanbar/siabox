# README ----
#
# Funciones de ayuda diversa.
#
# - *_id: hacen todas más o menos lo mismo.
#
# - unipar: devuelve unidades de un parámetro
#
# - demo_lm: genera un archivo nuevo con un ejemplo de informe de Laguna Merín.
#
# - colapsar_secuencia: convierte un vector en un texto escrito en español.
#
# . . . . . . . . .  . . . . .  .  . . . . . . . . -----
#
# ID -------

#' @describeIn par_id Busca departamentos en \code{\link{sia_departamento}} en
#'   base al campo `dep_nombre` de dicha tabla.
#' @export
dep_id <- function(patron, ...) {
  if (is.numeric(patron)) {
    pnum <- floor(abs(patron))
    if (pnum != patron)
      warning('Se cambi\u00f3 el n\u00famero de patron: de ',
              patron, ' a ', pnum)
    return(dplyr::filter(sia_departamento, id == pnum))
  }
  dplyr::filter(siabox::sia_departamento,
                agrepl(toascii(patron),
                       toascii(dep_nombre),
                       ignore.case = TRUE, ...))

}

#' @describeIn par_id Busca estaciones en \code{\link{sia_estacion}} en base a
#'   los campos `codigo_pto` y `estacion` de dicha tabla.
#'
#' @export
est_id <- function(patron, ...) {

  if (is.numeric(patron)) {
    pnum <- floor(abs(patron))
    if (pnum != patron)
      warning('Se cambi\u00f3 el n\u00famero de patron: de ',
              patron, ' a ', pnum)
    return(dplyr::filter(sia_estacion, id == pnum))
  }

  patron <- toascii(patron)

  resA <- agrepl(patron, toascii(siabox::sia_estacion$codigo_pto),
                 ignore.case = TRUE,
                 ...)

  resB <- agrepl(patron, toascii(siabox::sia_estacion$estacion),
                 ignore.case = TRUE,
                 ...)

  siabox::sia_estacion[resA | resB,]

}
#' @describeIn par_id Busca instituciones en \code{\link{sia_institucion}} en
#'   base al campo `nombre` de dicha tabla.
#' @export
ins_id <- function(patron, ...) {
  if (is.numeric(patron)) {
    pnum <- floor(abs(patron))
    if (pnum != patron)
      warning('Se cambi\u00f3 el n\u00famero de patron: de ',
              patron, ' a ', pnum)
    return(dplyr::filter(sia_institucion, id_institucion == pnum))
  }
  dplyr::filter(siabox::sia_institucion,
                agrepl(toascii(patron),
                       toascii(nombre),
                       ignore.case = TRUE, ...))

}

#' @describeIn par_id Busca matrices en \code{\link{sia_matriz}} en base al
#'   campo `nombre` de dicha tabla.
#'
#' @export
mat_id <- function(patron, ...) {
  if (is.numeric(patron)) {
    pnum <- floor(abs(patron))
    if (pnum != patron)
      warning('Se cambi\u00f3 el n\u00famero de patron: de ',
              patron, ' a ', pnum)
    return(dplyr::filter(sia_matriz, id_matriz == pnum))
  }
  dplyr::filter(siabox::sia_matriz,
                agrepl(toascii(patron),
                       toascii(nombre),
                       ignore.case = TRUE, ...))

}

#' Buscadores de id
#'
#' Buscadores de id para las varias tablas importadas del SIA, usando un texto
#' (un google de id_parametros). El texto o patrón puede ser una expresión
#' regular (la cual será evaluada por \code{\link[base:agrep]{agrepl}}).
#'
#' @param patron character o numeric. Si es character, expresión regular tipo
#'   \code{\link[base]{regex}}. Si es numeric, el id_parametro de interés.
#' @param ... Argumentos pasados a \code{\link[base:agrep]{agrepl}} para buscar en las
#'   columnas `parametro` y `nombre_clave` de \code{\link{sia_parametro}}
#'
#' @seealso \code{\link{sia_parametro}}
#'
#' @export
#'
#' @return Según la función, van a devolver parte relevante de la tabla original
#'   del SIA: \code{par_id} trae de \code{sia_parametro}, \code{pro_id} de
#'   \code{sia_programa}, \code{est_id} de \code{sia_estacion}, \code{mat_id} de
#'   \code{sia_matriz}, \code{uni_id} de \code{sia_unidad}, \code{ins_id} de
#'   \code{sia_institucion} y \code{dep_id} de \code{sia_departamento}.
#'
#' @examples
#' par_id("fósforo")
#' par_id(2098)
#' par_id(-2098)
#' par_id(2098.98654)
#' par_id("pt", max.distance = 0)
#' pro_id("merin")
#' est_id("pascual")
#' dep_id("cane")
#' est_id("pascual")
#' est_id("pascal")
#' est_id("pscal")
#' est_id("pacl")
#' mat_id("agua")
#' uni_id("mg/l")
#' uni_id("mg/l", max.distance = 0)
#' ins_id("cane")
#' dep_id("flor")
par_id <- function(patron, ...) {

  if (is.numeric(patron)) {
    pnum <- floor(abs(patron))
    if (pnum != patron)
      warning('Se cambi\u00f3 el n\u00famero de patron: de ',
              patron, ' a ', pnum)
    return(dplyr::filter(sia_parametro, id_parametro == pnum))
  }

  patron <- toascii(tolower(patron))

  w <- which(toascii(tolower(siabox::sia_parametro$nombre_clave)) == patron)
  if (length(w)) return(siabox::sia_parametro[w,])

  w <- which(toascii(tolower(siabox::sia_parametro$parametro)) == patron)
  if (length(w)) return(siabox::sia_parametro[w,])

  resA <- agrepl(patron, toascii(siabox::sia_parametro$nombre_clave),
                 ignore.case = TRUE,
                 ...)

  resB <- agrepl(patron, toascii(siabox::sia_parametro$parametro),
                 ignore.case = TRUE,
                 ...)

  siabox::sia_parametro[resA | resB,]
}

#' @describeIn par_id Busca programas en \code{\link{sia_programa}} en base al
#'   campo `nombre_programa` de dicha tabla.
#'
#' @export
pro_id <- function(patron, ...) {

  if (is.numeric(patron)) {
    pnum <- floor(abs(patron))
    if (pnum != patron)
      warning('Se cambi\u00f3 el n\u00famero de patron: de ',
              patron, ' a ', pnum)
    return(dplyr::filter(sia_programa, id_programa == pnum))
  }

  dplyr::filter(siabox::sia_programa, agrepl(toascii(patron),
                                             toascii(nombre_programa),
                                             ignore.case = TRUE, ...))

}

#' @describeIn par_id Busca unidades en \code{\link{sia_unidad}} en base al
#'   campo `uni_nombre` de dicha tabla.
#' @export
uni_id <- function(patron, ...) {
  if (is.numeric(patron)) {
    pnum <- floor(abs(patron))
    if (pnum != patron)
      warning('Se cambi\u00f3 el n\u00famero de patron: de ',
              patron, ' a ', pnum)
    return(dplyr::filter(sia_unidad, id == pnum))
  }
  dplyr::filter(siabox::sia_unidad, agrepl(toascii(patron),
                                           toascii(uni_nombre),
                                           ignore.case = TRUE, ...))

}

#' Ver unidades de un parámetro SIA
#'
#' @param id_parametro integer. id del parámetro (ver
#'   \code{\link{sia_parametro}})
#' @param id_matriz integer. id de la matriz (ver \code{\link{sia_matriz}})
#' @param nombre_clave character. Código o nombre clave del parámetro (ver
#'   \code{\link{sia_parametro}})
#'
#' @return
#' @export
#'
#' @seealso \code{\link{sia_parametro}}
#'
#' @examples
#' unipar(c(2098, 2005))
#' unipar(nombre_clave = "^PT$")
#' unipar(nombre_clave = "Colif")
unipar <- function(id_parametro, id_matriz = 6L, nombre_clave) {
  if (missing(id_parametro)) {
    id_parametro <-
      siabox::sia_parametro %>%
      dplyr::filter(grepl(!!nombre_clave, nombre_clave, ignore.case = TRUE)) %>%
      dplyr::pull(id_parametro)
  }
  out <-
    siabox::sia_parametro %>%
    dplyr::select(id_parametro, parametro, nombre_clave) %>%
    dplyr::filter(id_parametro %in% !!id_parametro) %>%
    dplyr::left_join(siabox::sia_param_unidad, by = "id_parametro") %>%
    dplyr::filter(id_matriz %in% !!id_matriz) %>%
    dplyr::left_join(siabox::sia_unidad, by = c("id_unidad_medida" = "id")) %>%
    dplyr::select(id_parametro, parametro, nombre_clave, id_matriz,
                  id_unidad = id_unidad_medida, uni_nombre)
  return(out)
}

# . . . . . . . . .  . . . . .  .  . . . . . . . . -----
#
# MISC ----

#' Preparar lista para imprimir
#'
#' Crea un string con la secuencia delementos en x separados por comas, con
#' excepción del último elemento que es separado con un conector ("y", por
#' defecto).
#'
#' @param x Vector atomico para imprimir.
#'
#' @param conector Caracter para conectar el ultimo elemento de la lista
#'   (tipicamente 'y', '&', 'and', etc...)
#'
#' @param comillas logical o character. Si es logical, determina si se deben
#'   agregar comillas a los elementos de \code{x}. Si es character, usa el valor
#'   asignado para agregar antes y después de cada elemento de \code{x}
#'
#' @return Vector character. Ver ejemplos.
#'
#' @export
#'
#' @examples
#' cat("Numeros:", colapsar_secuencia(4:8), "\n")
#' cat("Numeros:", colapsar_secuencia(4:8, ", "), "\n")
#' cat("Numbers:", colapsar_secuencia(4:8, " & "), "\n")
#' cat("Numeros:", colapsar_secuencia(4:8, comillas = TRUE), "\n")
colapsar_secuencia <- function(x, conector = " y ", comillas = FALSE) {
  if (is.character(comillas)) {
    z <- comillas
  } else if (is.logical(comillas)) {
    z <- ifelse(comillas, "'", "")
  } else {
    stop("El argumento comillas debe ser character o logical")
  }

  if (length(x) == 0) {
    out <- ""
  } else if (length(x) == 1) {
    out <- as.character(x)
  } else {
    l <- length(x)
    out <- paste0(
      z,
      paste0(x[-l], collapse = paste0(z, ", ", z)),
      paste0(z, conector, z, x[l], z)
    )
  }
  return(out)
}

#' Demostración de informe para Laguna Merín
#'
#' @param extension character. Determina el formato de salida del informe: html,
#'   pdf o doc.
#'
#' @return Genera un archivo Rmd con un ejemplo de informe para Laguna Merín
#'
#' @export
#'
#' @examples
#'\dontrun{
#' demo_lm()
#' demo_lm('pdf')
#' demo_lm('doc')
#' }
demo_lm <- function(extension = c('html', 'pdf', 'doc')) {
  tipo <- match.arg(extension)
  demoname <- paste0('informe-laguna-merin-', tipo, '.Rmd')
  demofile <- system.file("examples", demoname, package = "siabox")
  rs <- grepl('rstudio', .Platform$GUI, ignore.case = TRUE) &
    RStudio.Version()$version >= '1.2.640'
  if (rs) {
    contenidos <- readLines(demofile, encoding = 'UTF-8')
    rstudioapi::documentNew(paste(contenidos, collapse = '\n'), 'rmarkdown')
  } else {
    i <- 0
    wdarch <- dir()
    while (any(wdarch == demoname)) {
      nstr <- stringr::str_pad(i, 2, pad = '0')
      demoname <- gsub('_*[0-9]{0,2}\\.Rmd', paste0('_', nstr, '.Rmd'), demoname)
      i <- i + 1
    }
    file.copy(demofile, demoname)
    message('ATENCI\u00d3N: Se cre\u00f3 el archivo "',
            demoname,
            '" en la carpeta de trabajo')
    file.edit(demoname)
  }
}
