# README ----
#
# Funciones extras que pueden ser útiles pero no son fundamentales. En
# principio, mi idea es no exportarlas con el resto del paquete.
#
# Las excepciones son las funciones demo_lm y colapsar_secuencia

# . . . . . . . . .  . . . . .  .  . . . . . . . . -----
#
# EXPORT ----

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

# . . . . . . . . .  . . . . .  .  . . . . . . . . -----
#
# INTERNAL ----

#' Determinar cuáles son no numéricos
#'
#' Determina si un valor es no numérico. Excluye valores escritos con notación
#' científica.
#'
#' @param v Character con valores potencialmente convertibles a numéricos. No
#'   debe haber espacios en blanco en ninguno de los valores.
#'
#' @return
#'
#' @examples
#' v <- c("1.347e4", "<LC", "<78", "14.447", "7", "1,3E+02")
#' siabox:::det_nonum(v)
det_nonum <- function(v) {
  cientif <- grepl("^[[:digit:]]+[,\\.]*[[:digit:]]*([Ee][+-]*[[:digit:]]+)*$",
                   v, ignore.case = TRUE)
  comun   <- grepl("^[[:digit:]]+[,\\.]*[[:digit:]]*$", v)
  # desubic <- grepl("[^[:digit:]]", v)
  return(!cientif & !comun)
}

#' Conexión con SIA
#'
#' Genera una conexión con la base de datos infambiental, usando el usuario y
#' clave de JMB. Sólo funciona en algunas PCs (ej.: servidor o PC de JMB).
#'
#' @return Una conexión tipo DBI de PosgtgreSQL
#' @examples
#' \dontrun{
#' con <- con_sia()
#' sia_parametro <- dplyr::tbl(con, "parametro")
#' }
con_sia <- function() {
  pw <- {
    "shiny_passwd"
  }

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("El paquete \"DBI\" es necesario para esta funci\u00f3n.",
         call. = FALSE)
  }

  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("El paquete \"RPostgres\" es necesario para esta funci\u00f3n.",
         call. = FALSE)
  }

  nodo <- Sys.info()["nodename"]
  # Determinar en qué computadora se están ejecutando los comandos:
  prueba <- grepl("MA112|DINAMA", nodo, ignore.case = TRUE)

  out <- if (prueba) {
    DBI::dbConnect(RPostgres::Postgres(),
                   user = "shiny_usr",
                   password = pw,
                   host = "172.20.0.34",
                   port = 5432,
                   dbname = "infambientalbd")
  } else {
    DBI::dbConnect(RPostgres::Postgres(),
                   dbname = 'infambientalbd',
                   host = 'localhost',
                   port = 5432,
                   user = 'juan',
                   password = 'shiny')
  }

  return(out)
}

#' Limpiar texto con valores numéricos
#'
#' Quita espacios en blanco, y sustituye comas, comas
#' repetidas y puntos repetidos por un único punto (indicador de decimales). Se
#' basa en expresiones regulares y el paquete \code{stringr}.
#'
#' @param x character. Valores numéricos expresados en varias formas posibles.
#'
#' @return
#'
#' @examples
#' siabox:::limpia_num("2.3")
#' siabox:::limpia_num("2..3")
#' siabox:::limpia_num("2,3")
#' siabox:::limpia_num("2,,,,3")
#' siabox:::limpia_num("   2,,,, 3   ")
limpia_num <- function(x) {
  out <- x %>%
    # stringr::str_trim() %>%
    stringr::str_replace_all("[.,]+", ".") %>%
    stringr::str_replace_all("\\s+", "")
  return(out)
}

#' Auxiliar para `consulta_muestras`
#'
#' Crea un texto con la secuencia de elementos entre paréntesis y separados por
#' comas.
#'
#' @param v Vector con elementos a concatenar dentro del paréntesis.
#' @param ini Inicio del paréntesis.
#' @param fin Fin del paréntesis.
#' @param comillas logical (1 valor): si poner o no comillas (simples) alrededor
#'   de cada elemento de `v`
#'
#' @return
#'
#' @examples
#' siabox:::parentesis(3147:3152)
#' siabox:::parentesis(3147:3152, "-<<<- |", "| ->>>-")
#' siabox:::parentesis(3147:3152, comillas = TRUE)
#' siabox:::parentesis(c("gato", "perro"))
#' siabox:::parentesis(c("gato", "perro"), comillas = TRUE)
parentesis <- function(v, ini = "(", fin = ")", comillas = FALSE) {
  if (comillas)
    v <- paste0("'", v, "'")

  paste0(ini, paste(v, collapse = ", "), fin)
}

#' Pegar observaciones
#'
#' Pensada para trabajar internamente en \code{link{consulta_muestras}} y en la
#' creación de \code{link{datos_sia}} (en código de carpeta data-raw)
#'
#' @param obs_a character. Vector de observaciones.
#' @param obs_b character. Vector de observaciones.
#'
#' @return Vector character con observaciones de uno u otro vector, o, en caso
#'   de que ambos tengan, observacones pegadas.
#'
#' @examples
#' a <- c("", NA, "Nubes", "Sin calibrar", "")
#' b <- c("Lluvia", NA, NA, "Con cuchillo", "")
#' data.frame(a, b, pegar_obs(a, b))
pegar_obs <- function(obs_a, obs_b) {
  hay_a <- obs_a != "" & !is.na(obs_a)
  hay_b <- obs_b != "" & !is.na(obs_b)
  out <- dplyr::case_when(
    hay_a & !hay_b ~ obs_a,
    !hay_a & hay_b ~ obs_b,
    hay_a & hay_b ~ paste0(obs_a, '. ', obs_b),
    TRUE ~ NA_character_
  )
  return(out)
}

#' Codificar columnas con UTF-8
#'
#' Tomada de esta respuesta en SO: https://stackoverflow.com/a/41900818/1030523.
#' Notar que tal vez sea buena idea usar RPostgres en vez de RPostgreSQL para
#' evitar el problema original.
#'
#' @param x data.frame con columnas character para codificar como UTF-8
#'
#' @return data.frame con los mismos datos pero con columnas character
#'   codificadas con UTF-8
set_utf8 <- function(x) {
  # cat("=== set_utf8:\n")
  # Declare UTF-8 encoding on all character columns:
  chr <- sapply(x, is.character)

  if (sum(chr))
    x[, chr] <- lapply(x[, chr, drop = FALSE], `Encoding<-`, "UTF-8")

  # Same on column names:
  Encoding(names(x)) <- "UTF-8"
  x
}

#' Convertir texto en ASCII
#'
#' @param texto Un vector atomico character.
#'
#' @return Un vector atomico character, en el que todos los tildes, acentos,
#'         dieresis, etc, son sustituidos por su version ASCII.
#'
#' @examples
#' siabox:::toascii(c("sabañón!", "ungüento", "nâo è graça"))
toascii <- function(texto) {
  stringi::stri_trans_general(texto, "Latin-ASCII")
}


#' Obtener unicode escapes
#'
#' @param x Caracter a convertir a escape de unicode
#'
#' @return Código unicode con escape.
#'
#' @examples
#' siabox:::unicode("é")
unicode <- function(x) {
  stringi::stri_escape_unicode(x)
}
