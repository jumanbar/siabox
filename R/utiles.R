# README ----
#
# Funciones extras que pueden ser útiles pero no son fundamentales. En
# principio, mi idea es no exportarlas con el resto del paquete.
# 
# La única excepción es la función demo_lm

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
#' demo_lm()
#' demo_lm('pdf')
#' demo_lm('doc')
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
#' @param comillas Determina si se deben agregar comillas a los elementos de
#'   \code{x}
#'
#' @return
#'
#' @examples
#' cat("Numeros:", siabox:::colapsar_secuencia(4:8), "\n")
#' cat("Numeros:", siabox:::colapsar_secuencia(4:8, comillas = TRUE), "\n")
colapsar_secuencia <- function(x, conector = "y", comillas = FALSE) {
  z <- ifelse(comillas, "'", "")
  if (length(x) == 0) {
    out <- ""
  } else if (length(x) == 1) {
    out <- as.character(x)
  } else {
    l <- length(x)
    out <- paste0(
      z,
      paste0(x[-l], collapse = paste0(z, ", ", z)),
      paste0(z, " ", conector, " ", z, x[l], z)
    )
  }
  return(out)
}


#' Conexión con SIA
#'
#' Genera una conexión con la base de datos infambiental, usando el usuario y
#' clave de JMB. Sólo funciona en algunas PCs (ej.: servidor o PC de JMB).
#'
#' @return
#'
#' @examples
con_sia <- function() {
  # BASE DE DATOS SIA
  drv <- DBI::dbDriver("PostgreSQL")

  # *
  pw <- {
    "shiny_passwd"
  }
  out <- DBI::dbConnect(drv, dbname = "infambientalbd",
                        host = "172.20.0.34", port = 5432,
                        user = "shiny_usr", password = pw)

  if (grepl("DINAMA-OAN11", Sys.info()["nodename"], ignore.case = TRUE)) {
    # dbExecute(con, "SET CLIENT_ENCODING TO 'WIN1252';")
    DBI::dbExecute(out, "SET NAMES 'WIN1252';")
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
#'
#' @examples
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
