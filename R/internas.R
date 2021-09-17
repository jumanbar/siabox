# README ----
#
# Colección medio miscelánea.
#
# - det_nonum es muy importante
#
# . . . . . . . . .  . . . . .  .  . . . . . . . . -----

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
