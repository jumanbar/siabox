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
#' det_nonum(v)
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
#' @param conector Caracter para conectar el ultimo elemento de la lista
#'   (tipicamente 'y', '&', 'and', etc...)
#'
#' @return
#'
#' @examples
#' cat("Numeros:", colapsar_secuencia(4:8), "\n")
#' cat("Numeros:", colapsar_secuencia(4:8, comillas = TRUE), "\n")
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
#' limpia_num("2.3")
#' limpia_num("2..3")
#' limpia_num("2,3")
#' limpia_num("2,,,,3")
#' limpia_num("   2,,,, 3   ")
limpia_num <- function(x) {
  out <- x %>%
    # stringr::str_trim() %>%
    stringr::str_replace_all("[.,]+", ".") %>%
    stringr::str_replace_all("\\s+", "")
  return(out)
}

#' Buscador de parámetros
#'
#' Buscador de id_parametro según un texto (un google de id_parametros)
#'
#' @param patron character. Patrón (expresión regular tipo
#'   \code{\link[base]{regex}}).
#' @param ... Argumentos pasados a \code{\link[base]{agrepl}}
#'
#' @return
#'
#' @examples
#' param_id("fósforo")
#' param_id("pt", max.distance = 0)
param_id <- function(patron, ...) {

  patron <- toascii(patron)

  resA <- agrepl(patron, toascii(sia_parametro$nombre_clave),
                 ignore.case = TRUE,
                 ...)

  resB <- agrepl(patron, toascii(sia_parametro$parametro),
                 ignore.case = TRUE,
                 ...)

  sia_parametro[resA | resB,]
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
#' parentesis(3147:3152)
#' parentesis(3147:3152, "-<<<- |", "| ->>>-")
#' parentesis(3147:3152, comillas = TRUE)
#' parentesis(c("gato", "perro"))
#' parentesis(c("gato", "perro"), comillas = TRUE)
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
#' toascii(c("sabañón!", "ungüento", "nâo è graça"))
toascii <- function(texto) {
  stringi::stri_trans_general(texto, "Latin-ASCII")
}

#' Summary de una columna en formato tabla
#'
#' Función alternativa a \code{\link[base]{summary}}, pensada para trabajar
#' fluidamente con tablas de datos y el tidyverse.
#'
#' @param .data Tabla (\code{\link[base]{data.frame}}) con datos.
#' @param columna Columna numerica de \code{.data}, escrita sin comillas.
#'
#' @return Devuelve un summary de los valores de la columna en cuestion,
#'   ordenado de forma tal que cada estadistico es una columna. Los estadisticos
#'   tomados son: n (numero de valores), Min (minimo), '1er Cu' (primer
#'   cuartil), Media (promedio), Mediana (i.e.: segundo cuartil), '3er Cu'
#'   (tercer cuartil) y Max (maximo).
#'
#'   En caso de que `.data` sea de clase \code{\link[dplyr]{grouped_df}},
#'   devuelve una fila por cada grupo de `.data` con los estadisticos
#'   correspondientes.
#' @export
#'
#' @examples
#' tsummary(tibble(x = c(3, 3, 5, 2.2, 1, 3.4, 7.6)), x)
#' datos_sia %>% tsummary(valor)
#' datos_sia %>% dplyr::group_by(id_parametro) %>% tsummary(valor)
#' # Puede demorar unos segundos:
#' datos_sia %>%
#'   dplyr::filter(id_programa %in% c(4L, 7L)) %>%
#'   dplyr::group_by(id_parametro, id_programa, id_estacion) %>%
#'   tsummary(valor) %>%
#'   dplyr::left_join(sia_estacion[c(1, 8)], by = c("id_estacion" = "id")) %>%
#'   dplyr::left_join(sia_parametro)
tsummary <- function(.data, columna) {
  # grupo <- quos(...)
  columna <- dplyr::enquo(columna)
  .data %>%
    # select(!!!grupo,!!columna) %>%
    # dplyr::filter(!is.na(!!columna)) %>%
    # group_by(!!!grupo) %>%
    dplyr::summarise(
      n = dplyr::n(),
      Min = min(!!columna, na.rm = TRUE),
      `1er Cu` = quantile(!!columna, na.rm = TRUE, probs = .25),
      Media = mean(!!columna, na.rm = TRUE) %>% round(ifelse(. < 1, 5, 3)),
      Mediana = median(!!columna, na.rm = TRUE),
      `3er Cu` = quantile(!!columna, na.rm = TRUE, probs = .75),
      Max = max(!!columna, na.rm = TRUE)
      # NAs = sum(is.na(!!columna))
    ) %>%
    dplyr::filter(!is.na(Mediana))
}
# tsummary(tnum, Conduc)
# tnum %>% group_by(Programa) %>% tsummary(Conduc)
