# INFO ----
#
# Estas son funciones que se usan solamente para crear informes o gráficos
# automatizados.



# UTILES ----

# + Calculos ----

#' Índice de Estado Trófico
#'
#' Calcula valores del IET a partir de la concentración de Fósforo Total (PT),
#' en microgramos por litro.
#'
#' Cálculo: \deqn{IET = 10 \times (6-\frac{0.42-0.36 \times ln(\overline{PT%
#' (\mu g/L)})}{ln(2)})-20}{% IET = 10 (6 - (.42 - .36 log(PT \mu g/L)) /
#' log(2)) - 20}
#'
#' @param x numeric: valores de concentración de fósforo total, en microgramos
#'   por litro.
#'
#' @return Un vector numérico con los valores del IET.
#'
#'
#' @export
#'
#' @examples
#' iet(25)
#' iet(rlnorm(10, meanlog = 3, 1))
iet <- function(x) {
  10 * (6 - (0.42 - 0.36 * log(x)) / log(2)) - 20
}

#' Media geométrica
#'
#' Calcula la media geométrica de un vector numérico.
#'
#' Es la raíz \code{n}-ésima del producto de todos los números contenidos en
#' \code{x}, siendo \code{n} = \code{length(x)}.
#'
#' En general puede considerarse como el resultado de \code{prod(x) ^ (1 /
#' length(x))}, aunque en lugar de usar \code{^(1/length(x))} se usa la función
#' \code{\link{raiz}}, diseñada para contemplar casos particulares.
#'
#' @seealso \href{https://es.wikipedia.org/wiki/Media_geom%C3%A9trica}{Artículo
#'   en Wikipedia}, \code{\link{raiz}}
#'
#' @keywords arith univar
#'
#' @param x numeric
#'
#' @return Valor numérico equivalente a la media geométrica de x
#'
#' @export
#'
#' @examples
#' media_geom(c(3, -2.34, 5427))
#' media_geom(rnorm(6))
#' media_geom(rnorm(5))
media_geom <- function(x) {

  # Media geométrica de x:
  raiz(x = prod(x, na.rm = TRUE), n = length(x))

}

#' Raíz enésima
#'
#' @param x numeric.
#' @param n integer, valor único.
#'
#' @return Devuelve la raíz enésima de un valor \code{x}, incluso cuando ese
#' valor es negativo, si es que \code{n} es impar. En caso de que \code{n} sea
#' par y \code{x} negativo, devueve \code{NaN}. Ver ejemplo para comparar con
#' \code{x ^ (1 / n)} y con \code{abs(x) ^ (1 / n)}.
#'
#' @keywords arith univar
#'
#' @seealso \code{\link{media_geom}}
#'
#' @export
#'
#' @examples
#' d <- data.frame(x = c(4, -4, 4, -4), n = c(2, 2, 3, 3))
#' dplyr::mutate(d, raiz(x, n), x ^ (1 / n), abs(x) ^ (1 / n))
raiz <- function(x, n) {
  #else: x negativo y n par
  ifelse(x > 0 | n %% 2 == 1, sign(x) * abs(x) ^ (1 / n), NaN)
}

# GAFICOS ----

# + Para informes -----

#' Crea o modifica tabla de etiquetas
#'
#' Sin argumentos definidos, genera una tabla de etiquetas estándar, basada en
#' las tablas del SIA (infambientalbd): \code{sia_parametro},
#' \code{sia_param_unidad} y \code{sia_unidad}. Con argumentos, agrega o
#' modifica las etiquetas de uno o más parámetros.
#'
#' La idea de la tabla de etiquetas es generar una referencia que se usará en
#' funciones para crear gráficos, tablas, etc...
#'
#' @seealso \code{\link{t_eti_base}}
#'
#' @param id_parametro Vector integer
#' @param etiqueta Vector character o expression
#'
#' @return
#' @export
#'
#' @examples
#' # Sin ninguna etiqueta nueva:
#' t_eti <- t_eti_add()
#' plot(0, 0, xlab = dplyr::filter(t_eti, id_parametro == 2005)[[2]])
#'
#' # Modificando un parámetro existente:
#' t_eti <- t_eti_add(2005L, expression('AlcT (mg CaCO' [3] * ' /L)'))
#' plot(0, 0, xlab = dplyr::filter(t_eti, id_parametro == 2005)[[2]])
#'
#' # Modificando un parámetro existente, agregando uno nuevo y mezclando
#' # etiquetas del tipo expression y character:
#' t_eti <- t_eti_add(
#'   c(2005L, 9999L),
#'   c(expression('AlcT (mg CaCO' [3] * ' /L)'), "YOLO (Mb/min)")
#' )
#' plot(0, 0,
#'      xlab = dplyr::filter(t_eti, id_parametro == 2005L)[[2]],
#'      ylab = dplyr::filter(t_eti, id_parametro == 9999L)[[2]])
t_eti_add <- function(id_parametro = NULL, etiqueta) {

  out <- t_eti_base
  if (is.null(id_parametro))
    return(out)

  if (!is.integer(id_parametro)) {
    id_parametro <- as.integer(id_parametro)
    warning("id_parametro no es integer --> se coerciona a integer")
  }

  out <- dplyr::filter(out, !(id_parametro %in% !!id_parametro))

  out <- if (is.expression(etiqueta)) {
    p <- out[[1]]
    e <- as.list(out[[2]])
    for (i in 1:length(id_parametro)) {
      w <- which(p == id_parametro[i])
      if (length(w)) {
        e[[w]] <- etiqueta[i]
      } else {
        p <- c(p, id_parametro[i])
        e <- c(e, etiqueta[i])
      }
    }
    tibble::tibble(id_parametro = p, etiqueta = e)
  } else dplyr::add_row(out,
                        id_parametro = !!id_parametro,
                        etiqueta = !!etiqueta)
  return(out)
}

#' Crear etiquetas para gráficos
#'
#' Devuelve la etiqueta para el parámetro seleccionado por su \code{id}, basado
#' en la tabla \code{t_eti} (ver detalles).
#'
#' Si no se provee una tabla \code{t_eti}, la función usará \code{t_eti_add()}
#' como tabla de referencia.
#'
#' @seealso \code{\link{t_eti_add}}
#'
#' @param id_parametro Escalar (integer). Número de id de parámetro (i.e.:
#'   \code{id_parametro}).
#'
#' @param t_eti data.frame. Tabla generada con \code{\link{t_eti_add}}
#'
#' @return
#' @export
#'
#' @examples
#' e <- eti(2101, tibble::tibble(
#'   id_parametro = 2101,
#'   etiqueta = expression('NO'[2]*' (mg NO'[2] * '-N/L)')
#' ))
#' ggplot() + geom_blank() + xlab(eti(2101))
#' ggplot() + geom_blank() + xlab(e)
eti <- function(id_parametro, t_eti) {

  if (missing(t_eti)) {
    t_eti <- t_eti_base
    warning("Argumento t_eti faltante: se usa la tabla base (ver ?t_eti_base)")
  }

  w <- which(t_eti$id_parametro == id_parametro[[1]])
  if (length(w))
    return(t_eti$etiqueta[[w]]) else
      stop("No se encontró el parámetro ", "(id = ", id_parametro,
           ") en la tabla t_eti")
}

#' Graficos de parámetros por mes
#'
#' `g_mes` grafica los valores de un parámetro por mes. `g_mes_all` en escencia
#' llama a \code{\link{g_mes}} tantas veces como id de parámetros en
#' `id_parametro`, realizando una figura única que combina los gráficos de los
#' parámetros individuales.

#' @param .data data.frame. Datos del SIA
#' @param id_parametro integer. Id de uno/escalar (`g_mes`) o varios elementos
#'   (`g_mes_all`). Se debe(n) corresponder con los ids encontrados columna
#'   homónima de \code{\link{sia_parametro}}
#' @param pos_leyenda character. Valor enviado directamente al argumento
#'   `legend.position` de la función \code{\link[ggplot2]{theme}}
#' @param nombre_clave_param character. Opcional. Nombre clave del parámetro
#'   (ej.: "PT"). Se corresponde con la columna `nombre_clave` de
#'   \code{\link{sia_parametro}}
#' @param t_eti `tbl_df`. Opcional. Tabla de etiquetas
#' @param ylab Opcional. Etiqueta para el eje y del gráfico resultante. Si este
#'   argumento no es especificado, se utilizará la función \code{eti} para crear
#'   la etiqueta.
#' @param ... Argumentos adicionales para pasar a
#'   \code{\link[patchwork]{wrap_plots}}
#'
#' @details Requiere de la tabla decreto para agregar las líneas horizontales
#'   correspondientes a los valores límites aceptados para la clase 1 de aguas
#'   (decreto 253/79).
#' @return Objeto de clase `gg` (hereda de \code{\link[ggplot2]{ggplot}}).
#'
#' @import ggplot2
#' @return
#' @export
#'
#' @seealso \code{\link{t_eti_base}}
#'
#' @examples
#' p <- c(PT=2098, NT=2102, ST=2028, Conduc=2009)
#' dfo <- data.frame(mes = rep(3:6, 4),
#'                   codigo_pto = rep(c("RN1", "RN2", "RN3", "RN5"), each = 4),
#'                   id_parametro = rep(p, each = 4*4),
#'                   valor = c(rnorm(4*4, mean = 30, sd = 5),
#'                             rnorm(4*4, mean = 55, sd = 15),
#'                             rnorm(4*4, mean = 315, sd = 50),
#'                             rnorm(4*4, mean = 500, sd = 80)))
#' dfo$valor[sample(4*4*4, 5)] <- NA
#' dplyr::filter(dfo, is.na(valor))
#' g_mes(dfo, id_parametro = 2009, ylab = 'Cond (μS/cm)')
#' g_mes(dfo, nombre_clave_param = 'Conduc', ylab = 'Cond (μS/cm)')
#' g_mes_all(dfo, id_parametro = p)
#' g_mes_all(dfo, id_parametro = 2098L)
#' g_mes_all(dfo, id_parametro = 2008L) # Vacío
#' g_mes_all(dfo, id_parametro = c(p, 2008L)) # Vacío
g_mes <- function(.data, id_parametro = NULL, pos_leyenda = 'none',
                  nombre_clave_param, ylab = NULL, t_eti) {

  if (missing(t_eti)) {
    t_eti <- t_eti_base
    warning("Se usa t_eti_base como sustituta del argumento t_eti")
  }
  id_parametro <- if (is.null(id_parametro)) {
    dplyr::filter(sia_parametro,
                  nombre_clave == nombre_clave_param)$id_parametro
  } else id_parametro

  if (is.null(ylab))
    ylab <- eti(id_parametro, t_eti)

  out <-
    .data %>%
    dplyr::filter(id_parametro == !!id_parametro) %>%
    ggplot() +
    aes(mes, valor, color = codigo_pto) +
    geom_jitter(width = 0.1, alpha = 0.7) +
    labs(x = NULL, y = ylab) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 10),
          axis.text.x = element_text(angle = 0, vjust = 1),
          legend.position = pos_leyenda)

  if (!nrow(out$data)) {
    corte <-
      sia_parametro %>%
      dplyr::filter(id_parametro == !!id_parametro) %>%
      dplyr::left_join(codigos_param, by = "id_parametro")

    texto_par <- corte$parametro.x

    out <-
      ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = paste0(texto_par, ":\nNO HAY DATOS"))) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            text = element_text(size = 10),
            axis.text = element_blank(),
            legend.position = pos_leyenda) +
      xlab(NULL) +
      ylab(NULL)
    return(out)
  }

  dec <- dplyr::filter(decreto, id_parametro == !!id_parametro,
                       clase == "1",
                       !is.na(valor))
  if (nrow(dec)) {
    dec <- dplyr::mutate(dec, valor = coef_conversion * valor)
    out <- out +
      geom_hline(yintercept = dec$valor, linetype = "longdash", color = 'red')
  }

  return(out)
}

#' @describeIn g_mes Varios gráficos de valores de parámetros por mes en una
#'   misma figura
g_mes_all <- function(.data, id_parametro, t_eti, ...) {

  if (missing(t_eti)) {
    t_eti <- t_eti_base
    warning("Se usa t_eti_base como sustituta del argumento t_eti")
  }

  lista <- vector(mode = "list", length = length(id_parametro))
  # for (i in 1:(length(id_parametro) - 1))
  for (i in 1:(length(id_parametro)))
    lista[[i]] <- g_mes(.data, id_parametro = id_parametro[i], t_eti = t_eti)

  lista[[i]] <-
    lista[[i]] +
    theme_update(legend.position = "bottom") +
    guides(colour = guide_legend("Estación", title.position = 'left',
                                 direction = "horizontal",
                                 label.hjust = .5,
                                 label.vjust = .5,
                                 nrow = 1)) +
    theme_bw()

  out <-
    patchwork::wrap_plots(lista, guides = 'collect', ...) +
    patchwork::plot_annotation(tag_levels = 'A')

  print(out)
}

g_comp_est <- function(.data, nombre_clave_param,
                       id_parametro = NULL, ylab = NULL, t_eti) {

  if (missing(t_eti)) {
    t_eti <- t_eti_base
    warning("Se usa t_eti_base como sustituta del argumento t_eti")
  }

  id <- if (is.null(id_parametro))
    dplyr::filter(sia_parametro, nombre_clave == nombre_clave_param)$id_parametro else
      id_parametro

  if (is.null(ylab))
    ylab <- eti(id, t_eti)

  out <-
    .data %>%
    dplyr::filter(id_parametro == id) %>%
    group_by(codigo_pto) %>%
    summarise(Media = mean(valor),
              sd = sd(valor)) %>%
    mutate(ymin = Media - sd,
           ymax = Media + sd) %>%
    ggplot() +
    aes(x = codigo_pto, y = Media) +
    geom_errorbar(aes(ymin = Media - sd, ymax = Media + sd), width = .3,
                  position = position_dodge(.5), size = .5) +
    geom_point(position = position_dodge(.5), size = 2, alpha = .7)+
    labs(y = ylab, x = NULL, colour = "Cuenca")+
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 10),
          axis.text.x = element_text(angle=30, vjust=1))

  dec <- dplyr::filter(decreto, id_parametro == id & clase == "1")

  if (nrow(dec)) {
    out <- out +
      geom_hline(yintercept = dec$valor, linetype = "longdash", color = 'red')
  }

  return(out)

}

g_comp_est_all <- function(.data, id_parametro, ...) {

  lista <- vector(mode = "list", length = length(id_parametro))
  # for (i in 1:(length(id_parametro) - 1))
  for (i in 1:(length(id_parametro)))
    lista[[i]] <- g_comp_est(.data, id_parametro = id_parametro[i])

  # lista[[i]] <-
  #   lista[[i]] +
  #   theme_update(legend.position = "bottom") +
  #   guides(colour = guide_legend("Estación", title.position = 'left',
  #                                direction = "horizontal",
  #                                label.hjust = .5,
  #                                label.vjust = .5,
  #                                nrow = 1)) +
  #   theme_bw()

  out <- patchwork::wrap_plots(lista, ...) +
    patchwork::plot_annotation(tag_levels = 'A')

  print(out)
}



#' Gráfico de comparación entre cuencas
#'
#' @param .data data.frame. Datos del SIA
#' @param nombre_clave_param character. Nombre clave del parámetro (ej.: "PT").
#'   Se corresponde con la columna `nombre_clave` de \code{\link{sia_parametro}}
#' @param id_parametro integer. Id del parámetro. Se corresponde con la columna
#'   homónima de \code{\link{sia_parametro}}
#' @param ylab character. Etiqueta opcional para usar en la gráfica
#'
#' @return
#' @export
#'
#' @examples
#' d <- filtrar_datos(datos_sia, id_programa = 10L,
#'                    rango_fechas = c(2017, 2019),
#'                    id_parametro = 2098)
#' g_comp_cue(d, "PT")
#'
#' e <- filtrar_datos(datos_sia, id_programa = 4L,
#'                    rango_fechas = c(2017, 2019),
#'                    id_parametro = 2098)
#' g_comp_cue(e, "PT")
g_comp_cue <- function(.data,
                       nombre_clave_param,
                       id_parametro = NULL,
                       ylab = NULL) {
  id <- if (is.null(id_parametro))
    dplyr::filter(sia_parametro,
                  nombre_clave == nombre_clave_param)$id_parametro else
      id_parametro

  if (is.null(ylab))
    ylab <- eti(id)

  out <-
    .data %>%
    dplyr::filter(id_parametro == id) %>%
    ggplot() +
    geom_boxplot(fill = "lightblue", outlier.size = 1.2,
                 outlier.fill = "#000000") +
    labs(x = NULL, y = ylab) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size=10),
          axis.text.x = element_text(angle=90, vjust=1))

  out <- if (all(is.na(.data$nombre_subcuenca_informes))) {
    out + aes(sub_cue_nombre, valor)
    warning("Usando sub_cue_nombre en lugar de nombre_subcuenca_informes ",
            "debido a que no se encontraron valores válidos para la última")
  } else out + aes(nombre_subcuenca_informes, valor)

  return(out)
}


#' Gráfico de IET
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
g_iet <- function(.data) {
  out <-
    .data %>%
    ggplot() +
    aes(x = codigo_pto, y = IET) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = c(0, 47, 52, 59, 63), ymax = c(47, 52, 59, 63, 67),
             fill = c('lightgrey', 'lightblue', 'green', 'yellow', 'orange'),
             alpha = .5) +
    annotate("rect",
             xmin = -Inf, xmax = Inf,
             ymin = 67, ymax = Inf,
             fill = 'red',
             alpha = .5) +
    annotate('text', x = 7.5, y = c(46.5, 51.5, 58.5, 62.5, 66.5, 69.5),
             label = c('Ultraoligotrófico', 'Oligotrófico', 'Mesotrófico',
                       'Eutrófico', 'Supereutrófico', 'Hipereutrófico'),
             alpha = 0.6) +
    geom_segment(aes(x=codigo_pto, xend=codigo_pto, y=0, yend=IET)) +
    geom_point(size=1.5, with=0.1) +
    # scale_shape_manual(values = c(15, 0, 1, 3, 17))+
    scale_y_continuous(limits = c(45, 70)) +
    scale_x_discrete() +
    labs(y='IET', x='Estaciones')+
    theme_bw()
  return(out)
}

# + Graficos sueltos ----

#' Graficar valores anuales longitudinales
#'
#' Gráfica los valores de los parámetros por estaciones, ordenadas en orden de
#' la cuenca
#'
#' @param .data data.frame. Datos del SIA
#' @param id_parametro integer.
#' @param anio
#' @param colores_meses
#'
#' @return
#' @export
#'
#' @examples
#' d <- readRDS("tmp/datos_cuareim.rds")
#' decreto <- readRDS("../data/decreto.rds")
#' d$mes <- as.integer(d$mes)
#' d$anio <- as.integer(d$anio)
#' g_long(d, 2032, 2019L, horiz = c(min = 5, max = 28))
g_long <- function(.data, id_parametro, anio,
                      colores_meses = scales::hue_pal()(12),
                      horiz = NULL) {
  require(dplyr)
  require(magrittr)
  require(lubridate)

  # if (id_parametro == 2032L)
  #   save(.data, id_parametro, anio, colores_meses, file = "tmp/g_long.RData")

  .data <- dplyr::filter(.data, id_parametro == !!id_parametro)

  tmp <- dplyr::filter(.data, anio == !!anio)

  if (!nrow(tmp)) return(NULL)

  # Datos del año seleccionado, con valores promedio por estación y mes
  # (típicamente hay un único valor por estación y por mes):
  d_anio <- tmp %>%
    group_by(id_estacion, codigo_pto, mes) %>%
    summarise(valor = mean(valor)) %>%
    mutate(peri = factor(mes, levels = 1:12,
                         labels = abr_meses, ordered = TRUE)) %>%

             # month(mes, label = TRUE)) %>%
    ungroup()

  levels(d_anio$peri) <- str_to_title(levels(d_anio$peri))

  # Para que los meses tengan siempre los mismos colores (ver "valores", abajo):
  meses <- unique(sort(d_anio$mes))

  # Etiquetas para los meses:
  eti_meses <- levels(d_anio$peri)[meses]

  # Id de las estaciones que nos interesan (solamente las que tienen datos para
  # el año seleccionado):
  id_est <- d_anio %>%
    dplyr::filter(anio == !!anio) %>%
    pull(id_estacion) %>%
    unique %>%
    sort

  # Datos anuales, promediados:
  d_anio_prom <- d_anio %>%
    group_by(id_estacion, codigo_pto) %>%
    summarise(valor = mean(valor)) %>%
    mutate(peri = as.character(!!anio),
           mes = -1) %>%
    ungroup()

  d_fin <- bind_rows(mutate(d_anio, peri = as.character(peri)),
                     d_anio_prom)

  # Datos del año anterior:
  tmp <- .data %>%
    dplyr::filter(anio == !!anio - 1L, id_estacion %in% id_est)

  eti_anterior <- NULL
  if (nrow(tmp)) {
    eti_anterior <- as.character(anio - 1L)
    d_anterior <- tmp %>%
      group_by(id_estacion, codigo_pto) %>%
      summarise(valor = mean(valor)) %>%
      mutate(peri = eti_anterior, mes = -1) %>%
      ungroup()
    d_fin <- bind_rows(d_fin, d_anterior)
  }

  # Datos del último lustro:
  eti_lustro <- NULL
  if (min(.data$anio) < anio - 1L) {
    # Etiqueta para la leyenda:
    eti_lustro <- paste0(min(.data$anio), "-", anio - 1L)
    d_lustro <- .data %>%
      dplyr::filter(anio >= min(.data$anio),
                    anio <= !!anio - 1L,
                    id_estacion %in% id_est) %>%
      group_by(id_estacion, codigo_pto) %>%
      summarise(valor = mean(valor)) %>%
      mutate(peri = eti_lustro, mes = -1) %>%
      ungroup()

    d_fin <- bind_rows(d_fin, d_lustro)
  }
  cortes <- c(eti_meses, eti_lustro, eti_anterior, as.character(anio))

  valores_color <- c(
    # Para que los colores sean consistentes entre diferentes gráficas
    # (especialmente cuando g_long es llamada por g_long_files):
    colores_meses[meses],
    # El promedio de los 5 (o n) años anteriores, por estación, en gris:
    if (!is.null(eti_lustro)) "#a6a6a6" else NULL,
    # Valores promedio por estación del año anterior en línea punteada negra:
    if (!is.null(eti_anterior)) "#000000" else NULL,
    # Valores promedio anual, por estación:
    "#000000"
    )

  valores_linea <- c(
    # Sin lineas para los valores mensuales:
    rep.int("blank", length(eti_meses)),
    # El promedio de los 5 (o n) años anteriores, en línea continua:
    if (!is.null(eti_lustro)) "solid" else NULL,
    # Valores promedio por estación del año anterior en línea punteada:
    if (!is.null(eti_anterior)) "dotted" else NULL,
    # El promedio anual por estación, en línea continua:
    "solid")

  valores_punto <- c(
    # Valores mensuales, en círculos:
    rep.int(19, length(eti_meses)),
    # El promedio de los 5 (o n) años anteriores, con círculos:
    if (!is.null(eti_lustro)) 17 else NULL,
    # Valores promedio por estación del año anterior, con triángulos:
    if (!is.null(eti_anterior)) 1 else NULL,
    # El promedio anual por estación, con círculos:
    19)

  g <-
    d_fin %>%
    ggplot() +
    aes(codigo_pto, valor,
        shape = peri,
        color = peri,
        linetype = peri,
        group = peri) +
    geom_point() +
    geom_line() +
    # geom_line(data = d_lustro,    aes(codigo_pto, valor, group = peri)) +
    # geom_line(data = d_anterior,  aes(codigo_pto, valor, group = peri),
    #           color = "#000000") +
    # geom_line(data = d_anio_prom, aes(codigo_pto, valor, group = peri),
    #           color = "#000000", linetype = "dashed") +
    # guides(color = guide_legend(title = NULL)) +
    xlab("Estación") +
    ylab(eti(id_parametro)) +
    theme_bw() +
    theme(legend.position = "bottom")

  if (!is.null(horiz)) {

    rango_y <- ggplot_build(g)$layout$panel_params[[1]]$y.range

    w <- which(horiz >= rango_y[1] & horiz <= rango_y[2])

    if (length(w)) {
      txt <- "Decreto 253/79"
      th <- data.frame(peri = txt, valor = horiz[w])

      g <- g + geom_hline(aes(yintercept = valor, alpha = peri),
                          color = "#990000", data = th) +
        scale_alpha_manual(name = NULL, values = 1, breaks = txt)

      if (!is.null(names(horiz[w]))) {
        rango_x <- ggplot_build(g)$layout$panel_params[[1]]$x.range
        g <- g + annotate("label", x = rango_x[2], y = horiz[w],
                          label = names(horiz[w]), fill = "#9b4b4b",
                          hjust = "inward", label.r = unit(0.2, "lines"),
                          label.size = 0, colour = "white", fontface = "bold"
                          )
      }
    }
  }

  g <- g +
    scale_linetype_manual(name = NULL, values = valores_linea, breaks = cortes)+
    scale_color_manual(   name = NULL, values = valores_color, breaks = cortes)+
    scale_shape_manual(   name = NULL, values = valores_punto, breaks = cortes)

  return(g)
}

#' Hacer archivos con gráficos g_long
#'
#' @param .data
#' @param id_programa
#' @param anio
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' d <- readRDS("tmp/datos_cuareim.rds") %>%
#'   dplyr::filter(id_parametro %in% c(2035, 2017:2018, 2091, 2098, 2111))
#' h <- dplyr::filter(decreto, clase == "1", !is.na(valor))
#' g_long_files(d, 2019L, tabla_horiz = h, path = "tmp")
g_long_files <- function(.data, anio, tabla_horiz = NULL, path = NULL) {

  # save(.data, id_programa, anio, path, file = "tmp/g_long_files.RData")

  # if (is.null(id_prog)) {
  #   prog <- adiv_prog(unique(.data$codigo_pto))
  #   id_prog <- prog$id_programa
  #   warning("Se estimó que la muestra proviene del programa de monitoreo '",
  #           prog$nombre_programa, "' en base a los nombres de las estaciones ",
  #           "presentes en los datos.")
  # }

  # Para pruebas:
  # id_par <- c(2032L, 2009, 2098, 2097, 2105)
  id_par <- sort(unique(.data$id_parametro))

  directorio <- if (is.null(path)) tempdir() else path

  out <- character(n <- length(id_par))

  archivos <-
    tibble(id_parametro = id_par) %>%
    left_join(sia_parametro, by = "id_parametro") %>%
    mutate(out = nombre_clave %>%
             toascii() %>%
             str_replace_all("[^[:alnum:]]", "_") %>%
             str_replace_all("_+", "_") %>%
             paste0("grafico_", ., "_", anio,".png")) %>%
    pull(out)

  paleta <- scales::hue_pal()(12)

  withProgress(message = "Preparando gráficas...", value = 0, min = 0, max = 1,
               expr = {
    for (i in 1:n) {

      horiz <- NULL
      if (!is.null(tabla_horiz)) {
        v <- dplyr::filter(tabla_horiz, id_parametro == id_par[i])
        if (nrow(v)) {
          horiz <- v$valor
          w <- which(names(v) == "extremo")
          if (length(w)) {
            names(horiz) <- v$extremo
          }
        }
      }

      g <- g_long(.data, id_par[i], anio,
                     colores_meses = paleta,
                     horiz = horiz)

      if (!is.null(g))
        ggsave(archivos[i], g, device = "png", path = directorio, scale = .6)

      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste("Nro.", i, "de", n))
    }
  })

  return(file.path(directorio, archivos))
}
