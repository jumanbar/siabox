# source(file.path(ruta_base, "R/funciones.R"), 
#        encoding = 'UTF-8', local = TRUE)
# source(file.path(ruta_base, "R/rest_fun.R"), 
#        encoding = 'UTF-8', local = TRUE)
# source(file.path(ruta_base, "R/ws_funciones_temp.R"), 
#        encoding = 'UTF-8', local = TRUE)
# source(file.path(ruta_base, "R/traer_tablas_sia.R"), 
#        encoding = 'UTF-8', local = TRUE)
library(tidyverse)
library(siabox)
source("data-raw/traer_tablas_sia.R", encoding = 'UTF-8', local = TRUE)
lim <- siabox::lim

# Función usada para crear rango_param y rango_param_sitio:
#
# Para cuántos sitios aplica una condición encontrada en lim$Condiciones.r?
#
# La idea es que la condición más frecuente sea la usada para determinar valores
# por defecto para cada parámetro (los valores por defecto van a rango_param y
# el resto a rango_param_sitio).
contar_casos_cond <- function(id_parametro) {
  out <-
    lim %>% 
    dplyr::filter(id_parametro == {{ id_parametro }}) %>% 
    dplyr::count(Linf, Lsup, Condiciones.r)
  
  out$n_estaciones <- integer(nrow(out))
  
  for (i in 1:nrow(out)) {
    condi <- out$Condiciones.r[i]
    logi  <- eval(parse(text = condi), envir = e)
    out$n_estaciones[i] <- sum(logi, na.rm = TRUE)
  }
  return(dplyr::arrange(out, dplyr::desc(n_estaciones)))
}

# Pensando en cómo sería una tabla con los rangos de parámetros, que estaría en
# una base de datos (no necesariamente infambiental, claramente, pero no hay
# porqué descartarlo). Inicialmente las llamé límites, pero capaz que no es el
# nombre más expresivo. Ahora me inclino más por el nombre "rango_param".

## Ejemplo piloto ----
#
# Ejemplo de juguete usado para crear alertas por valores afuera del rango
# esperado. Acá se muestra solamente la estructura de las tablas. En el archivo
# pruebas_rangos.R se muestran códigos de ejemplos de evaluaciones (los nombres
# de las tablas son diferentes: rango_param y rango_param_sitio, etc...)
#
# Usa dos tablas:
#
### 1. rango_param: ----
#
# Contiene el rango básico para cada parámetro: son los valores de mínimo y
# máximo para cáda parámetro, usados por defecto. Deberían afectar a la gran
# mayoría de los casos, aunque para que eso se cumpla, depende de la creación de
# las entradas de la tabla, lo cual es un proceso manual y más bien "artesanal"
# (hacerlo de otra forma implicaría más esfuerzo del que se justifica, al menos
# en esta instancia).
# 
# Columnas:
# 
# id_parametro: el id de los paráemtros tal como está en la tabla parametro del
# infambiental.
# 
# id_matriz: el id de la matriz (hoy en día sólo hay rangos definidos para aguas
# superficiales).
# 
# valor_min: el valor mínimo esperado para cada parámetro & matriz
# 
# valor_max: el valor máximo esperado para cada parámetro & matriz

rango_param <-
  tibble(
    id_parametro = 2098,
    id_matriz = 6,
    valor_min = 10,
    valor_max = 1000
  )

# Nota: si el parámetro NO está en esta tabla, debe interpretarse simplemente
# como que no hay valores "demasiado altos" ni "demasiado bajos" para generar
# alertas, para dicho parámetro. Estos casos en general son indicativos de que
# aún no hay suficiente información acumulada como para establecer rangos
# normales. (En pricipio se podría establecer rangos basados en la literatura,
# pero al momento no se ha hecho esta tarea.)


### 2. rango_param_sitio: ----
#
# Tiene el rango para algunos parámetros para los cuales hay rangos específicos
# según la ubicación geográfica del punto de monitoreo. Esta tabla tendrá las
# columnas id_parametro e id_matriz, tal como rango_param, pero las demás
# columnas serán:
#
# id_sitio: el id del sitio, que se corresponde con una única ubicación
# geográfica (definida con coordenadas, nombre, etc...)
#
# valor_min_sitio: el valor mínimo esperado para cada parámetro & matriz & sitio
#
# valor_max_sitio: el valor máximo esperado para cada parámetro & matriz & sitio

rango_param_sitio <-
  tibble(
    id_parametro = 2098,
    id_matriz = 6,
    id_sitio = 100231,
    valor_min_sitio = 10,
    valor_max_sitio = 6000
  )

# 3. sitio ------
# 
# _ Respecto al id_sitio ----
#
# Hoy en día la app de validación trabaja con sitios definidos en la tabla
# estacion, de infambiental, la cual tiene los puntos de monitoreo establecidos
# por los programas de monitoreo.
#
# Sin embargo en un futuro los datos que van a ser evaluados por las app de
# validación vendrán de fuentes muy diferentes, que involucran a otras bases de
# datos y otros muestreos (industrias, evaluaciones de impacto, etc). Entonces,
# habrá que resolver cómo se comunicarán estas fuentes con la validación. Esto
# implica que cada punto de muestreo sea identificado correctamente y con un id
# único. Posiblemente sea necesaria una tabla aparte con un id único para cada
# sitio, así como una referencia al id en su base de datos original. Pero
# también sería bueno tener coordenadas, depto, etc, de forma que se pueda
# chequear varios problemas que pueden surgir con la identificación de sitios.
# En definitiva, habría que tener un sistema de validación para el momento en
# que se ingresan sitios nuevos, que evalue cosas como:
#
# 1. Sitios sea ingresados varias veces con coordenadas ligeramente diferentes
# (y nombres, ids, etc, diferentes también). Esto se puede chequear calculando
# la distancia entre sitios: dos sitios son dudosos de ser un único sitio si
# tienen coordenadas muy cercanas.
#
# 2. Coordenadas ingresadas mal, tal vez por diferencias en los sistemas de
# coordenadas (ej: UTM vs sin proyección, etc). Varias veces el error es
# relativamente fácil de detectar (el punto cae en medio del océano, por
# ejemplo, o fuera del departamento esperado.). Estos casos son detectables
# automáticamente si es que hay un error lo suficientemente grande como para que
# no apruebe evaluaciones como: no se encuentra dentro del polígono
# correspondiente al departamento/país/continente al cual debería pertenecer.

sitio <- tibble(
  id_sitio = 1:nrow(sia_estacion),
  # Cada sitio / contrato, deberá tener una matriz asociada sí o sí, de forma
  # sólo con la información del sitio ya podemos saber a qué matriz corresponde.
  # Esto es una configuración invenad por mí, no sé si es compatible con el SIA
  # etc...
  id_matriz = sia_estacion$matriz_estacion,
  id_sub_cue = sia_estacion$sub_cuenca,
  id_depto = sia_estacion$departamento,
  latitud = sia_estacion$latitud,
  longitud = sia_estacion$longitud,
  # id_matriz = sia_estacion$matriz_estacion,
  id_fuente = rep.int(1, nrow(sia_estacion)),
  # tabla_fuente: No me queda claro si el nombre de la tabla es lo mejor, o si
  # existe una convención confiable y consistente a través de la cual se pueda
  # señalar a la tabla necesaria con un ID, capaz que sería una buena opción.
  # Esto que hice es un intento claramente amateur:
  tabla_fuente = "estacion",
  id_interno = sia_estacion$id
  # Otros campos?
)

fuente <- tibble(
  id_interno = 1, # id_fuente?
  fuente = "infambientalbd"
  # Otros campos?
)

# save(sitio, file = "data/sitio.rda")
# save(fuente, file = "data/fuente.rda")



# Por último agregar: las coordenadas permiten asociar los puntos con regiones:
# cuencas, subcuencas, etc, por ejemplo, que pueden servir para tomar de
# referencia a la hora de hacer evaluaciones ambientales e incluso para
# actualizar los rangos de los parámetros (ie: tomar estadísticas de parámetros
# por zonas.).

# Rangos básicos y específicos -----

lim$id_parametro <- as.integer(lim$id_parametro)
lim <- dplyr::filter(lim, !is.na(id_parametro))
lim$basico <- !logical(nrow(lim))

# Parámetros con rangos específicos según sitios:
p <- sort(unique(dplyr::filter(lim, !is.na(Condiciones.r))$id_parametro))
lim$basico[lim$id_parametro %in% p] <- FALSE

# + Conductividad ----
dplyr::filter(lim, id_parametro == 2009)$Condiciones.r 

conduc_lim_cond <-
  dplyr::filter(lim, id_parametro == 2009) %>% 
  dplyr::count(Linf, Lsup, Condiciones.r)

filter(lim, id_parametro  == 2009 & Linf == "1" & Lsup == "500") %>% 
  select(id_parametro, id_matriz, Linf, Lsup, Condiciones.r)

w <- with(lim, which(id_parametro  == 2009 & Linf == "1" & Lsup == "500"))
tmp <- lim[w,]

lim <- lim[-w[-1],]

e <- sia_estacion %>% rename(id_programa = prog_monitoreo, id_estacion = id)

# NOTA: hay 22 estaciones sin programa:
filter(e, is.na(id_programa))

condi <- "(id_programa %in% c(4, 5, 15)) | (id_programa == 1 & id_estacion == 100159L)"

logi <- parse(text = condi) %>% eval(envir = e)
sia_estacion[logi,]

lim$Condiciones.r[w[1]] <- condi

conduc_lim_cond <- contar_casos_cond(2009)

w <- with(lim, which(id_parametro == 2009 & 
                       Linf == conduc_lim_cond$Linf[1] & 
                       Lsup == conduc_lim_cond$Lsup[1]))

lim$basico[w] <- TRUE


# + Dureza total ----
id_par <- 2010
tmp <- contar_casos_cond(id_par)

w <- with(lim, which(id_parametro == id_par & 
                       Linf == tmp$Linf[1] & 
                       Lsup == tmp$Lsup[1]))

lim$basico[w] <- TRUE

# + Salinidad ----
w <- with(lim, which(id_parametro  == 2020 & Linf == "0" & Lsup == "3"))
tmp <- lim[w,] %>% select(id_parametro, id_matriz, Condiciones.r, Linf, Lsup)
condi <- paste0("(", paste(tmp$Condiciones.r, collapse = ") | ("), ")")
lim <- lim[-w[-1],]
lim$Condiciones.r[w[1]] <- condi

tmp <- contar_casos_cond(2020)

w <- with(lim, which(id_parametro == 2020 & 
                       Linf == tmp$Linf[1] & 
                       Lsup == tmp$Lsup[1]))
lim$basico[w] <- TRUE

# + Sólidos Totales (ST) ----
id_par <- 2028
tmp <- contar_casos_cond(id_par)
w <- with(lim, which(id_parametro == id_par & 
                       Linf == tmp$Linf[1] & 
                       Lsup == tmp$Lsup[1]))
lim$basico[w] <- TRUE


# + Aluminio ----
filter(lim, id_parametro == 2036)

lim$basico[lim$id_parametro == 2036 & 
             lim$Linf == "0.01" & 
             lim$Lsup == "50"] <- TRUE

# + NAmoniacal ----
id_par <- 2090
tmp <- contar_casos_cond(id_par)
w <- with(lim, which(id_parametro == id_par & 
                       Linf == tmp$Linf[1] & 
                       Lsup == tmp$Lsup[1]))
lim$basico[w] <- TRUE

# + PT ----
id_par <- 2098
tmp <- contar_casos_cond(id_par)
w <- with(lim, which(id_parametro == id_par & 
                       Linf == tmp$Linf[1] & 
                       Lsup == tmp$Lsup[1]))
lim$basico[w] <- TRUE

# + NO3 ----
id_par <- 2099
tmp <- contar_casos_cond(id_par)
w <- with(lim, which(id_parametro == id_par & 
                       Linf == tmp$Linf[1] & 
                       Lsup == tmp$Lsup[1]))
lim$basico[w] <- TRUE

rango_param <-
  lim %>% 
  filter(basico) %>% 
  transmute(id_parametro, 
            id_matriz, 
            valor_min = as.numeric(Linf), 
            valor_max = as.numeric(Lsup))

# Ambas deben dar TRUE:
all(lim$id_parametro %in% rango_param$id_parametro)
all(rango_param$id_parametro %in% lim$id_parametro)

rango_param_sitio <- tibble(
  id_sitio  = integer(0),
  id_parametro = integer(0),
  # id_matriz = integer(0),
  valor_min_sitio = numeric(0),
  valor_max_sitio = numeric(0)
)

w <- which(!lim$basico)
i <- 1
w[i]
for (i in 1:length(w)) {
  fila <- lim[w[i],]
  condi <- fila$Condiciones.r
  logi <- parse(text = condi) %>% eval(envir = e)
  estaciones <- e$id_estacion[which(logi)]
  sitios <- sitio$id_sitio[sitio$id_interno %in% estaciones]
  
  rango_param_sitio <-
    rango_param_sitio %>% 
    add_case(
      id_sitio = sitios,
      id_parametro = fila$id_parametro,
      # id_matriz = fila$id_matriz,
      valor_min_sitio = as.numeric(fila$Linf),
      valor_max_sitio = as.numeric(fila$Lsup)
    )
}

# @ rango_param_sitio ----
# rango_param_sitio <- 
left_join(rango_param_sitio, sitio, by = c("id_sitio")) 
# %>%
#   select(id_parametro, id_matriz, id_sitio = id_sitio.y,
#          valor_min_sitio, valor_max_sitio)

# save(rango_param, file = "data/rango_param.rda")
# save(rango_param_sitio, file = "data/rango_param_sitio.rda")
