
<!-- README.md is generated from README.Rmd. Please edit that file -->
manoSIAR
========

<!-- badges: start -->
<!-- badges: end -->
Este paquete contiene funciones y datos utilizados para trabajar con las bases datos del SIA. De momento abarca muchas tareas, tales como las que se usan en las aplicaciónes Shiny (vSIA, hSIA e iSIA), o funciones para hacer gráficas que sirven para hacer informes.

Tiene además un grupo no menor de tablas, incluyendo varias de la base de datos INFAMBIENTALBD del SIA.

Tablas de infambiental (base de datos)
--------------------------------------

Es la base de datos en donde se almacenan muchos de los datos e información concerniente al SIA. En particular, tiene datos tomados de las matrices aguas superficiales y sedimentos (al 15/10/2020), así como informaciones relativas a los programas de monitoreo (estaciones, cuencas, etc), parámetros (nombres, unidades de medida según matriz, etc.).

Todas las tablas traídas de infambiental se nombran con el prefijo *sia\_* seguido por el nombre de la tabla encontrado en infambiental. Ejemplo: la tabla `sia_parametro` contiene la misma información que la tabla `parametro` de infambiental.

Como se estila usar en las bases de datos, estas tablas suelen tener una columna dedicada a un indentificador numérico que es único para cada entrada, el cual generalmente se llama `id_...` o simplemente `id`. Por ejemplo, en la tabla `sia_parametro`, la columna `id_parametro` identifica a cada parámetro con un número entero positivo. Siempre que haya algún tipo de ambigüedad respecto a qué parámetro nos referimos, podemos usar el `id_parametro` para asegurarnos de que trabajamos con el que nos interesa. Por ejemplo, el Fósforo Total figura con el `id_parametro` = 2098:

<!-- ```{r sia_parametro} -->
``` r
library(manoSIAR)
# data(sia_parametro)
dplyr::filter(sia_parametro, id_parametro == 2098)
```

<table>
<colgroup>
<col width="14%" />
<col width="15%" />
<col width="11%" />
<col width="14%" />
<col width="11%" />
<col width="13%" />
<col width="17%" />
</colgroup>
<thead>
<tr class="header">
<th align="right">id_parametro</th>
<th align="left">parametro</th>
<th align="left">enumerado</th>
<th align="left">nombre_clave</th>
<th align="right">decimales</th>
<th align="left">par_vigente</th>
<th align="left">codigo_airviro</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">2098</td>
<td align="left">Fósforo total</td>
<td align="left">FALSE</td>
<td align="left">PT</td>
<td align="right">9</td>
<td align="left">TRUE</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

Aún si cambiamos los textos de columnas como `nombre_clave` o `parametro`, el `id_parametro` siempre será 2098.

**NOTA**: En general exportar datos desde infambiental (i.e.: muestras con valores de parámetros), la regla que uso es usar nombres de `id` descriptivos. Entonces, el `id` importado de la tabla `estacion` se convierte en `id_estacion` a la hora de extraer los datos de las muestras. De todas formas, en pro de la consistencia entre las tablas `sia_...` y la base de datos, los nombres de las columnas se mantienen idénticos en las tablas descriptas a continuación:

<!--
## Installation

You can install the released version of manoSIAR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("manoSIAR")
```
-->
El paquete se puede descargar desde [GitHub](https://github.com/) mediante los comandos:

``` r
# install.packages("devtools")
devtools::install_github("jumanbar/manoSIAR")
```

Ejemplos
--------

### Índice de estado trófico (IET)

La función `iet` calcula el IET para valores de Fósforo Total (en microgramos por litro):

``` r
library(manoSIAR)
iet(c(25, 50, 75, 250))
#> [1] 50.65856 54.25856 56.36443 62.61750
plot(iet(1:300), ylab = "IET", xlab = "PT (ug/L)", pch = 20)
```

<img src="man/figures/README-iet-1.png" width="100%" />

------------------------------------------------------------------------

Para desearrolladores
---------------------

> (Sección que, al menos en parte, escribo para refrescar mi propia memoria.)

Este paquete se creó siguiendo de forma aproximada los consejos del libro [R Packages](https://r-pkgs.org/index.html) de Hadley Wickham. El libro entero es importante para entender el desarrollo de paquetes (o la documentación original de CRAN), pero para referencia rápida de quien ya sabe la teoría y sólo tiene que recordar el flujo, ir directamente al capítulo 5: [Fundamental development workflows](https://r-pkgs.org/workflows101.html).

Algunas notas:

-   En funciones no usar `require` o `library`, sino el operador `::`. Ejemplo: `dplyr::filter`. Eso evita afectar el ambiente de trabajo del usuario. La excepción es `ggplot2`, porque se vuelve muy engorroso si no.

-   La función `devtools::load_all()` (Ctrl+Shift+L) sirve para "cargar" el paquete en la sesión, incluyendo funciones, viñetas, y otros códigos. Conviene correrlo cada vez que cambiamos alguna función y queremos probarla, por ejemplo.

-   La función `devtools::document()` (Ctrl+Shift+D) sirve para actualizar los archivos de documentación.
