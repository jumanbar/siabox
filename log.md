# 2021-08-12

Observaciones en `datos_sia`, `datos_sia_sed`: se resolvió un bug que había dejado a muchas observaciones como `NA`, cuando en verdad debían tener texto. Para esto se creó una función interna (`siabox:::pegar_obs`) y, para prevenir problemas, se modificaron los códigos generadores de las tablas mencionadas, así como el código de `consulta_muestras`.

Además: la función `ancho` ahora no solamente concatena las observaciones pertenecientes a una misma muestra, sino que además agrega un texto para indicar a qué parámetro corresponde cada parte. Por ejemplo, en la muestra con ID 3291053, se pueden ver dos observaciones, para los parámetros STV y PT:

| id_muestra | nombre_clave | observaciones |
|-----------:|:-------------|:--------------|
|3291053     | STV          | No me los subia repetidos en el template y los subi a mano |
|3291053     | PT           | no eran 10 eran 100 y fue cambiado |

Luego de aplicar ancho, se obtendrá una tabla con una columna de obsrevaciones tal como se vé aquí (se quitan columnas para facilitar la lectura):

| id_muestra|STV (mg/L) |PT (µg P/L) |observaciones                                                                                           |
|----------:|:----------|:-----------|:-------------------------------------------------------------------------------------------------------|
|    3291053|130.000    |100.000     |STV: No me los subia repetidos en el template y los subi a mano. PT: no eran 10 eran 100 y fue cambiado |

# 2021-08-05

Las tablas importadas desde el SIA (`sia_cuenca`, `sia_datos_muestra_parametros`, `sia_departamento`, `sia_estacion`, `sia_institucion`, `sia_matriz`, `sia_muestra`, `sia_param_unidad`, `sia_parametro`, `sia_programa`, `sia_programa_parametro`, `sia_sub_cuenca`, `sia_tipo_punto_estacion` y `sia_unidad`), así como otras construidas a partir de las anteriores (`datos_sia`, `datos_sia_sed`, `usuarios` y `t_eti_base`), fueron actualizadas.

También se incluye a la lista de funciones "exportadas" (o externas?) del paquete a `colapsar_secuencia`, que arregla vectores con listas de cosas en forma de listado expresado en lenguaje natural:

```r
library(siabox)
cat("Los primeros 5 programas de monitoreo en el SIA son:\n\t",
    colapsar_secuencia(sia_programa$nombre_programa[1:5], comillas = TRUE))
```

    Los primeros 5 programas de monitoreo en el SIA son:
    	 'Playa', 'Fray Bentos', 'Santa Lucía', 'Río Negro' y 'Rio Cuareim'

Un cambio menor además, es la eliminación de `RPostgreSQL` como paquete necesario, y el agregado de `RPostgres` como paquete sugerido (no es estrictamente necesario).

# 2021-07-03

La función `g_tie_lin` fue creada para graficar series de tiempo de 1 a 5 parámetros.

# 2021-07-03

- Funciones `[...]_id`: ahora, si el `patron` (primer argumento) es un valor numérico, devuelve la entrada de la tabla correspondiente con el `id == patron`, si es que hay alguna. Ejemplos:

```r
> par_id(2098)
# A tibble: 1 x 7
  id_parametro parametro     enumerado nombre_clave decimales par_vigente codigo_airviro
         <int> <chr>         <lgl>     <chr>            <int> <lgl>       <chr>         
1         2098 Fósforo total FALSE     PT                   9 TRUE        NA        

> pro_id(10)
# A tibble: 1 x 6
  id_programa nombre_programa codigo_programa visible_externos version id_programa_silad
        <int> <chr>           <chr>           <lgl>              <int>             <int>
1          10 Laguna Merin    LM              TRUE                  23                 0

```

- g_mes_pto_all: ahora no da error si se pide una sóla gráfica para un parámetro sin datos

- g_est_dsv: Ahora cuando no hay datos da una gráfica vacía indicando eso

- g_est_dsv_all: ídem

- Cambios en `ancho`: detalles que evitan algunos problemas infrecuentes.

- Mejoras de varios detalles de documentación de funciones.

# 2021-07-02

## `ancho`

La función `ancho` no estaba funcionando bien en todos los casos, ya que asumía que para cada muestra (es decir, `id_muestra`), había un único usuario... lo cual no es el caso, ya que a veces se agregan nuevos parámetros (o se corrigen) a través de diferentes usuarios. Esto debía ser obvio pero en el momento no me había dado cuenta.

## `amoniaco_libre_add`

El arreglo de la función `ancho` impacta sobre esta función directamente, pero además se cambiaron varias cosas:

1. Se suprime el mensaje de advertencia que se generaba automáticamente debido a la función `ancho`. Esperemos que esto no esconda problemas serios.

2. Hasta ahora la función sólo verificaba que los datos ingresados tuvieran al menos un valor de cada uno de los tres parámetros. Ahora verifica, **luego de usar `ancho`**, que hayan valores válidos para los tres parámetros involucrados, para al menos una de las muestras presentes en los datos. En principio se podría verificar antes de aplicar `ancho`, lo cual ahorraría tiempo de procesamiento (ancho es relativamente pesada), pero no estoy seguro, porque para eso tendría que usar una(s) columna(s) de referencia, como `id_muestra`, para determinar si al por lo menos una muestra con datos válidos para pH, Tem y NH4. Por las pruebas que hice hoy sospecho de que usando `id_muestra` de referencia sería suficiente, pero queda la confirmación (y cambios a la función), para otro día.

3. Si los datos de entrada de la función ya tenía valores de NH3L, estos son sustituidos, con una advertencia informativa mediante (que incluye cuántos datos cambiaron y cuál es la diferencia promedio, así como el alejamiento porcentual promedio respecto al dato original).

PENDIENTES: ver punto (2)

## `datos_sia_sed`

Ahora esta también tiene la columna `codigo_pto_mod`, aunque son todos NA, de momento.


# 2021-06-28

`tsummary` actualizada: ahora permite usar columnas con nombres 'reservados' ('n', 'Min', '1er Cu', 'Media', 'Mediana', '3er Cu', 'Max')

Se agrega, además, este archivo de logs.
