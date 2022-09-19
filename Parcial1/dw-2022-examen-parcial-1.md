dw-2022-parcial-1
================
Javier Alejandro Mazariegos Godoy
9/19/2022

# Examen parcial

Indicaciones generales:

-   Usted tiene el período de la clase para resolver el examen parcial.

-   La entrega del parcial, al igual que las tareas, es por medio de su
    cuenta de github, pegando el link en el portal de MiU.

-   Pueden hacer uso del material del curso e internet (stackoverflow,
    etc.). Sin embargo, si encontramos algún indicio de copia, se
    anulará el exámen para los estudiantes involucrados. Por lo tanto,
    aconsejamos no compartir las agregaciones que generen.

## Sección I: Preguntas teóricas.

-   Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
    deberá responder 5. Las 5 a responder estarán determinadas por un
    muestreo aleatorio basado en su número de carné.

-   Ingrese su número de carné en `set.seed()` y corra el chunk de R
    para determinar cuáles preguntas debe responder.

``` r
set.seed("20200223") 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 4, 5, 6, 7, 9"

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:

    -   `str()`
    -   `df[,c("a","b")]`
    -   `names(df)[4] <- "new_name"` donde la posición 4 corresponde a
        la variable `old_name`
    -   `df[df$variable == "valor",]`

2.  Al momento de filtrar en SQL, ¿cuál keyword cumple las mismas
    funciones que el keyword `OR` para filtrar uno o más elementos una
    misma columna?

3.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

4.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R?

5.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`?

6.  ¿Qué es un vector y en qué se diferencia en una lista en R?

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes?

8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?

    -   El nuevo elemento
    -   `NA`

9.  En SQL, ¿para qué utilizamos el keyword `HAVING`?

10. Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    -   SELECT \* FROM A \_\_\_\_\_\_\_ B ON A.KEY = B.KEY WHERE
        \_\_\_\_\_\_\_\_\_\_ = \_\_\_\_\_\_\_\_\_\_

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

## Sección II Preguntas prácticas.

-   Conteste las siguientes preguntas utilizando sus conocimientos de R.
    Adjunte el código que utilizó para llegar a sus conclusiones en un
    chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas teóricas

4.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R? \# La
    diferencia es que `==` se utiliza para comparación, es un oprador
    booleano. `=` se utiliza para asignacion

5.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`? \#La forma correcta de cargar un archivo de
    texto con el delimitador `:`, es utilizando la funcion
    “read_delim()” de la libreria reader \#Ejemplo: read_delim(“Nombre
    del archivo”, delim=“:”)

6.  ¿Qué es un vector y en qué se diferencia en una lista en R? \#Los
    vectores son objetos unidimensionales, estas estructuras de datos
    pueden alamcenar datos númericos, cadenas de caracteres, booleanos,
    entre otros. La diferencia es que los vectores son homogéneos,
    mientras que las listas pueden ser heterogeneas, es decir, pueden
    alamcenar diferentes tipos de datos.

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes? No se puede agregar
    directamente una nueva categoria, hay que convertir el factor a una
    lista, agregar el nuevo valor y luego convertir a factor.

9 En SQL, ¿para qué utilizamos el keyword `HAVING`? \#El having en SQL
se utiliza cuando se quiere filtrar por agregación, es decir, después de
haber agrupado los datos con la funcion GROUP_BY()

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

``` r
library(gtools)
nrow(combinations(10, 5))
```

    ## [1] 252

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

## A

``` r
###resuelva acá
parcial_anonimo <- readRDS("parcial_anonimo.rds")
#repetidos <- parcial_anonimo %>% distinct(Pais)
pais1 <- parcial_anonimo %>% filter(Pais =="4046ee34")
pais2 <- parcial_anonimo %>% filter(Pais =="4f03bd9b")

mas_de_uno <- pais1 %>%
              filter(Cliente %in% pais2$Cliente)

rentable <- parcial_anonimo %>%
              filter(Cliente %in% mas_de_uno$Cliente) %>%
              group_by(Cliente) %>%
              summarise(rentable = sum(Venta)) %>%
              slice(which.max(rentable))
rentable
```

    ## # A tibble: 1 x 2
    ##   Cliente  rentable
    ##   <chr>       <dbl>
    ## 1 a17a7558   19818.

El cliente más retable es a17a7558 porque es el que más ventas ha
tenido.

## B

``` r
###resuelva acá

parcial_anonimo %>% filter(Venta < 0) %>%
                     group_by(Territorio) %>%
                     summarise (Perdidas = sum(Venta)) %>%
                     filter(Perdidas < -3000)
```

    ## # A tibble: 7 x 2
    ##   Territorio Perdidas
    ##   <chr>         <dbl>
    ## 1 1d407777     -3300.
    ## 2 2e812869     -3056.
    ## 3 69c1b705     -3370.
    ## 4 72520ba2     -3761.
    ## 5 77192d63     -5641.
    ## 6 bc8e06ed     -3269.
    ## 7 f7dfc635    -14985.

Suposiciones: 1. Los Territorios que tengan más de Q3,000 en perdidas se
considera como un riesgo para la empresa 2. Las perdidas son anuales

Se debrían de cerrar 7 Territorios, los cuales son: 1d407777, 2e812869,
69c1b705, 72520ba2, 77192d63, bc8e06ed, f7dfc635. Esto se debe a que
están teniendo perdias de más de Q3,000, ya no se gana nada y están
representando un peligro en las finanzas de la empresa.
