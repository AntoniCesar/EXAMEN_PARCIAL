RESOLUCION
================
Grupo\_05
27/7/2021

``` r
library(tidyverse)
library(hydroGOF)
library(pacman)
```

## PARTE 1

**1) Se tiene una variable x (no necesariamente temperatura) que depende
de la elevación. Se sabe que entre los 1000 y 3000 metros, esta variable
se ve reducido en 2 unidades cada 500 metros. Entre los 3000 y
4000metros, varía en 0.5 unidades, y a una altitud mayor, su valor es
constante. Cree una función que permitaobtener el valor de esta
variable, ́unicamente con el dato de la elevación**

**El valor de la variable x a 1000 metros es de 81.4 unidades**

``` r
#Donde x es la altura
x <- 5000
y <- ((-0.004*x) + 85.4)
z <- ((-0.001*x) + 72.9)
m <- 72.4
if (1000 <= x & x <= 3000) {
 cat("El valor de la variable x es", y)
} else if (3000 < x & x <= 4000) {
  cat("El valor de la variable x es", z)
} else {
   cat("El valor de la variable x es", m)
}
```

    ## El valor de la variable x es 72.4

**2)Resolver el siguiente sistema de ecuaciones.**

``` r
(matriz<- matrix(c(3,2,-2,2,-1,3,1,4,2), nrow = 3 , byrow = T))
```

    ##      [,1] [,2] [,3]
    ## [1,]    3    2   -2
    ## [2,]    2   -1    3
    ## [3,]    1    4    2

``` r
 sol_matriz <- c(0,9,-4)
(solve(matriz,sol_matriz))
```

    ## [1]  2 -2  1

``` r
respuestas <- solve(matriz,sol_matriz)
names(respuestas) <- c("x","y","z")
respuestas
```

    ##  x  y  z 
    ##  2 -2  1

## PARTE 2

**Primero que nada leeremos la data**

``` r
parametros <- as_tibble(read.csv("mods_clima_uh.csv"))
```

**a) Calcular la precipitación acumulada anual (Valores observados) para
la cuenca asignada**

``` r
(cuenca_tumbes_obs <- parametros %>%
    dplyr::filter(uh_name == "Cuenca Tumbes" & bh_esc == "Observado" ) %>%
    group_by(uh_name) %>% 
    summarize( pp_acumulada = sum(bh_pc)))
```

    ## # A tibble: 1 x 2
    ##   uh_name       pp_acumulada
    ##   <chr>                <dbl>
    ## 1 Cuenca Tumbes         852.

**b) Calcular el porcentaje de sesgo (%, PBIAS) de los escenarios
climáticos (ACCESS, HADGEM2, MPI) respecto a los datos observados para
cada mes (enero - diciembre) de cada variable, para la cuenca asignada**

*Primero filtramos los datos y seleccionamos el parametro con el que
trabajaremos, el cual es la precipitación mensual (bh\_pc)*

``` r
ppobs <- dplyr::filter(parametros, bh_esc == "Observado" & 
                         uh_name == "Cuenca Tumbes") 
mod_Aces <- dplyr::filter(parametros, bh_esc == "ACCESS 1.0" &
                          uh_name == "Cuenca Tumbes") 
mod_Had <- dplyr::filter(parametros, bh_esc == "HadGEM2-ES" & 
                           uh_name == "Cuenca Tumbes") 
         
mod_MPI<- dplyr::filter(parametros, bh_esc == "MPI-ESM-LR" & 
                          uh_name == "Cuenca Tumbes") 
```

*Ahora aplicamos la función “pbias” el cual esta el la libreria
“hydroGOF” para tener los valores de sesgo y por ultimo unimos para
que no se repitan los valores*

*Aplicamos sesgo para la precipitacion*

``` r
(sesgo_pp <- parametros %>% 
  transmute(bias_Aces = pbias(mod_Aces$bh_pc, ppobs$bh_pc),
            bias_Had = pbias(mod_Had$bh_pc, ppobs$bh_pc),
            bias_MPI = pbias(mod_MPI$bh_pc, ppobs$bh_pc)) %>% unique())
```

    ## # A tibble: 1 x 3
    ##   bias_Aces bias_Had bias_MPI
    ##       <dbl>    <dbl>    <dbl>
    ## 1      25.9     13.8     -1.5

*Aplicamos sesgo para la evapotranspiracion real*

``` r
(sesgo_evap <- parametros %>% 
  transmute(bias_Aces = pbias(mod_Aces$bh_er, ppobs$bh_er),
            bias_Had = pbias(mod_Had$bh_er, ppobs$bh_er),
            bias_MPI = pbias(mod_MPI$bh_er, ppobs$bh_er)) %>% unique())
```

    ## # A tibble: 1 x 3
    ##   bias_Aces bias_Had bias_MPI
    ##       <dbl>    <dbl>    <dbl>
    ## 1       8.2      7.3        1

*Aplicamos sesgo para el rendimiento hídrico*

``` r
(sesgo_rhidrico <- parametros %>% 
  transmute(bias_Aces = pbias(mod_Aces$bh_rh, ppobs$bh_rh),
            bias_Had = pbias(mod_Had$bh_rh, ppobs$bh_rh),
            bias_MPI = pbias(mod_MPI$bh_rh, ppobs$bh_rh)) %>% unique())
```

    ## # A tibble: 1 x 3
    ##   bias_Aces bias_Had bias_MPI
    ##       <dbl>    <dbl>    <dbl>
    ## 1      35.9     17.4     -2.9

*Aplicamos sesgo para el caudal*

``` r
(sesgo_caudal <- parametros %>% 
  transmute(bias_Aces = pbias(mod_Aces$bh_qd, ppobs$bh_qd),
            bias_Had = pbias(mod_Had$bh_qd, ppobs$bh_qd),
            bias_MPI = pbias(mod_MPI$bh_qd, ppobs$bh_qd)) %>% unique())
```

    ## # A tibble: 1 x 3
    ##   bias_Aces bias_Had bias_MPI
    ##       <dbl>    <dbl>    <dbl>
    ## 1      40.9     21.3     -3.1

**c) De la pregunta anterior, ¿Cual es el escenario climático más
preciso? Fundamente su respuesta.**

## PARTE 3

*Previamente se realizó la conversión de los valores de -99.9 a N.A*

``` r
head(datos_temperatura <- read_csv("temperatureDataset.csv") %>%
    dplyr::select(DATE,qc00000804) %>%
    mutate(DATE = as.Date (DATE,format = "%d/%m/%Y")) %>%
    rename(Temperaturas =qc00000804) %>%
    arrange(DATE) %>%
    mutate(Temperaturas = ifelse(Temperaturas == -99.9, NA, Temperaturas))
    ) 
```

    ## # A tibble: 6 x 2
    ##   DATE       Temperaturas
    ##   <date>            <dbl>
    ## 1 1928-11-02           NA
    ## 2 1928-11-03           NA
    ## 3 1928-11-04           NA
    ## 4 1928-11-05           NA
    ## 5 1928-11-06           NA
    ## 6 1928-11-07           NA

*Luego se verificó que el rango de datos esté completo*

``` r
seq(as.Date("1928-11-02"), as.Date("2015-10-31"), by = "day") %>%
  length()
```

    ## [1] 31775

``` r
tail(datos_temperatura)
```

    ## # A tibble: 6 x 2
    ##   DATE       Temperaturas
    ##   <date>            <dbl>
    ## 1 2015-10-26           NA
    ## 2 2015-10-27           NA
    ## 3 2015-10-28           NA
    ## 4 2015-10-29           NA
    ## 5 2015-10-30           NA
    ## 6 2015-10-31           NA

**a). Determine la cantidad de missing values para los años hidrológicos
Sep1983-Agos1984 y Sep1997-Agos1998.**

``` r
(missing_values1 <- 
   sum(is.na(dplyr::filter(datos_temperatura, DATE >= "1983-09-01" &
                             DATE <= "1984-08-31" )$Temperaturas)))
```

    ## [1] 0

``` r
(missing_value2 <- 
   sum(is.na(dplyr::filter(datos_temperatura, DATE >= "1997-09-01" &
                             DATE <= "1998-08-31" )$Temperaturas)))
```

    ## [1] 3

**b). Calcule la serie de tiempo de temperatura mensual (si el de días
con missing values, en un mes, supera el 5%, la temperatura mensual será
considerado como un NA). Además, identifique visualmente, posibles
valores atípicos y describa una posible causa**

``` r
(Temperatura_mensual <- 
  datos_temperatura %>%
  group_by(DATE = str_sub(DATE,1 , 7)) %>%
  mutate(
    missval = sum(is.na(Temperaturas))*100/n()
  ) %>%
  summarise(
    Temperaturas = mean(Temperaturas, na.rm = T),
    missval = unique(missval)
  )  %>%
  mutate(
    Temperaturas = ifelse(missval >= 5, NA, Temperaturas),
    DATE = as.Date(sprintf("%1$s-01",DATE)),
    month=str_sub(DATE,6,7)
  ))
```

    ## # A tibble: 1,044 x 4
    ##    DATE       Temperaturas missval month
    ##    <date>            <dbl>   <dbl> <chr>
    ##  1 1928-11-01           NA     100 11   
    ##  2 1928-12-01           NA     100 12   
    ##  3 1929-01-01           NA     100 01   
    ##  4 1929-02-01           NA     100 02   
    ##  5 1929-03-01           NA     100 03   
    ##  6 1929-04-01           NA     100 04   
    ##  7 1929-05-01           NA     100 05   
    ##  8 1929-06-01           NA     100 06   
    ##  9 1929-07-01           NA     100 07   
    ## 10 1929-08-01           NA     100 08   
    ## # ... with 1,034 more rows

**c). Determine la cantidad de missing values de la serie de tiempo a
paso mensual para los años 2005 y 2010.**

``` r
(NA_mensual1<- 
   sum(is.na(dplyr::filter(Temperatura_mensual, DATE >= "2005-01-01" & 
                             DATE <= "2005-12-01") $Temperaturas)))
```

    ## [1] 0
