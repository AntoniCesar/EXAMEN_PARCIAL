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
variable, ́unicamente con el dato de la elevacióon**

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

## PARTE 2

**Primero que nada leeremos la data**

``` r
parametros <- as_tibble(read.csv("mods_clima_uh.csv"))
```

**a) Calcular la precipitación acumulada anual (Valores observados) para
la cuenca asignada**

``` r
cuenca_tumbes_obs <- parametros %>% 
   dplyr::filter( uh_name == "Cuenca Tumbes" & bh_esc == "Observado" )%>% 
  group_by(uh_name) %>% summarize( pp_acumulada = sum(bh_pc))
```

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

*ahora aplicamos la función “pbias” el cual esta el la libreria
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
