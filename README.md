RESOLUCION
================
Grupo\_05
27/7/2021

``` r
library(tidyverse)
library(hydroGOF)
library(pacman)
```

## PARTE 2

**Calcular la precipitación acumulada anual (Valores observados) para la
cuenca asignada**

``` r
parametros <- as_tibble(read.csv("mods_clima_uh.csv")) 
```

**b) Calcular el porcentaje de sesgo (%, PBIAS) de los escenarios
climáticos (ACCESS, HADGEM2, MPI) respecto a los datos observados para
cada mes (enero - diciembre) de cada variable, para la cuenca asignada**

*Primero filtramos los datos y seleccionamos el parametro con el que
trabajaremos, el cual es la precipitación mensual (bh\_pc)*

``` r
ppobs <- dplyr::filter(parametros, bh_esc == "Observado") %>% 
         select(bh_pc)
mod_Aces <- dplyr::filter(parametros, bh_esc == "ACCESS 1.0") %>% 
         select(bh_pc)
mod_Had <- dplyr::filter(parametros, bh_esc == "HadGEM2-ES") %>% 
         select(bh_pc)
mod_MPI<- dplyr::filter(parametros, bh_esc == "MPI-ESM-LR") %>% 
         select(bh_pc)
```

*ahora aplicamos la función “pbias” el cual esta el la libreria
“hydroGOF” para tener los valores de sesgo y por ultimo unimos para
que no se repitan los valores*

``` r
sesgo <- parametros %>% 
  transmute (bias_Aces = pbias(ppobs, mod_Aces),
            bias_Had = pbias(ppobs, mod_Had),
            bias_MPI = pbias(ppobs, mod_MPI)) %>% 
  unique()
```
