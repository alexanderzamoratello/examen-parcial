resolucion examen practico - zamora
================
zamora tello
6/8/2021

# EXAMEN PARCIAL

*ALUMNO:ALEXANDER RENATO ZAMORA TELLO*

*CODIGO :17160048*

## Parte 1 - Usar R nativo (no librer´ıas)

**ejercicio 1**

``` r
variable1 <- function(h, x = 81.4){
  if (h == 1000){
    var1 = 81.4
  } else if (h<=3000 & h> 1000){
    var1 = x - 2*((h-1000)%/%500)
  } else if (h > 3000 & h<=4000){
    var1 = x - 8 - 0.5*((h-3000)%/%500)
  } else if (h > 4000){
    var1 = x - 9
  } else {
    print("no hay respuesta!")
  }
  return(var1)
}

variable1(1000)
```

    ## [1] 81.4

**ejercicio 2**

``` r
A <- rbind(c(2, 1, 3), 
           c(5, -4, 1), 
           c(1, -1, -4))
B <- c(7, -19, 4)
solve(A, B)
```

    ## [1]  2.923077  7.846154 -2.230769

## Parte 2

**2A**

``` r
data1 <- as_tibble(read.csv("mods_clima_uh.csv"))
(data2 <- data1 %>%
  dplyr::filter(uh_name =="Cuenca Ushusuma", bh_esc == "Observado" )%>%
  group_by(uh_name) %>%
  summarise(bh_pc = sum(bh_pc, na.rm = T)))
```

    ## # A tibble: 1 x 2
    ##   uh_name         bh_pc
    ##   <chr>           <dbl>
    ## 1 Cuenca Ushusuma  286.

**2B**

``` r
(data3 <- data1 %>% 
  dplyr::select(uh_name,bh_month,bh_pc,bh_esc)%>% 
  dplyr::filter(uh_name == "Cuenca Ushusuma") %>% 
  pivot_wider(names_from = "bh_esc", values_from = "bh_pc"))
```

    ## # A tibble: 12 x 6
    ##    uh_name         bh_month Observado `ACCESS 1.0` `HadGEM2-ES` `MPI-ESM-LR`
    ##    <chr>              <int>     <dbl>        <dbl>        <dbl>        <dbl>
    ##  1 Cuenca Ushusuma        1    85.1         93.7         76.6         68.1  
    ##  2 Cuenca Ushusuma        2    85.2         68.1         76.6         76.6  
    ##  3 Cuenca Ushusuma        3    53.9         35.9         48.5         55.6  
    ##  4 Cuenca Ushusuma        4    11.0         11.0         10.3         10.7  
    ##  5 Cuenca Ushusuma        5     0.994        0.392        0.392        0.686
    ##  6 Cuenca Ushusuma        6     1.45         0.967        0.967        1.65 
    ##  7 Cuenca Ushusuma        7     1.48         0.983        0.983        2.42 
    ##  8 Cuenca Ushusuma        8     1.69         1.69         1.69         2.25 
    ##  9 Cuenca Ushusuma        9     0.894        0.736        0.736        1.16 
    ## 10 Cuenca Ushusuma       10     3.31         3.1          3.64         5.77 
    ## 11 Cuenca Ushusuma       11     6.28         3.98         6.28         6.93 
    ## 12 Cuenca Ushusuma       12    34.7         22.0         28.9         32.4

``` r
(OB_ACCES <-pbias(data3$`ACCESS 1.0`, data3$Observado, na.rm = T))
```

    ## [1] -15.2

``` r
(OB_HadGE <-pbias(data3$`HadGEM2-ES` , data3$Observado , na.rm = T))
```

    ## [1] -10.6

``` r
(OB_MPIEM <-pbias(data3$`MPI-ESM-LR` , data3$Observado , na.rm = T))
```

    ## [1] -7.6

**2C**

**el escenario mas preciso es el de “MPI-ESM-LR” ya que tiene un valor
de -7.6 de sesgo si es sesgo es mas cercano a 0 es mas preciso**

**2D**

``` r
(data4 <- data1 %>% 
  group_by(bh_month, bh_esc) %>% 
  ggplot(mapping = aes(x = bh_esc, y = bh_pc, fill = bh_esc))+
  geom_boxplot()+
  labs (title = "Precipitación de Enero a Diciembre",
       x = "Escenarios",
       y = "Precipitación"
  ))
```

![](resolucion-examen-zamora_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Parte 3

**Pregunta1**

``` r
df_t1 <- as_tibble(read.csv("temperatureDataset.csv"))
(df_t2 <- df_t1 %>% 
  dplyr::mutate(DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::select(DATE, qc00000805) %>%
  rename(tt = qc00000805) %>% 
  dplyr::mutate(tt = ifelse(tt == -99.9, NA, tt)))
```

    ## # A tibble: 31,775 x 2
    ##    DATE          tt
    ##    <date>     <dbl>
    ##  1 1928-11-02    NA
    ##  2 1928-11-03    NA
    ##  3 1928-11-04    NA
    ##  4 1928-11-05    NA
    ##  5 1928-11-06    NA
    ##  6 1928-11-07    NA
    ##  7 1928-11-08    NA
    ##  8 1928-11-09    NA
    ##  9 1928-11-10    NA
    ## 10 1928-11-11    NA
    ## # ... with 31,765 more rows

**1 A**

``` r
(df_t3 <- df_t2 %>% 
  dplyr::filter(DATE >= "1983-09-01" & DATE < "1984-08-31" | 
                  DATE >= "1997-09-01" & DATE < "1998-08-31"))
```

    ## # A tibble: 729 x 2
    ##    DATE          tt
    ##    <date>     <dbl>
    ##  1 1983-09-01  22  
    ##  2 1983-09-02  22  
    ##  3 1983-09-03  22.6
    ##  4 1983-09-04  22  
    ##  5 1983-09-05  24  
    ##  6 1983-09-06  24.4
    ##  7 1983-09-07  24  
    ##  8 1983-09-08  26  
    ##  9 1983-09-09  24.4
    ## 10 1983-09-10  25  
    ## # ... with 719 more rows

``` r
sum(is.na(df_t3$tt))
```

    ## [1] 1

**1 B**

``` r
(df_t4 <- df_t2 %>% 
  group_by(DATE = str_sub(DATE, 1, 7)) %>% 
  mutate(MissVal = sum(is.na(tt))*100/n()) %>% 
  summarise(
    tt = mean(tt, na.rm = T),
    MissVal = unique(MissVal)
  ) %>% 
  mutate(
    tt = ifelse(MissVal >= 5, NA, tt),
    DATE = as.Date(sprintf("%1$s-01", DATE)),
    month = str_sub(DATE, 6, 7)
  ))
```

    ## # A tibble: 1,044 x 4
    ##    DATE          tt MissVal month
    ##    <date>     <dbl>   <dbl> <chr>
    ##  1 1928-11-01    NA     100 11   
    ##  2 1928-12-01    NA     100 12   
    ##  3 1929-01-01    NA     100 01   
    ##  4 1929-02-01    NA     100 02   
    ##  5 1929-03-01    NA     100 03   
    ##  6 1929-04-01    NA     100 04   
    ##  7 1929-05-01    NA     100 05   
    ##  8 1929-06-01    NA     100 06   
    ##  9 1929-07-01    NA     100 07   
    ## 10 1929-08-01    NA     100 08   
    ## # ... with 1,034 more rows

``` r
ggplot(data = df_t4, mapping = aes(x = month, y = tt, fill = month))+
  geom_boxplot()
```

    ## Warning: Removed 353 rows containing non-finite values (stat_boxplot).

![](resolucion-examen-zamora_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**se puede observar dos valores atipicos en el mes de agosto uno menor a
24 y uno mayor a 28 , en el mes de marzo hay un valor atipico menor a 24
y en el mes de noviembre hay un valor atipico superior a 28 esto se
puede dar debido anomalias climaticas que pasan cada cierto tiempo por
diferentes causas como por ejemplo el fenomeno del niño**

**1 C**

``` r
(df_t5 <- df_t2 %>% 
    group_by(DATE = str_sub(DATE, 1, 7)) %>% 
    mutate(MissVal = sum(is.na(tt))) %>% 
    summarise(
      tt = mean(tt, na.rm = T),
      MissVal = unique(MissVal)
    ) %>% 
    dplyr::filter(DATE >= "2005-01-01" & DATE < "2005-12-31" |
                    DATE >= "2010-01-01" & DATE < "2010-12-31") %>% 
    mutate(
      DATE = as.Date(sprintf("%1$s-01", DATE)),
      month = str_sub(DATE, 6, 7)
    ))
```

    ## # A tibble: 22 x 4
    ##    DATE          tt MissVal month
    ##    <date>     <dbl>   <int> <chr>
    ##  1 2005-02-01  25.0       0 02   
    ##  2 2005-03-01  24.8       0 03   
    ##  3 2005-04-01  24.5       0 04   
    ##  4 2005-05-01  24.5       0 05   
    ##  5 2005-06-01  24.8       0 06   
    ##  6 2005-07-01  23.7       0 07   
    ##  7 2005-08-01  25.1       0 08   
    ##  8 2005-09-01  24.5       0 09   
    ##  9 2005-10-01  24.4       0 10   
    ## 10 2005-11-01  24.8       0 11   
    ## # ... with 12 more rows

**1 D**

``` r
funcion2 <- function(data_temp){
  dplyr::filter(data_temp, DATE >= "1980-01-01" & DATE < "1995-12-31" | 
                  DATE >= "1996-01-01" & DATE < "2010-12-31") %>% 
    group_by(DATE = str_sub(DATE, 1,7)) %>% 
    summarise(ttmean = mean(tt, na.rm = T)) %>% 
    return()
    }
(df_t6 <- funcion2(df_t2))
```

    ## # A tibble: 372 x 2
    ##    DATE    ttmean
    ##    <chr>    <dbl>
    ##  1 1980-01   26.8
    ##  2 1980-02   25.3
    ##  3 1980-03   27.1
    ##  4 1980-04   26.3
    ##  5 1980-05   25.9
    ##  6 1980-06   25.6
    ##  7 1980-07   25.8
    ##  8 1980-08   25.6
    ##  9 1980-09   26.1
    ## 10 1980-10   26.4
    ## # ... with 362 more rows

``` r
df_t6 %>% 
  mutate(month = str_sub(DATE, 6, 7)) %>% 
  ggplot(mapping = aes(x = month, y = ttmean, fill = month))+
  geom_boxplot()+
  ggtitle("Variacion mensual de la temperatura")+
  labs(
    x = "Meses",
    y = "Temperatura"
    )
```

    ## Warning: Removed 3 rows containing non-finite values (stat_boxplot).

![](resolucion-examen-zamora_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

**podemos observar 3 valores atipicos en el mes de enero que estan entre
26 y 28 C°, en cambio en el mes de tenemos dos valores atipicos pero uno
de ellos es menor de 23 C°y hay un solo valor atipico en los meses de
febrero y agosto**

**1 E**

``` r
df_t4 %>% 
  dplyr::filter(DATE >= "1980-01-01" & DATE < "2013-12-31") %>% 
  ggplot(mapping = aes(x = month, y = tt, fill = month))+
  geom_boxplot()+
  labs(
    x = "Meses",
    y = "Temperatura"
  )
```

    ## Warning: Removed 14 rows containing non-finite values (stat_boxplot).

![](resolucion-examen-zamora_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

**se puede notar en el grafico que la mediana de las cajas de los meses
octubre , noviembre y diciembre son casi iguales con un valor superior a
25 C°. se puede notar un valor atipico en el mes de agosto de una
temperatura menor a 23 C°. el mes que tiene mayor cantidad de valores
atipicos y la max temperatura es el de enero con una tmax superior 27
C°**
