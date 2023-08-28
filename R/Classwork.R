#' Analisis de datos de vuelos comerciales primer corte
#'
#Se realiza analisis de base de datos de los ultimos vuelos hechos, segun el paquete nycflights13
#'
#' @param exercise_num Ejercicios de taller en clase
#' @return Solución del o de los datos.
#' @export
#'
#' @examples

# Funcion "select" para mostrar datos solicitados en cada item

# Usamos la "library(tidyverse)" para acceder a la base de datos
library(tidyverse)
# 5.2.4 item 1  Filtramos los vuelos con retraso de dos o mas horas
# Asignamos "vuelos" a la base de datos
vuelos <- nycflights13 ::flights
# Con la funcion "filter" seleccionamos el titulo de "arr_delay" indicando que
# queremos ver los vuelos con 2 o mas horas de retraso
vuelos_con_retraso <- filter (vuelos, arr_delay >= "2",
                              na.rm=TRUE)

# 5.2.4 item 2, Solicitamos mediante la funcion "filter" los vuelos que llegan a HOUSTON,
# a los aeropuertos HOU o IAH

vue_dest_hou <- filter (vuelos, dest == "HOU" | dest == "IAH"
                        ,na.rm=TRUE)

#5.3.1 item 1 Usamos la función "arrange()" para ordenar las filas en orden y que se
#muestre los valores faltantes al inicio usando "is.na"

organizar <- vuelos%>%
  arrange (desc(is.na(dep_time)))

#5.3.1 item 2 nuevamente con la funcion "arrange()" filtrando la columna "dep_delay"
# y nos muestre los vuelos con mayor tiempo de retraso en orden descendete

vue_conmayorretraso <- vuelos%>%
  arrange (desc(dep_delay))

# 5.3.1 item 2 Ahora queremos ver los que salieron antes de tiempo y nos mostrara
# en orden ascendente para esto eliminamos la funcion "desc" antes de "dep_delay"

vue_adel <- vuelos%>%
  arrange (dep_delay)

# 5.3.1 item 3 Continuando con la funcion "arrange" orden de los vuelos mas rapidos (gastaron menos tiempo)
#para esto organizamos desde el titulo "hour, minute"
vue_rapid <- vuelos%>%
  arrange (hour, minute)

# 5.3.1 item 4 Ordenamos mediante "arrange" los vuelos mas lejanos con base al titulo "distance"

vue_lejos <- vuelos%>%
  arrange (desc(distance))

# 5.3.1 item 4 Ordenamos mediante "arrange" los vuelos mas ceranos con base al titulo "distance"
vue_cerca <- vuelos%>%
  arrange (distance)

# 5.4.1 item 2 llamaremos una variable varias veces mediante la funcion "select", esta unicamente
# mostrara los valores del titulo seleccionado, en este caso "origin". Si llamo varias veces
#la misma variable no cambiara en nada, mostrara unicamente esa variable, una sola vez.

call_var <-  vuelos%>%
  select(origin, origin, origin)

# 5.4.1 item 3 Con la funcion "any_of" podemos decirle al motor de expresiones que asocie solo unas variables en especifico.
#Simplemente coloque los caracteres que desea mostra, en este caso con "select" y "any_of" solo queremos ver "year", "month",
#"day", "dep_delay", "arr_delay"

rp <-  vuelos%>%
  select(any_of(c("year", "month", "day", "dep_delay", "arr_delay")))

# 5.4.1 item 4 los datos mostrados mediante la funcion "select" son de variables que incluyen la
#palabra TIME, esto gracias al comando "contains", si cambio por otra palabra en comun me mostrara
#unicamente las variables que tengan esta palabra.

rp1 <-  vuelos%>%
  select(vuelos, contains("TIME"))

# 5.5.2 item 1 mediante "select" mostraremos datos de dep_time y sched_dep_time mas parecido a una hora
#pusamos a dividir por 100 y ,multiplicar por 60 para tener los numeros desde las 00:00 esto con la funcion "mutate"

hora_vuelos <- vuelos %>%
  select(dep_time, sched_dep_time)%>%
  mutate(minutos_deptime = (dep_time/100*60), minutos_sched = (sched_dep_time/100*60))

#5.5.2 item 2 Con la funcion "mutate" creamos una nueva variable diviendo el "air_time" entre 100
# luego con "select" moestraremos todo y realizaremos lo mismo con el titulos "arr_time" crando las
# variables "air_t" y "arr_t"

comparar <- vuelos %>%
  mutate(air_t = air_time /100) %>%
  select(air_t, air_time, everything()) %>%
  mutate(arr_t = arr_time/100) %>%
  select(arr_t, everything())

### 5.6.7 Para esta tabla con "filter" mostraremos "air_time" mostrando los valores na
no_cancelados <- vuelos %>%
  filter(!is.na(air_time))

### Defiinimos la llegada como la mas importante
no_cancelados %>%
  group_by(tailnum) %>% #agrupamos
  mutate(
    count = n(),
    median_arr_delay = median(arr_delay), #creamos nuevas variables "median_arr_delay"
    median_dep_delay = median(dep_delay)  #y "median_dep_delay"
  ) %>%
  filter(count > 30) %>%
  arrange(median_arr_delay, median_dep_delay) #organizamos con "arrange"

#Aquí tenemos los vuelos que realizaron al menos 30 vuelos en un año y llegaron
#o salieron antes de los 15 minutos durante más del 50% del tiempo.

no_cancelados %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    p_15_tempr_arr = mean(arr_delay < -15),
    p_15_dep_arr = mean(dep_delay < -15)
  ) %>%
  filter(p_15_tempr_arr > 0.5 | p_15_dep_arr > 0.5) %>%
  filter(count > 30) %>%
  arrange(desc(p_15_tempr_arr), desc(p_15_dep_arr))


# Vuelos con mas de 10 minutos de tardanza
no_cancelados %>%
  group_by(tailnum, origin, dest) %>%
  summarise(
    count = n(),
    arr_delay_10_c = sum(arr_delay > 10),
    arr_delay_10_p = mean(arr_delay > 10),
    dep_delay_10_c = sum(dep_delay > 10),
    dep_delay_10_p = mean(dep_delay > 10)
  ) %>%
  filter(count > 20) %>%
  arrange(desc(arr_delay_10_p))


#Vuelos con 10 minutos de probabilidad de tardanza

no_cancelados %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    exact_10 = mean(arr_delay == 10)
  ) %>%
  filter(count > 10) %>%
  arrange(desc(exact_10))


# Seleccionamos Seattle con un enfoque de 30 minutos
# Vuelos con 30 minutos adelantados o de tardanza
#
no_cancelados %>%
  group_by(tailnum) %>%
  mutate(
    count = n(),
    arr_30_early = mean(arr_delay < -30),
    dep_30_early = mean(dep_delay < -30),
    arr_30_late  = mean(arr_delay > 30),
    dep_30_late = mean(dep_delay > 30)
  ) %>%
  filter(count > 20) %>%
  arrange(desc(arr_30_early), desc(dep_30_early), arr_30_late, dep_30_late) %>%
  select(dest)


# 5.7.1 Calculamos el retraso máximo de llegada y la proporcion de puntualidad de la compañia
#"tailum"
no_cancelados  %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    max_arr_delay = max(arr_delay),
    is_on_time_freq = mean(arr_delay <= 0, na.rm = TRUE)
  ) %>%
  filter(count > 30) %>%
  arrange(desc(is_on_time_freq))

