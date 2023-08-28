library(tidyverse)

# 5.2.4 item 1  retraso de dos o mas horas
vuelos <- nycflights13 ::flights
vuelos_con_retraso <- filter (vuelos, arr_delay >= "2",
                              na.rm=TRUE)

# 5.2.4 item 2, vuelos que llegan a HOUSTON, HOU o IAH

vue_dest_hou <- filter (vuelos, dest == "HOU" | dest == "IAH"
                        ,na.rm=TRUE)


# 5.3.1 item 1 ordenar valores faltantes usando is.na

organizar <- vuelos%>%
  arrange (desc(is.na(dep_time)))

# 5.3.1 item 2 Orden de los vuelos mas retrasados

vue_conmayorretraso <- vuelos%>%
  arrange (desc(dep_delay))

# 5.3.1 item 2 Orden de los vuelos adelantados

vue_adel <- vuelos%>%
  arrange (dep_delay)

# 5.3.1 item 3 Orden de los vuelos mas rapidos (gastaron menos tiempo)
vue_rapid <- vuelos%>%
  arrange (hour, minute)

# 5.3.1 item 4 Orden de los vuelos mas lejanos

vue_lejos <- vuelos%>%
  arrange (desc(distance))

# 5.3.1 item 4 Orden de los vuelos mas cercanos
vue_cerca <- vuelos%>%
  arrange (distance)

# 5.4.1 item 2 llamar una variable varias veces, si llamo varias veces
#la misma variable no cambia en nada, muestra unicamente esa variable, una sola vez.

call_var <-  vuelos%>%
  select(origin, origin, origin)

# 5.4.1 item 3 funcion any_of se puede decirle al motor de expresiones que asocie solo unas variables en especifico.
#Simplemente coloque los caracteres que desea mostrar
rp <-  vuelos%>%
  select(any_of(c("year", "month", "day", "dep_delay", "arr_delay")))

# 5.4.1 item 4 los datos mostrados son de variables que incluyen la
#palabra TIME, si cambio por otra palabra en comun me mostrara unicamente las variables que tengan esta palabra

rp1 <-  vuelos%>%
  select(vuelos, contains("TIME"))

# 5.5.2 item 1 mostrar datos de dep_time y sched_dep_time mas parecido a una hora
#puse a dividir por 100 y ,multiplicar por 60 para tener los numeros desde las 00:00

hora_vuelos <- vuelos %>%
  select(dep_time, sched_dep_time)%>%
  mutate(minutos_deptime = (dep_time/100*60), minutos_sched = (sched_dep_time/100*60))

#5.5.2 item 2 no entendi bien este punto profe :( me ayude con internet

comparar <- vuelos %>%
  mutate(air_t = air_time /100) %>%
  select(air_t, air_time, everything()) %>%
  mutate(arr_t = arr_time/100) %>%
  select(arr_t, everything())

### 5.6.7
no_cancelados <- vuelos %>%
  filter(!is.na(air_time))

### la llegada es mas importante
no_cancelados %>%
  group_by(tailnum) %>%
  mutate(
    count = n(),
    median_arr_delay = median(arr_delay),
    median_dep_delay = median(dep_delay)
  ) %>%
  filter(count > 30) %>%
  arrange(median_arr_delay, median_dep_delay)


#De la tabla vemos que N479AA no. el avión llega
#antes de los 20 minutos el 50% del tiempo, sale antes de
#los 3 minutos el 50% del tiempo.
#la llegada es mas importante, asi que organice primero eso


no_cancelados %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    p_15_early_arr = mean(arr_delay < -15),
    p_15_dep_arr = mean(dep_delay < -15)
  ) %>%
  filter(p_15_early_arr > 0.5 | p_15_dep_arr > 0.5) %>%
  filter(count > 30) %>%
  arrange(desc(p_15_early_arr), desc(p_15_dep_arr))



#Aquí tenemos los vuelos que realizaron al menos 30 vuelos en un año y llegaron
#o salieron antes de los 15 minutos durante más del 50% del tiempo. La llegada
#es más importante, ya que la salida se puede compensar en avión.


#MÁS DE 10 MINUTOS TARDE
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


#Exatamente 10 minutos de probabilidad de tardanza

no_cancelados %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    exact_10 = mean(arr_delay == 10)
  ) %>%
  filter(count > 10) %>%
  arrange(desc(exact_10))


### SELECCIONE SEATTLE PARA UN ENFOQUE DE 30 MINUTOS
### MEJORES VUELOS MENOS DE 30 MINUTOS CRITERIOS TEMPRANO O TARDÍO ORDENADOS
###
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

# 5.7.1 Calcule el retraso máximo de llegada y la proporción de puntualidad.
no_cancelados  %>%
  group_by(tailnum) %>%
  summarise(
    count = n(),
    max_arr_delay = max(arr_delay),
    is_on_time_freq = mean(arr_delay <= 0, na.rm = TRUE)
  ) %>%
  filter(count > 30) %>%
  arrange(desc(is_on_time_freq))

