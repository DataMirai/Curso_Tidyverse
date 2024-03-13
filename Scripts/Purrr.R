pacman::p_load(tidyverse)

Temperatura <- read.csv("C:/Users/Aitor/Desktop/UAB_Medicina/Curso_Tidyverse/Data/temperature.csv") %>% 
  as_tibble()

# se trata de tres temperaturas registradas simultáneamente en una pieza electrónica
# será muy valioso poder caracterizar la temperatura transitoria de cada sensor
# queremos aplicar el mismo conjunto de modelos a los tres sensores
# será más fácil mostrarlo mediante imágenes

Temperatura_long <- Temperatura %>% 
  mutate(instant = ymd_hms(instant)) %>% 
  pivot_longer(cols = -instant, names_to = 'id_sensor', values_to = 'temperatura') %>% 
  mutate(id_sensor  = as_factor(toupper(str_replace(id_sensor, 'temperature_','')) ) )

Temperatura_long

Temperatura_long %>%  
  ggplot(aes(x= instant, y=temperatura, color= id_sensor))+
  geom_line()


Temperatura_delta <- Temperatura_long %>%
  arrange(id_sensor, instant) %>%
  group_by(id_sensor) %>%
  mutate(
    delta_tiempo = as.numeric(instant - instant[1]),
    delta_temperatura = temperatura - temperatura[1]) %>%
  select(id_sensor, delta_tiempo, delta_temperatura) %>% 
  ungroup()

Temperatura_delta

Temperatura_delta %>%
  ggplot(aes(x = delta_tiempo, y = delta_temperatura, color = id_sensor)) +
  geom_line()

# Funciones -----

## Apoyo auxiliar ----
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
erfc <- function(x) {2 * pnorm(x * sqrt(2), lower.tail = FALSE)}

## Newton cooling ----
newton_cooling <- function(Temperatura_data) {
  nls(
    delta_temperatura ~ delta_temperatura_0 * (1 - exp( -delta_tiempo/tau_0)),
    start = list(
      delta_temperatura_0 = -10, 
      tau_0 = 50),
    data = Temperatura_data
  )
}

## Semi_infinite_simple ----
semi_infinite_simple <- function(Temperatura_data) {
  nls(
    delta_temperatura ~ delta_temperatura_0 * erfc(sqrt(tau_0/delta_tiempo)),
    start = list(
      delta_temperatura_0 = -10, 
      tau_0 = 50),
    data = Temperatura_data
  )    
}

## Semi_infinite_convection ----

semi_infinite_convection <- function(Temperatura_data){
  nls(
    delta_temperatura ~
      delta_temperatura_0 * (
        erfc(sqrt(tau_0/delta_tiempo )) -
          exp(Bi_0 + (Bi_0/2)^2*delta_tiempo /tau_0)*
          erfc(sqrt(tau_0/delta_tiempo ) + (Bi_0/2)*sqrt(delta_tiempo /tau_0))
      ),
    start = list(
      delta_temperatura_0 = -5, 
      tau_0 = 50, 
      Bi_0 = 1.e6),
    data = Temperatura_data
  )
}






Model_NewtonCooling_A <- Temperatura_delta %>% 
  filter(id_sensor == 'A') %>%
  newton_cooling()

predicciones_newtocooling_A <- Temperatura_delta %>%
  filter(id_sensor == 'A') %>%
  mutate(modelado = predict(Model_NewtonCooling_A, data = .)) %>% 
  select(id_sensor, delta_tiempo, observado= delta_temperatura, modelado) %>% 
  pivot_longer(cols = c(observado, modelado), names_to = 'tipo', values_to = 'delta_temperatura')


predicciones_newtocooling_A %>%
  ggplot(aes(x = delta_tiempo, y = delta_temperatura, linetype= tipo )) +
  geom_line()+
  # cambio de 
  scale_linetype_manual(values=c('dashed','solid'))


Model_semi_infinite_convection_C <- Temperatura_delta %>% 
  filter(id_sensor == 'C') %>%
  newton_cooling()

predicciones_semi_infinite_convection_C <- Temperatura_delta %>%
  filter(id_sensor == 'C') %>%
  mutate(modelado = predict(Model_NewtonCooling_A, data = .)) %>% 
  select(id_sensor, delta_tiempo, observado= delta_temperatura, modelado) %>% 
  pivot_longer(cols = c(observado, modelado), names_to = 'tipo', values_to = 'delta_temperatura')


predicciones_semi_infinite_convection_C %>%
  ggplot(aes(x = delta_tiempo, y = delta_temperatura, linetype= tipo )) +
  geom_line()+
  # cambio de 
  scale_linetype_manual(values=c('dashed','solid'))







# ¿Como hacerlo todo junto? ------
# Tenemos que probar 3 funciones, newtoncooling, semi_infinite_simple, semi_infinite_convection sobre 3 sensores
# 9 combinaciones

Temperatura_delta_nested <- Temperatura_delta %>% 
  group_by(id_sensor) %>% 
  nest() %>%  
  ungroup()

Models_functions_list <-
  list(
    'newton_cooling' = newton_cooling,
    'semi_infinite_simple' = semi_infinite_simple,
    'semi_infinite_convection' = semi_infinite_convection
  )


fn_model <- function(.model, df){
  # safer to avoid non-standard evaluation
  # df %>% mutate(model = map(data, .model)) 
  df %>% 
    mutate('model'= map(data, possibly(.model, NULL)))
}

Temperatura_nested_models <- list(
  'newton_cooling' = newton_cooling,
  'semi_infinite_simple' = semi_infinite_simple,
  'semi_infinite_convection' = semi_infinite_convection) %>%
  map_df(fn_model, Temperatura_delta_nested, .id = "id_model") 

Temperatura_nested_models


Temperatura_nested_valid_models <- Temperatura_nested_models %>% 
  mutate(is_null = map_lgl(model, is.null)) %>%
  filter(!is_null) %>%
  select(-is_null)

Temperatura_nested_valid_models %>%
  mutate(pred = map2(model,data, predict))

Temperatura_nested_valid_models %>%
  mutate(pred = map2(data,model, ~predict(object=..2,..1)))

Temperatura_nested_valid_models %>%
  mutate(pred = pmap(list(data,model), ~predict(object=..2,..1)) ) %>%  unnest(c(data, pred))

