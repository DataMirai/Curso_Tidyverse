# Funciones -----

## Apoyo auxiliar ----
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
erfc <- function(x) {2 * pnorm(x * sqrt(2), lower.tail = FALSE)}

## Newton cooling ----
newton_cooling <- function(Temperatura_data, delta_temperatura_0 = -10 , tau_0 = 50 ) {
  nls(
    delta_temperatura ~ delta_temperatura_0 * (1 - exp( -delta_tiempo/tau_0)),
    start = list(
      delta_temperatura_0 = delta_temperatura_0, 
      tau_0 = tau_0),
    data = Temperatura_data
  )
}

## Semi_infinite_simple ----
semi_infinite_simple <- function(Temperatura_data, delta_temperatura_0 = -10 , tau_0 = 50 ) {
  nls(
    delta_temperatura ~ delta_temperatura_0 * erfc(sqrt(tau_0/delta_tiempo)),
    start = list(
      delta_temperatura_0 = delta_temperatura_0, 
      tau_0 = tau_0),
    data = Temperatura_data
  )    
}

## Semi_infinite_convection ----

semi_infinite_convection <- function(Temperatura_data, delta_temperatura_0 = -10 , tau_0 = 50 , Bi_0 = 1.e6){
  nls(
    delta_temperatura ~
      delta_temperatura_0 * (
        erfc(sqrt(tau_0/delta_tiempo )) -
          exp(Bi_0 + (Bi_0/2)^2*delta_tiempo /tau_0)*
          erfc(sqrt(tau_0/delta_tiempo ) + (Bi_0/2)*sqrt(delta_tiempo /tau_0))
      ),
    start = list(
      delta_temperatura_0 = delta_temperatura_0, 
      tau_0 = tau_0, 
      Bi_0 = Bi_0),
    data = Temperatura_data
  )
}


all_possibilities <- dplyr::tribble(
  ~f,      ~params,
  "newton_cooling", list(data=data, delta_temperatura_0  = -10, tau_0  = 50),
  "semi_infinite_simple",  list(data=data, delta_temperatura_0  = -10, tau_0  = 50),
  "semi_infinite_convection", list(data=data, delta_temperatura_0 = -5, tau_0 = 50, Bi_0 = 1.e6)
)

crossing(Temperatura_delta_nested, all_possibilities)%>% 
  mutate(sim = invoke_map(f, params))

tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10) ) %>% 
  mutate(sim = invoke_map(f, params, n = 10))