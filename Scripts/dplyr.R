pacman::p_load(tidyverse)


Iris_tibble <- iris %>%
  as_tibble()

# Filter -----

Iris_tibble

Iris_tibble %>% 
  filter(Species=='setosa')

Iris_tibble %>% 
  filter(Species=='setosa' & Sepal.Length <=2)

Iris_tibble %>% 
  filter(Species=='setosa' & Sepal.Length >=5)

between(Iris_tibble$Petal.Length,5,10)

Iris_tibble %>% 
  filter(between(Petal.Length,5,10) )

# Mutate ------

Iris_tibble %>% 
  mutate('A' = 'A')

Iris_tibble %>% 
  mutate(
    'Gran_versicolor' = case_when( 
      between(Petal.Length,5,10) & between(Petal.Width,1,4) & (Species =='versicolor') ~ TRUE,
      TRUE ~ FALSE)) %>% 
  filter(Gran_versicolor == TRUE)

Iris_tibble %>%  mutate(across(where(is.numeric), ~.x + rnorm(1,0,1) ))
  

# group_by ----
Iris_tibble %>%  
  count()

Iris_tibble %>%  
  group_by(Species) %>% 
  count()

# No cuenta NA's n8i nada mas


#Summarize ----

Iris_tibble %>%  
  group_by(Species) %>% 
  summarize(across(everything(),
    list('media'=mean, 'mediana'=median) ))


Iris_tibble %>%  
  group_by(Species) %>% 
  summarize(
    across(
      everything(),
      list(
        'min'=min,
        'Q1'=~quantile(.x,0.25), 
        'media'=mean, 
        'mediana'=median,  
        'Q3'=~quantile(.x,0.75), 
        'max'=max,
        'NA_count'=~sum(is.na(.x)),) ))


Iris_tibble %>%  
  group_by(Species) %>% 
  summarize(
    across(
      everything(),
      list('NA_count'=~sum(is.na(.x)),
           'min'=min,
           'Q1'=~quantile(.x,0.25), 
           'media'=mean, 
           'mediana'=median,  
           'Q3'=~quantile(.x,0.75), 
           'max'=max) )) %>% 
  pivot_longer(cols=-Species, names_to = 'algo', values_to = 'valores') %>% 
  separate(algo,into = c('Variable', 'funcion_summarize'), sep='_') %>% 
  pivot_wider( names_from = 'funcion_summarize', values_from = 'valores')

Iris_tibble %>%  
  group_by(Species) %>% 
  summarize(
    across(
      everything(),
      list('NA_count'=~sum(is.na(.x)),
           'min'=min,
           'Q1'=~quantile(.x,0.25), 
           'media'=mean, 
           'mediana'=median,  
           'Q3'=~quantile(.x,0.75), 
           'max'=max) )) %>% 
  pivot_longer(cols=-Species, names_to = c('variables','funcion'), values_to = 'resumen',names_sep ='_')  %>% 
  # este warning es para indicar que puedes tener resultados erróneos en caso de que separador estre duplicado
  # habría que reafinar la funcion con expresión regular.
  pivot_wider(names_from = 'funcion', values_from = 'resumen')
  
# %>% 
#   group_by(Species,variables) %>%  nest()

Iris_tibble %>%  
  group_by(Species) %>% 
  summarize(
    across(
      everything(),
      list('NA_count'=~sum(is.na(.x)),
           'min'=min,
           'Q1'=~quantile(.x,0.25), 
           'media'=mean, 
           'mediana'=median,  
           'Q3'=~quantile(.x,0.75), 
           'max'=max) )) %>% 
  pivot_longer(cols=-Species, names_to = c('variables','funcion'), values_to = 'resumen',names_sep ='_')  %>% 
  # este warning es para indicar que puedes tener resultados erróneos en caso de que separador estre duplicado
  # habría que reafinar la funcion con expresión regular.
  pivot_wider(names_from = 'funcion', values_from = 'resumen')


Iris_tibble %>% 
  slice(1)

Iris_tibble %>% 
  slice(20:50)

# hay otras formas de slice
Iris_tibble %>% 
  slice_max(Petal.Length)




