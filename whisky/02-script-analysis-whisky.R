
# library(tidyverse)
# install.packages("tidyverse")

# Llamar al módulo ETL
source("whisky/01-script-ETL-whisky.R")

glimpse(DF3)

DF3 <- DF3 %>% 
  as_tibble() %>% 
  mutate(across(.cols = everything(),
                .fns = ~ gsub("Johnny Walker", "Johnnie Walker", .)))



# Generando gráficos ----

## Marca que más compra (lealtad)

tabla_lealtad <- DF3 %>% 
  #contar los casos
  count(`¿Cuál es la marca que más compra?`) %>% 
  # ordenar la tabla
  arrange(desc(n)) %>% 
  # eliminar NINGUNO
  filter(`¿Cuál es la marca que más compra?` != "Ninguno") %>% 
  # calculando proporciones
  mutate(proporción = n/sum(n),
         porcentaje = scales::percent(proporción))



## Exportando a Excel
library(openxlsx)
write.xlsx(x = tabla_lealtad,
           file = "whisky/lealtad.xlsx")



## Gráfico de columnas
library(viridis)

tabla_lealtad %>% 
  # renombrando una columna
  rename(Marca = `¿Cuál es la marca que más compra?`) %>% 
  
  # reordenando los niveles de Marca
  mutate(Marca = fct_reorder(Marca, n, .desc = T)) %>% 
  
  ggplot(aes(x = Marca,
             y = proporción,
             fill = Marca,
             label = porcentaje)) +
  
  geom_col() +
  geom_label(fill = "white") +
  
  labs(title = "Lealtad de marca",
       subtitle = "¿Cuál es la marca que más compra?",
       caption = "Johnnie Walker es la marca con mayor índice de lealtad") +
  
  # scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())



## Prueba de marca

DF3 %>% 
  pivot_longer(cols = starts_with("Prueba"),
               names_to = "Variable",
               values_to = "Marca") %>%
  select(Marca) %>% 
  na.omit() %>% 
  count(Marca) %>% 
  mutate(proporción = n/nrow(DF3),
         porcentaje = scales::percent(proporción),
         Marca = fct_reorder(Marca, n, .desc = T)) %>% 
  
  ggplot(aes(x = Marca,
             y = proporción,
             fill = Marca,
             label = porcentaje)) +
  
  geom_col() +
  geom_label(fill = "white") +
  
  labs(title = "Niveles de Prueba",
       subtitle = "¿Cuáles de las siguientes marcas ha probado?") +
  
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank())



# Función que crea la tabla

tabla_multiple <- function(dataFrame, indicador) {
  
  dataFrame %>% 
    pivot_longer(cols = starts_with(indicador),
                 names_to = "Variable",
                 values_to = "Marca") %>% 
    select(Marca) %>% 
    na.omit() %>% 
    count(Marca) %>% 
    mutate(proporción = n/nrow(dataFrame),
           porcentaje = scales::percent(proporción),
           KPI = rep(indicador, times = nrow(.)))
  
}

tabla_conocimiento <- tabla_multiple(dataFrame = DF3,
                                     indicador = "Conocimiento")

tabla_prueba <- tabla_multiple(dataFrame = DF3,
                               indicador = "Prueba")

tabla_lealtad <- tabla_lealtad %>% 
  rename(Marca = `¿Cuál es la marca que más compra?`) %>% 
  mutate(KPI = rep("Lealtad", times = nrow(.)))



# Tabla de consideración

tabla_consideración <- DF3 %>% 
  select(starts_with("Consideración")) %>% 
  pivot_longer(cols = everything(),
               names_to = "Marca") %>% 
  mutate(t2b = ifelse(test = value %in% c("Creo que SÍ la consideraría",
                                          "Sería mi 1ra opción"),
                      yes = 1,
                      no = 0),
         Marca = case_when(
           Marca == "Consideración Chivas" ~ "Chivas Regal",
           Marca == "Consideración Johnny Walker" ~ "Johnnie Walker",
           Marca == "Consideración Old Parr" ~ "Old Parr",
           Marca == "Consideración Jack Daniel's" ~ "Jack Daniel's",
           .default = NA
         )) %>%
  group_by(Marca) %>% 
  summarise(n = sum(t2b)) %>% 
  ungroup() %>% 
  mutate(proporción = n/nrow(DF3),
         porcentaje = scales::percent(proporción),
         KPI = rep("Consideración", times = nrow(.)))



# Tabla de recomendación

tabla_recomendación <- DF3 %>% 
  select(starts_with("Recomendación")) %>% 
  pivot_longer(cols = everything(),
               names_to = "Marca") %>% 
  mutate(t2b = ifelse(test = value %in% c(9:10),
                      yes = 1,
                      no = 0),
         Marca = case_when(
           Marca == "Recomendación Chivas" ~ "Chivas Regal",
           Marca == "Recomendación Johnny Walker" ~ "Johnnie Walker",
           Marca == "Recomendación Old Parr" ~ "Old Parr",
           Marca == "Recomendación Jack Daniel's" ~ "Jack Daniel's",
           .default = NA
         )) %>%
  group_by(Marca) %>% 
  summarise(n = sum(t2b)) %>% 
  ungroup() %>% 
  mutate(proporción = n/nrow(DF3),
         porcentaje = scales::percent(proporción),
         KPI = rep("Recomendación", times = nrow(.)))



# Tabla concatenada

tabla_concat <- bind_rows(tabla_conocimiento,
                          tabla_consideración,
                          tabla_prueba,
                          tabla_lealtad,
                          tabla_recomendación) %>% 
  
  mutate(KPI = fct_relevel(KPI, c("Conocimiento",
                                  "Consideración",
                                  "Prueba",
                                  "Lealtad",
                                  "Recomendación")))

tabla_concat %>%
  ggplot(aes(x = KPI,
             y = proporción,
             fill = KPI,
             label = porcentaje)) +
  geom_col() +
  geom_label(fill = "white") +
  facet_wrap(~ Marca, nrow = 2) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid = element_blank())

















