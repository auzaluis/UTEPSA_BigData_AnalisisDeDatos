
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
  
  ggplot(aes(x = Marca,
             y = proporción,
             fill = Marca,
             label = porcentaje)) +
  
  geom_col() +
  geom_label(fill = "white") +
  
  labs(title = "Lealtad de marca",
       subtitle = "¿Cuál es la marca que más compra?",
       caption = "Johnnie Walker es la marca con mayor índice de lealtad") +
  
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())




































