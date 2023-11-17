
library(ggcorrplot)
library(FactoMineR)
library(tidyverse)

# Matriz de correlaciones ----
frases3 <- as.vector(frases2)

# Al ser variables dicotómicas, usamos spearman o pearson
r <- cor(
  x = DF6 %>% select(all_of(frases3)),
  method = "spearman"
)



# Gráfico de correlaciones

ggplotly(
  ggcorrplot(corr = r,
             # type = "upper",
             colors = c("red", "white", "blue"),
             show.legend = F,
             tl.cex = 14) +
    
    theme(axis.text.x = element_blank(),
          panel.grid.major = element_blank())
)



# PCA: Principal Component Analysis

## Dimensión 01: Innovación

### Crear un vector que contenga las variables
innovación <- frases3[c(5,7,19)]

### Crear la dimensión
PCA.innovación <- FactoMineR::PCA(
  DF6 %>% select(all_of(innovación)),
  ncp = 1
)



### Eigenvalues y variación explicada
PCA.innovación$eig


### Correlación entre la dimensión y las var originales
PCA.innovación$var$cor


### Valores de la dimensión creada
head(PCA.innovación$ind$coord)



### Comparando los valores originales con la dimensión
tibble(
  dim = PCA.innovación$ind$coord * -1,
  DF6 %>% select(all_of(innovación))
) %>% View()



DF10 <- DF6 %>% 
  mutate(innovación = PCA.innovación$ind$coord*-1) #porque la corr son negativas



# Tarea para evaluación del módulo
## 1. Crear otras 4 dimensiones
## 5. Anexar las dimensiones creadas al DF6





