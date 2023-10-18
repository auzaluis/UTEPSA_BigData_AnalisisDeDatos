
# Tema 1: Carga de datos ----

## Carga local
DF <- read.csv(file = "Personalidad y uso de apps (respuestas).csv",
               check.names = F)
colnames(DF)



## Carga en línea
install.packages("gsheet")
library(gsheet)

url_google <- "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"

DF <- read.csv(text = gsheet2text(url = url_google),
               check.names = F)



# Estructura de un data frame ----
class(DF)
class(DF$`Escribe tu edad exacta`)
nrow(DF) # Cantidad de filas del data frame
ncol(DF) # Cantidad de columnas del data frame



# Tema 2: Transformación del data frame ---- 

## Valores perdidos (NA) ----

# Los NA pueden ser tratados de 2 maneras:
# 1. Imputarlos (reemplazarlos)
# 2. Eliminarlos

DF$`Escribe tu edad exacta`
is.na(DF$`Escribe tu edad exacta`)
summary(is.na(DF$`Escribe tu edad exacta`))

### Reemplazo por el promedio
install.packages("tidyverse")
library(tidyverse)


DF2 <- DF %>% 
  
  mutate(edad2 = ifelse(test = is.na(`Escribe tu edad exacta`),
                        yes = mean(`Escribe tu edad exacta`, na.rm = T),
                        no = `Escribe tu edad exacta`)) %>% 
  
  relocate(edad2, .after = `Escribe tu edad exacta`)



### Eliminar la fila completa

# DF2 <- DF %>% na.omit()
DF2 <- na.omit(DF)

nrow(DF)
nrow(DF2)



## Estandarización de variables ----







