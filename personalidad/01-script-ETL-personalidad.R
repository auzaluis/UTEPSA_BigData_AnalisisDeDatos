
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



## Estructura de un data frame
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

### Normalización

scale(DF2$`Escribe tu edad exacta`)



# comparando la variable original con la normalizada

data.frame(
  original = DF2$`Escribe tu edad exacta`,
  normalizado = scale(DF2$`Escribe tu edad exacta`)
) %>% head()



# Creando DF3 con variable de edad normalizada

DF3 <- DF2 %>%
  mutate(edadZ = scale(DF2$`Escribe tu edad exacta`)) %>% 
  relocate(edadZ, .after = `Escribe tu edad exacta`)



### Rango

library(scales)

rescale(DF3$`Escribe tu edad exacta`)

data.frame(
  original = DF3$`Escribe tu edad exacta`,
  rango = rescale(DF3$`Escribe tu edad exacta`)
)



## Agrupaciones ----

### Numéricas

DF4 <- DF3 %>% 
  
  # Crear la agrupación
  mutate(edadGR = cut(`Escribe tu edad exacta`,
                      breaks = c(-Inf, 18, 21, Inf),
                      labels = c("18 o menos", "19 a 21", "Más de 21"))) %>% 
  
  # Reubicar la variable
  relocate(edadGR, .after = `Escribe tu edad exacta`)


summary(DF4$edadGR)



### Categóricas

# Valores únicos de una variable categórica

unique(DF4$`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [Me gusta comprar marcas que representen mi status]`)
unique(DF4[,8])

ifelse(test = DF4[,8] == "Un poco verdadero" | DF4[,8] == "Totalmente verdadero",
       yes = 1,
       no = 0)



## Bucles ----

# Paso 1: Crear un vector que contenga los nombres de las variables

frases <- DF4 %>% 
  select(starts_with("Según tu")) %>% 
  colnames()

frases

# Paso 2: crear bucle

DF5 <- DF4

for (frase in frases) {
  
  DF5[,frase] <- ifelse(
    test = DF5[,frase] == "Totalmente verdadero" | DF5[,frase] == "Un poco verdadero",
    yes = 1,
    no = 0
  )
  
}



# Tema 3: Manipulación de datos ----

# Convirtiendo el data frame en tibble
DF5 <- DF5 %>% as_tibble()

## Función select: columnas ----
DF5 %>% select(Sexo)
DF5 %>% select(Sexo, `Escribe tu edad exacta`)
DF5 %>% select(-`Marca temporal`)
DF5 %>% select(starts_with("edad"))
DF5 %>% select(ends_with("00:00"))
DF5 %>% select(contains("edad"))



## Función filter: filas ----

DF5 %>% filter(Sexo == "Hombre")

DF5 %>% filter(Sexo != "Mujer") # Haciendo uso de la negación, mismo resultado

DF5 %>% filter(`Escribe tu edad exacta` >= 21)

DF5 %>% filter(`Escribe tu edad exacta` <= 21)

DF4 %>% filter(`Según tu forma de ser ¿Cuál de las siguientes frases te describe mejor: [No discrimino y trato a todos por igual]` %in%
                 c("No lo sé", "Un poco falso", "Totalmente falso"))

# 4 maneras de llegar al mismo resultado
DF5 %>% filter(between(x = `Escribe tu edad exacta`,
                       left = 18,
                       right = 21))

DF5 %>% filter(`Escribe tu edad exacta` %in% c(18:21))

DF5 %>% filter(`Escribe tu edad exacta` >= 18 &
                 `Escribe tu edad exacta` <= 21)

DF5 %>% filter(`Escribe tu edad exacta` >= 18,
               `Escribe tu edad exacta` <= 21)

# Filtro con más de una variable
DF5 %>% filter(`Escribe tu edad exacta` >= 18,
               `Escribe tu edad exacta` <= 21,
               Sexo == "Mujer")



## Cambio de nombre de columnas ----
DF6 <- DF5



### APPS
#### Paso1: Crear un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

#### Paso2: Reemplazar por los valores del nuevo vector
colnames(DF6)[32:35] <- apps



### Frases
#### Paso1: Crear un vector con los nuevos nombres
frases2 <- frases %>% 
  as_tibble() %>% 
  
  separate(col = "value",
           into = c("No sirve", "Sirve"),
           sep = "\\[") %>% 
  select(Sirve) %>% 
  
  separate(col = "Sirve",
           into = c("Sirve", "No sirve"),
           sep = "\\]") %>% 
  select(Sirve) %>% 
  
  as_vector()



#### Paso2: Reemplazar por los valores del nuevo vector
colnames(DF6)[7:30] <- frases2



## Pivotado de base ----

#### Pivot longer

DF7 <- DF6 %>% 
  select(`Marca temporal`, Sexo, edadGR, all_of(apps)) %>% 
  pivot_longer(cols = apps,
               names_to = "app",
               values_to = "time")

#### Pivot wider

DF8 <- DF7 %>% 
  pivot_wider(names_from = "app",
              values_from = "time")



# Detección de outliers ----

## (paréntesis) Transformar las horas a número
# strsplit separa los textos
strsplit(x = DF7$time,
         split = ":") %>% head()

# transformación
DF7$time <- sapply(
  
  X = strsplit(x = DF7$time,
               split = ":"),
  
  function(x){
    
    x <- as.numeric(x)
    x[1] + x[2]/60 + x[3]/60^2
    
  }
  
)


## Detección gráfica

# Boxplot (feo)
boxplot(DF7$time)

# Boxplot bonito
# install.packages("plotly")
library(plotly)

ggplotly(
  
  # definimos el df con el que vamos a trabajar
  DF7 %>% 
    
    # dibujar un lienzo
    ggplot(mapping = aes(x = app,
                         y = time,
                         fill = app)) +
    
    # tipo de gráfico
    geom_boxplot() +
    
    # faceting
    # facet_wrap(~ app, nrow = 1) +
    
    # modificando el tema
    theme_minimal() +
    
    # otras opciones de tema
    theme(legend.position = "none")
  
)



DF9 <- DF7 %>% 
  
  # defininendo los outliers
  mutate(
    outlier = case_when(
      app == "Facebook"  & time > 10    ~ "outlier",
      app == "Instagram" & time > 13.28 ~ "outlier",
      app == "TikTok"    & time > 15.58 ~ "outlier",
      app == "YouTube"   & time > 8.18  ~ "outlier",
      .default = "No outlier"
    )
  ) %>% 
  
  # separando el análisis por app
  group_by(app) %>% 
  
  # imputando/reemplazando los outliers por la media
  mutate(time2 = ifelse(test = outlier == "outlier",
                        yes = mean(time, na.rm = T),
                        no = time)) %>% 
  
  ungroup()



# Uso de count, group_by y summarise

DF9 %>%
  group_by(app, Sexo) %>% 
  summarise(conteo = n(),
            promedio = mean(time2),
            desv = sd(time2))



DF9 %>% 
  count(edadGR) %>% 
  # arrange no ordena gráficas
  arrange(desc(n)) %>% 
  mutate(prop = n/sum(n))


DF9 %>% 
  count(Sexo, edadGR) %>% 
  group_by(Sexo) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup()




















