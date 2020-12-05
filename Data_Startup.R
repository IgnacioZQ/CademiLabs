library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(forcats)
library(countrycode)

# Observaciones

# Que tipo de startups tiene mas exito.

# Que tipo de Startups ya no se pueden sustentar.

# Cuales son las Startups mas estables.

# Datos a trabajar: Año de la inversión, Inversiones, Categoria de la inversión (Company_Category_Code).

# Pregunta / Observación 1: Cual es la categoria de Startup con mayores inversiones a lo largo de los años (Gráfico de Linea)

# Pregunta / Observación 2: Ver el total e inversión en cada categoria y comparar cual ha recibido mayor inversión en el tiempo (Gráfico de Barra / Columna).

### Cargar base de datos

crunchbase_investments <- read_csv("crunch2013/crunchbase-investments.csv")

### Limpiar base de datos.

Dataset <- select(crunchbase_investments, company_category_code, funded_year, raised_amount_usd)

# Para graficar y realizar los análisis usar la base de datos Data_Limpia.csv.

Data_Limpia <- read_csv("crunch2013/Data_Limpia.csv")

#Filraremos solamente las inversiones sobre 10.000.000 USD desde el año 1995 hasta 2013

Data_Limpia_2 <- filter(Data_Limpia, Data_Limpia$raised_amount_usd >= 10000000,
                        Data_Limpia$funded_year >= 1995)
Data_Limpia_2 <- na.omit(Data_Limpia_2) # (Eliminar NA y valores nulos)

# IDEA: Podriamos segmentar las variables en varios tipos.
# Ej: ECONOMY (ecommerce, enterpise, finance) TECHNOLOGY (analytics, game_video, mobile, nanotech, software)

### Grafica

ggplot(data = Data_Limpia_2, aes(x = company_category_code, y = raised_amount_usd)) +
  geom_segment(aes(x = company_category_code, xend = company_category_code, y = 0, yend = raised_amount_usd)) +
  geom_point(size=0.2, color= "orange", alpha=0.5, shape=21, stroke=2) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  coord_flip() +
  labs(title = paste("Movimientos de Inversiones a Startups entre 1995 - 2013 a partir de los 10.000.000 USD"), 
       caption = "FUENTE: Base de datos Crunchbase", 
       y = "Inversion en USD", 
       x = ""
  )
