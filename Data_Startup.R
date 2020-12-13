library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lubridate)

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

# ¿Cuales han sido las inversiones de Startups a lo largo del tiempo?

##Gráfico 1 (Plantilla)

ggplot(data = Data_Limpia_2, aes(x = company_category_code, y = raised_amount_usd, color = funded_year)) +
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

## Observaciones Gráfico 1

# 32 de las Startups no recibieron inversiones por sobre los 1.000.000.000 USD, lo que equivale al 78% del total de las Startups. Por ende, aquellas que recibieron por sobre los 1.000.000.00 USD equivalen a 22%.
# Las startups con mayores inversiones son Mobile, Health y Social. (liderando Mobile).
# Durante el año 2000 hubo un incremento en inversiones de Startups Mobile.
# Las Startups que mas tuvieron inversion tardía (2006 - 2013) fueron Health, Education, Network Hosting, Security.
# A partir del año 2005 se comenzó a ver un alto incremento en Inversiones de Startups.


## Gráfico de burbuja 1

ggplot(Data_Limpia_2, mapping = aes(x = funded_year, y = raised_amount_usd, size = raised_amount_usd, colour = raised_amount_usd)) +
  geom_point(alpha=0.4) +
  scale_size_continuous(range=c(1, 24)) +
  scale_colour_continuous(guide = FALSE) +
  theme_ipsum() +
  theme(legend.position="bottom") +
  ylab("Investment in USD") +
  xlab("Year") +
  theme(legend.position = "none") +
  labs(title="Inversiones a Startups entre 1995 - 2013 a partir de los 10.000.000 USD", 
       caption="FUENTE: Base de Datos Crunchbase 2013") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme (text = element_text(size=10)) +
  geom_text(data = Data_Limpia_2 %>% filter(raised_amount_usd >= 600000000), aes(x = funded_year, y = raised_amount_usd, label = company_category_code), color="black", fontface="bold",alpha= 0.5, size=3.5, inherit.aes = FALSE)


## Gráfico de burbuja 2

ggplot(data = Data_Limpia_2, aes(x = company_category_code, y = funded_year, color = raised_amount_usd, size = raised_amount_usd)) +
  geom_point(alpha=0.04) +
  theme (text = element_text(size=10)) +
  coord_flip() +
  theme_light() +
  ylab("Year") +
  xlab("Company Category") +
  labs(title="Inversiones a Startups entre 1995 - 2013 a partir de los 10.000.000 USD", 
       caption="FUENTE: Base de Datos Crunchbase 2013") +
  scale_size_continuous(range=c(1, 23), labels = scales::dollar_format(), name = "Investment in USD") +
  scale_color_continuous(guide = FALSE, labels = scales::dollar_format()) +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A")

# Observaciónes Gráfico 2
# El tamaño de la burbuja es según las inversiones unitarias por año. Al haber varias inversiones en un mismo año, se encuentran sobrecolocadas unas sobre otras.
# Las Startups que mas tuvieron inversion temprana (1995 - 2005) fueron Mobile, Manufacturing, Public Relations.
# Web y Software han sido las Startups que mas han mantenido inversiones constantes a lo largo del tiempo.
# La Startup con mayor magnitud de inversiones fué Mobile.
# A partir del 2005 Biotech, Cleantech, Ecommerce, Education, Medical, Advertising,  Mobile, Nanotech, Network_Hosting, Security y software Tvieron un impulso de Inversiones a partir del 2005.
# Las Satartups que tuvieron menos inversiones y no se mantuvieron con el tiempo fueron Legal, Local, Design, Pets, Sport y Nonprofit.
