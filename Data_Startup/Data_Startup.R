library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lubridate)
library(RCurl)

# Observaciones2

# Que tipo de startups tiene mas exito.

# Que tipo de Startups ya no se pueden sustentar.

# Cuales son las Startups mas estables.

# Datos a trabajar: Año de la inversión, Inversiones, Categoria de la inversión (Company_Category_Code).

# Pregunta / Observación 1: Cual es la categoria de Startup con mayores inversiones a lo largo de los años (Gráfico de Linea)

# Pregunta / Observación 2: Ver el total e inversión en cada categoria y comparar cual ha recibido mayor inversión en el tiempo (Gráfico de Barra / Columna).

# Para graficar y realizar los análisis usar la base de datos Data_Limpia.csv.

Data_Limpia <- getURL("https://raw.githubusercontent.com/IgnacioZQ/CademiLabs/main/crunch2013/crunchbase-investments.csv")
Data_Limpia <- read.csv(text = Data_Limpia)

#Filraremos solamente las inversiones sobre 10.000.000 USD desde el año 1995 hasta 2013

Data_Limpia_2 <- filter(Data_Limpia, Data_Limpia$raised_amount_usd >= 10000000, Data_Limpia$funded_year >= 1995)

Data_Limpia_2 <- na.omit(Data_Limpia_2) # (Eliminar NA y valores nulos)

Data_Limpia_3 <- select(Data_Limpia_2, company_category_code, funded_year, raised_amount_usd)

# IDEA: Podriamos segmentar las variables en varios tipos.
# Ej: ECONOMY (ecommerce, enterpise, finance) TECHNOLOGY (analytics, game_video, mobile, nanotech, software)

### Grafica

# ¿Cuales han sido las inversiones de Startups a lo largo del tiempo?

##Gráfico 1 (Plantilla)

ggplot(data = Data_Limpia_3, aes(x = company_category_code, y = raised_amount_usd, color = funded_year)) +
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

ggplot(Data_Limpia_3, mapping = aes(x = funded_year, y = raised_amount_usd, size = raised_amount_usd, colour = raised_amount_usd)) +
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
  geom_text(data = Data_Limpia_3 %>% filter(raised_amount_usd >= 600000000), aes(x = funded_year, y = raised_amount_usd, label = company_category_code), color="black", fontface="bold",alpha= 0.5, size=3.5, inherit.aes = FALSE)


## Gráfico de burbuja 2 (DEFINITIVO).

ggplot(data = Data_Limpia_3, aes(x = company_category_code, y = funded_year, color = raised_amount_usd, size = raised_amount_usd)) +
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

## Gráfico "Total de Inversiones por Categoria"

Total_Inv_Cat <- Data_Limpia_3 %>%
  group_by(company_category_code) %>%
  summarise(sum(raised_amount_usd))

ggplot(data = Total_Inv_Cat, aes(x = company_category_code, y = `sum(raised_amount_usd)`)) +
  geom_segment(aes(x = company_category_code, xend = company_category_code, y = `sum(raised_amount_usd)`, yend = 0), size = 1.5, color = "gray") +
  theme_light() +
  geom_point( size = 3, color="gray") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title="Total de Inversiones a Startups entre 1995 - 2013 por Categoria a partir de los 10.000.000 USD", 
       caption="FUENTE: Base de Datos Crunchbase 2013") +
  scale_y_continuous(labels = scales::dollar_format()) +
  ylab("Investment in USD") +
  xlab("Category")

## Gráfico "Total de Inversiones por Año"

Total_Inv_Year <- Data_Limpia_3 %>%
  group_by(Data_Limpia_3$funded_year) %>%
  summarise(sum(raised_amount_usd))

ggplot(data = Total_Inv_Year, aes(x = `Data_Limpia_3$funded_year`, y = `sum(raised_amount_usd)`)) +
  geom_segment(aes(x = `Data_Limpia_3$funded_year`, xend = `Data_Limpia_3$funded_year`, y = `sum(raised_amount_usd)`, yend = 0), size = 8, color = "gray") +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title="Total de Inversiones a Startups por Año entre 1995 - 2013 a partir de los 10.000.000 USD", 
       caption="FUENTE: Base de Datos Crunchbase 2013") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(breaks = c(1995, 1997, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013)) +
  ylab("Investment in USD") +
  xlab("Year")
