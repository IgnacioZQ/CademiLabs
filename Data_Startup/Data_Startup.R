#Librerias

{library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lubridate)
library(RCurl)
library(car)
library(psych)
library(ggpubr)
library(ggthemes)
library(RCurl)
library(magrittr)
library(knitr)}

# Observaciones2

# Que tipo de startups tiene mas exito.

# Que tipo de Startups ya no se pueden sustentar.

# Cuales son las Startups mas estables.

# Datos a trabajar: Año de la inversión, Inversiones, Categoria de la inversión (Company_Category_Code).

# Pregunta / Observación 1: Cual es la categoria de Startup con mayores inversiones a lo largo de los años (Gráfico de Linea)

# Pregunta / Observación 2: Ver el total e inversión en cada categoria y comparar cual ha recibido mayor inversión en el tiempo (Gráfico de Barra / Columna).

# Para graficar y realizar los análisis usar la base de datos Data_Limpia.csv.

{Data_Limpia <- getURL("https://raw.githubusercontent.com/IgnacioZQ/CademiLabs/main/crunch2013/crunchbase-investments.csv")
Data_Limpia <- read.csv(text = Data_Limpia)}

#Filraremos solamente las inversiones sobre 10.000.000 USD desde el año 1995 hasta 2013

{Data_Limpia_2 <- filter(Data_Limpia, Data_Limpia$raised_amount_usd >= 100000, Data_Limpia$funded_year >= 1995)

Data_Limpia_2 <- na.omit(Data_Limpia_2) # (Eliminar NA y valores nulos)

Data_Limpia_3 <- select(Data_Limpia_2, company_category_code, funded_year, raised_amount_usd) %>%
  filter(Data_Limpia_2$company_category_code!= "")}

#### Cambio de nombres a español y mayuscula.

{ Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "sports"] = "Deporte"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "biotech"] = "Biotecnologia"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "software"] = "Software"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "mobile"] = "Móvil"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "cleantech"] = "Tecnologias Limpias"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "enterprise"] = "Empresa"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "web"] = "Web"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "medical"] = "Medicina"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "advertising"] = "Publicidad"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "ecommerce"] = "Comercio Electronico"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "network_hosting"] = "Servidor de Red"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "semiconductor"] = "Semiconductor"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "hardware"] = "Hardware"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "games_video"] = "Videojuegos"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "analytics"] = "Analítica"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "security"] = "Seguridad"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "social"] = "Social"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "finance"] = "Finanzas"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "health"] = "Salud"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "nanotech"] = "Nanotecnologia"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "public_relations"] = "Relaciones Publicas"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "automotive"] = "Automotriz"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "other"] = "Otros"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "search"] = "Busqueda"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "education"] = "Educación"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "manufacturing"] = "Fabricación"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "news"] = "Noticias"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "fashion"] = "Moda"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "travel"] = "Viaje"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "messaging"] = "Mensajeria"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "consulting"] = "Consultoria"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "transportation"] = "Transporte"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "music"] = "Musica"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "photo_video"] = "Fotografia y Video"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "hospitality"] = "Hoteleria"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "real_estate"] = "Bienes Raices"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "nonprofit"] = "Organización sin Fines de Lucro"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "legal"] = "Legal"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "design"] = "Diseño"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "sports"] = "Deportes"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "local"] = "Comercio Local"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "pets"] = "Mascotas"
  Data_Limpia_3$company_category_code[Data_Limpia_3$company_category_code == "government"] = "Gobierno"}

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
  geom_text(data = Data_Limpia_3 %>%
              filter(raised_amount_usd >= 600000000), aes(x = funded_year, y = raised_amount_usd, label = company_category_code), color="black", fontface="bold",alpha= 0.5, size=3.5, inherit.aes = FALSE)


## Gráfico de burbuja 2 (DEFINITIVO).

ggplot(data = Data_Limpia_3, aes(x = company_category_code, y = funded_year, size = raised_amount_usd)) +
  geom_point(alpha=0.04) +
  theme (text = element_text(size=10)) +
  coord_flip() +
  theme_light() +
  ylab("Year") +
  xlab("Company Category") +
  labs(title = paste("Inversiones a Startups entre 1995 - 2013 a partir de los 100.000 USD"), 
       caption="FUENTE: Base de Datos Crunchbase 2013",
       subtitle = paste("Entre 1995 - 2013")) +
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

Total_Inv_Cat %>%
  arrange(`sum(raised_amount_usd)`) %>%
  mutate(company_category_code = factor(company_category_code, levels = company_category_code)) %>%
  ggplot(aes(x = company_category_code, y = `sum(raised_amount_usd)`)) +
  geom_segment(aes(x = company_category_code, xend = company_category_code, y = `sum(raised_amount_usd)`, yend = 0), size = 1.6, color = "turquoise2") +
  theme_light() +
  geom_point( size = 3.5, color="turquoise3") +
  geom_point( size = 1.5, color="white") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Top Categorias de Startups de la Década", 
       caption = "FUENTE: Base de Datos Crunchbase 2013",
       subtitle = "Total de Inversiones a Startups por Categoria a partir de los 100.000 USD entre 1995 - 2013") +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(20000000000, 40000000000, 60000000000, 80000000000, 100000000000)) +
  ylab("Inversiones en USD") +
  xlab("")

# Datos extra

summary(Total_Inv_Cat)

## Gráfico "Total de Inversiones por Año"

{Total_Inv_Year <- Data_Limpia_3 %>%
  group_by(Data_Limpia_3$funded_year) %>%
  summarise(sum(raised_amount_usd))
Total_Inv_Year <- Total_Inv_Year %>%
  filter(Total_Inv_Year$`Data_Limpia_3$funded_year` >= 2005)}

ggplot(data = Total_Inv_Year, aes(x = `Data_Limpia_3$funded_year`, y = `sum(raised_amount_usd)`)) +
  geom_segment(aes(x = `Data_Limpia_3$funded_year`, xend = `Data_Limpia_3$funded_year`, y = `sum(raised_amount_usd)`, yend = 0), size = 12, color = "cyan3") +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = paste("El Boom de las Startups"), 
       caption="FUENTE: Base de Datos Crunchbase 2013",
       subtitle = paste("Total de Inversiones a Startups por Año a partir de los 100.000 USD entre 1995 - 2013")) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(20000000000, 40000000000, 60000000000, 80000000000, 100000000000)) +
  scale_x_continuous(breaks = c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)) +
  ylab("Inversiones en USD") +
  xlab("Año")
# Se omitieron inversiones a años anteriores a 2005 dado que puede que exista la
# postibilidad de que hayan datos perdidos o registros nulos (debido al cambio
# brusco que sufren las inversiones de un año a otro).

# Datos extra

summary(Total_Inv_Year)

### Notas:
# Dividir la columna de Inversiones por 1000 USD.


