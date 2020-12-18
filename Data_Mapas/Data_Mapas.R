# Data Mapas
install.packages("ggplot2")
install.packages("sf")
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lubridate)


library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(lubridate)
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


