library(readr)
library(dplyr)
library(ggplot2)

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

#Filraremos solamente las inversiones sobre 10.000.000 USD desde el año 2000 hasta 2013

Data_Limpia_2 <- filter(Data_Limpia, Data_Limpia$raised_amount_usd >= 10000000,
                        Data_Limpia$funded_year >= 2000)
Data_Limpia_2 <- na.omit(Data_Limpia_2) # (Eliminar NA y valores nulos)

# IDEA: Podriamos segmentar las variables en varios tipos.
# Ej: ECONOMY (ecommerce, enterpise, finance) TECHNOLOGY (analytics, game_video, mobile, nanotech, software)

### Grafica
