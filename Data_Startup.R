library(readr)
library(dplyr)

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

### Grafica
