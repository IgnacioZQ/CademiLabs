# Librerias ----

{library(RCurl)
library(tidyverse)
library(viridis)}

# Cargar Data ----

{Inv <- getURL("https://raw.githubusercontent.com/IgnacioZQ/CademiLabs/main/crunch2020/Inversion.csv")
Inv <- read.csv(text = Inv)
Inv <- na.omit(Inv)}

# Manejo Data ----

Inv_2 <- Inv %>%
  group_by(Inv$co_category_list) %>%
  summarise(sum(raised_amount_usd))

# Graficar ----

ggplot(data = Inv, aes(x = Inv$co_category_list , y = Inv$funding_date, size = Inv$raised_amount_usd)) +
  geom_point(alpha=0.04) +
  theme (text = element_text(size=10)) +
  coord_flip() +
  theme_light() +
  ylab("Año") +
  xlab("Categoria") +
  labs(title = paste(""), 
       caption="FUENTE: Base de Datos Crunchbase 2020",
       subtitle = paste("Entre 1995 - 2020")) +
  scale_size_continuous(range=c(1, 23), labels = scales::dollar_format(), name = "Investment in USD") +
  scale_color_continuous(guide = FALSE, labels = scales::dollar_format())
