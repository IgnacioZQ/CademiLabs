# Librerias ----

{library(RCurl)
library(tidyverse)}

# Cargar Data (Como Df1) ----

{Df1 <- Df1 %>%
  filter(Df1$category != "", Df1$raised_amount_usd != "")
Df1 <- Df1 %>%
  filter(Df1$category != " ", Df1$raised_amount_usd != " ")
Df1 <- na.omit(Df1)}

# Traduccion ----

{ Df1$category[Df1$category == "Administrative Services"] = "Servicios Administrativos"
Df1$category[Df1$category == "Advertising"] = "Publicidad"
Df1$category[Df1$category == "Agriculture and Farming"] = "Agricultura y Ganaderia"
Df1$category[Df1$category == "Artificial Intelligence"] = "Inteligencia Artificial"
Df1$category[Df1$category == "Biotechnology"] = "Biotecnologia"
Df1$category[Df1$category == "Clothing and Apparel"] = "Ropa e Indumentaria"
Df1$category[Df1$category == "Commerce and Shopping"] = "Industria Comercial"
Df1$category[Df1$category == "Community and Lifestyle"] = "Comunidad y Estilo de Vida"
Df1$category[Df1$category == "Consumer Electronics"] = "Electronica"
Df1$category[Df1$category == "Consumer Goods"] = "Bienes de Consumo"
Df1$category[Df1$category == "Content and Publishing"] = "Media y Publicacion"
Df1$category[Df1$category == "Data and Analytics"] = "Analisis de Datos"
Df1$category[Df1$category == "Design"] = "Diseño"
Df1$category[Df1$category == "Education"] = "Educacion"
Df1$category[Df1$category == "Energy"] = "Energia"
Df1$category[Df1$category == "Events"] = "Eventos"
Df1$category[Df1$category == "Financial Services"] = "Servicios Financieros"
Df1$category[Df1$category == "Food and Beverage"] = "Alimentos y Bebidas"
Df1$category[Df1$category == "Gaming"] = "Videojuegos"
Df1$category[Df1$category == "Government and Military"] = "Gobierno y Milicia"
Df1$category[Df1$category == "Health Care"] = "Cuidados de la Salud"
Df1$category[Df1$category == "Information Technology"] = "Tecnologias de la Información"
Df1$category[Df1$category == "Lending and Investments"] = "Prestamos e Inversiones"
Df1$category[Df1$category == "Manufacturing"] = "Fabricación"
Df1$category[Df1$category == "Media and Entertainment"] = "Media y Entretenimiento"
Df1$category[Df1$category == "Messaging and Telecommunications"] = "Telecomunicaciones"
Df1$category[Df1$category == "Mobile"] = "Movil"
Df1$category[Df1$category == "Music and Audio"] = "Musica"
Df1$category[Df1$category == "Natural Resources"] = "Recursos Naturales"
Df1$category[Df1$category == "Navigation and Mapping"] = "Navegacion"
Df1$category[Df1$category == "Payments"] = "Sistemas de Pago"
Df1$category[Df1$category == "Platforms"] = "Plataformas"
Df1$category[Df1$category == "Privacy and Security"] = "Seguridad y Privacidad"
Df1$category[Df1$category == "Professional Services"] = "Servicios Personales"
Df1$category[Df1$category == "Real Estate"] = "Bienes Raices"
Df1$category[Df1$category == "Sales and Marketing"] = "Ventas y Marketing"
Df1$category[Df1$category == "Science and Engineering"] = "Ciencia e Ingenieria"
Df1$category[Df1$category == "Sports"] = "Deportes"
Df1$category[Df1$category == "Sustainability"] = "Sustentabilidad"
Df1$category[Df1$category == "Transportation"] = "Transporte"
Df1$category[Df1$category == "Travel and Tourism"] = "Viaje y Turismo"}

# Grafico 1 ----

Df2 <- Df1 %>%
  group_by(category) %>%
  summarise(sum(raised_amount_usd))

Df2 %>%
  arrange(`sum(raised_amount_usd)`) %>%
  mutate(category = factor(category, levels = category)) %>%
  ggplot(aes(x = category, y = `sum(raised_amount_usd)`)) +
  geom_segment(aes(x = category, xend = category, y = `sum(raised_amount_usd)`, yend = 0), size = 1.6, color = "turquoise2") +
  theme_light() +
  geom_point( size = 3.5, color="turquoise3") +
  geom_point( size = 1.5, color="white") +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title = "Top Categorias de Startups hasta la Actualidad", 
       caption = "FUENTE: Base de Datos Crunchbase 2020",
       subtitle = "Total de Inversiones a Startups por Categoria entre 1915 - 2020") +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(250000000000, 500000000000, 750000000000)) +
  ylab("Inversiones en USD") +
  xlab("")

# Grafico 2 ----

Df3 <- Df1 %>%
  group_by(category, year) %>%
  summarise(sum(raised_amount_usd))
Df3 <- Df3 %>%
  filter(Df3$year >= 1980)

ggplot(data = Df3, aes(x = category, y = year, size = `sum(raised_amount_usd)`)) +
  geom_point(alpha=0.04) +
  theme (text = element_text(size=10)) +
  coord_flip() +
  theme_light() +
  ylab("Año") +
  xlab("Categoria") +
  labs(title = paste("Inversiones a Startups entre 1980 - 2020"), 
       caption="FUENTE: Base de Datos Crunchbase 2020") +
  scale_size_continuous(range=c(1, 23), labels = scales::dollar_format(), name = "Investment in USD") +
  scale_color_continuous(guide = FALSE, labels = scales::dollar_format())
