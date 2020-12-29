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
library(RCurl)

Data_Limpia <- getURL("https://raw.githubusercontent.com/IgnacioZQ/CademiLabs/main/crunch2013/crunchbase-investments.csv")

Data_Limpia <- read.csv(text = Data_Limpia)

Mapas <- st_read("Data_Limpia")

ggplot(data = Data_Limpia) +
  geom_sf()
