library(tidyverse)
library(readxl)
library(lubridate)

dados <- read_excel("serie-pib-brasil.xlsx")
dados$PIB <- as.double(dados$PIB)
dados$Ano <- as.integer(dados$Ano)

fonte <- "Fonte: Banco Mundial Data."


dados %>% 
  arrange(Ano) %>% 
  ggplot() +
  geom_line(
    aes(x = Ano, y = PIB)
  ) +
  scale_y_continuous(limits = c(0, 1e15))
