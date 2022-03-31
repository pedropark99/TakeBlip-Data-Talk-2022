library(tidyverse)
library(readxl)
library(lubridate)
library(forecast)

dados <- read_excel("Dados/serie-pib-brasil.xlsx")
dados$PIB <- as.double(dados$PIB)
dados$Ano <- as.integer(dados$Ano)

dados <- dados %>% 
  mutate(PIB_em_milhoes = PIB/1e6)

fonte <- "Fonte: Banco Mundial Data."

serie <- ts(dados$PIB_em_milhoes, start = 1961, frequency = 1)

plot.ts(serie)



pl <- autoplot(serie) +
  labs(
    y = "",
    x = "",
    title = "Evolução do PIB brasileiro (em US$ milhões)"
  )


png("Figuras/pib.png", width = 2000, height = 1000, res = 300, type = "cairo")
print(pl)
dev.off()





### Preço diário das ações da Google

load(file = "Dados/goog200.rda")

pl <- autoplot(goog200) +
  labs(
    y = "",
    x = "",
    title = "Evolução dos preços diários das ações do Google (em US$)"
  )


png("Figuras/preco-acoes-google.png", width = 2000, height = 1000, res = 300, type = "cairo")
print(pl)
dev.off()




pl <- autoplot(diff(goog200)) +
  labs(
    y = "",
    x = "",
    title = "Série dos preços diários das ações do Google, em primeira diferença"
  )


png("Figuras/diff-preco-acoes-google.png", width = 2000, height = 1000, res = 300, type = "cairo")
print(pl)
dev.off()


### Um exemplo de série temporal estacionária:
set.seed(10)
x <- arima.sim(list(1, 1, 0), n = 120)


pl <- ggplot(data = data.frame(x = seq_along(as.double(x)), y = as.double(x))) +
  geom_hline(
    size = 1,
    color = "steelblue",
    yintercept = mean(as.double(x))
  ) +
  geom_line(
    aes(x = x, y = y)
  ) +
  labs(title = "Exemplo de série estacionária") +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color = "#222222", size = 12),
    plot.title = element_text(color = "#222222", size = 17, face = "bold", margin = margin(b = 20)),
    plot.title.position = "plot"
  )

png("Figuras/exemplo-serie-estacionaria.png", width = 2200, height = 1300, res = 300, type = "cairo")
print(pl)
dev.off()


### Podemos utilizar um teste ADF (Augmented Dickey-Fuller) para testarmos
### se essa série é de fato estacionária.

### O pacote `urca` nos oferece a função `ur.df()` que é capaz de calcular
### tal teste. Perceba pelos resultados do teste calculado abaixo, que a série `x` é
### de fato uma série estacionária.
urca::summary(urca::ur.df(x, type = "none"))
