library(tidyverse)
library(forecast)
library(urca)


### Série do número diário de usuários ativos (DAUs) -----------------------------

# Número de observações
n <- 120
# Componente aleatório
set.seed(50)
noise <- rnorm(n, mean = 200, sd = 100)
# Componente de tendência
trend <- 15.67
# Componente estocástico
t <- 1:120
stochastic_component <- (10.32 * t) + (0.287 * (t ^ 2))

### A série temporal deste exemplo:
serie <- ts(trend + stochastic_component + noise)
plot.ts(serie)




### Análise das funções autoregressivas (FAC e FACP):
# Gráfico da FAC apresenta uma queda lenta e prolongada.
# Isso indica que a série se encaixa em um processo
# autoregressivo.
acf(serie)

# Gráfico da FACP apresenta um grande pico na primeira lag
# da série, porém, seu valor cai subitamente já a partir da
# segunda lag. Isso é uma característica clássica de um
# processo autoregressivo de ordem 1, isto é, um modelo AR(1).
pacf(serie)



### Teste de Raiz Unitária (Dickey-Fuller):
# Estatística de -1,2335 não foi capaz de rejeitar a hipótese
# nula do teste;
summary(urca::ur.df(serie, type = "trend"))


# Em primeira diferença, a série produz estatísticas que rejeitam
# a hipótese nula nas três especificações do teste; Logo,
# a série é estacionária em primeira diferença
serie_df <- diff(serie)
summary(urca::ur.df(serie_df, type = "none"))
summary(urca::ur.df(serie_df, type = "drift"))
summary(urca::ur.df(serie_df, type = "trend"))




### Cálculo dos modelos candidatos:
# Modelo 1:
modelo <- forecast::Arima(serie, order = c(1, 1, 0))
summary(modelo)

# Modelo 2:
modelo <- forecast::Arima(serie, order = c(1, 1, 0), include.drift = TRUE)
summary(modelo)

# Modelo 3:
modelo <- forecast::Arima(serie, order = c(0, 1, 1))
summary(modelo)

# Modelo 4:
modelo <- forecast::Arima(serie, order = c(0, 1, 1), include.drift = TRUE)
summary(modelo)



### O modelo 4 é o que apresenta o meno AIC.
previsoes <- forecast::forecast(
  modelo,
  h = 20   # Número de períodos a frente para serem previstos pelo modelo
)






pl <- autoplot(previsoes) +
  labs(
    title = "Número diário de usuários ativos (DAUs)",
    x = "Dias"
  ) +
  theme(
    text = element_text(family = "Segoe UI", size = 13, color = "#222222"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_blank()
  )


ragg::agg_png("Figuras/serie-aletoria.png", width = 2000, height = 1000, res = 300)
print(pl)
dev.off()














### Série queda nas vendas -------------------------------------

# Número de observações
n <- 120
# Componente aleatório
set.seed(50)
noise <- rnorm(n, mean = 100, sd = 60)
# Componente estocástico
t <- 1:120
stochastic_component <- (-2.45 * t) + (0.02 * (t ^ 2))

### A série temporal deste exemplo:
serie <- ts(stochastic_component + noise + 1500)
plot.ts(serie)




pl <- tibble(x = seq_len(n), y = serie) %>% 
  ggplot() +
  geom_line(
    aes(x = x, y = y)
  ) +
  scale_y_continuous(limits = c(0, max(serie) + 200)) +
  labs(
    title = "Número diário de doces vendidos",
    x = "Dias",
    subtitle = "Um contato inteligente para venda de bolos, doces e salgados"
  ) +
  theme(
    text = element_text(family = "Segoe UI", size = 13, color = "#222222"),
    plot.subtitle = element_text(family = "Roboto Condensed"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_blank()
  )




ragg::agg_png("Figuras/vendas-estagnada.png", width = 2000, height = 1000, res = 300)
print(pl)
dev.off()







### Série aumento expressivo nas mensagens -------------------------------------

set.seed(10)
base <- rnorm(140, mean = 5000, sd = 1500)
fatores <- c(1, 0.9, 0.82, 0.68, 0.61, 0.55, 0.2)
fatores <- rep(fatores, times =  20)
base <- base * fatores


### Adicionando uma quebra à série temporal
base[80:119] <- base[80:119] + c(
  5200, 4000, 5300, 3600, 3800, 4000, 3800, 3800,
  4100, 4200, 4300, 4600, 5000, 4200, 5300, 5600,
  3100, 3000, 3400, 3700, 6200, 6300, 6400, 6000,
  7000, 6300, 6000, 6500, 7000, 7200, 6300, 5100,
  5600, 5500, 6200, 7000, 6800, 6000, 6100, 6300
) + 2000

serie <- base[1:119]


pl <- tibble(x = seq_along(serie), y = serie) %>% 
  ggplot() +
  geom_line(
    aes(x = x, y = y)
  ) +
  labs(
    title = "Número de mensagens enviadas pelo contato inteligente",
    x = "Dias",
    subtitle = "Porque o número de mensagens subiu drasticamente se o número de\nusuários permaneceu o mesmo?"
  ) +
  theme(
    text = element_text(family = "Segoe UI", size = 13, color = "#222222"),
    plot.subtitle = element_text(family = "Roboto Condensed", margin = margin(b = 20)),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_blank()
  )


ragg::agg_png("Figuras/mensagens-aumento-drastico.png", width = 2200, height = 1400, res = 300)
print(pl)
dev.off()

