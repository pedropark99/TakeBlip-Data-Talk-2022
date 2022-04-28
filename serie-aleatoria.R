library(tidyverse)
library(forecast)
library(urca)

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


### Aplicação do modelo:
modelo <- arima(serie, order = c(1, 1, 0))
summary(modelo)




pl <- tibble(x = seq_len(n), y = serie) %>% 
  ggplot() +
  geom_line(
    aes(y = y, x = x)
  ) +
  labs(
    title = "Número diário de usuários ativos (DAUs)"
  ) +
  theme(
    plot.title.position = "plot",
    axis.title = element_blank()
  )


ragg::agg_png("Figuras/serie-aletoria.png", width = 2000, height = 1000, res = 300)
print(pl)
dev.off()
