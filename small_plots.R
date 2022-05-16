library(tidyverse)
load(file = "Dados/goog200.rda")



### SÉRIE HETEROCEDÁSTICA -------------------------------
## Isto é, uma série onde a variância não é constante
## ao longo da série

heterocedastico <- ts(
  datasets::AirPassengers,
  start = c(1949,1), frequency = 12
)

plot.ts(heterocedastico)


heterocedastico <- datasets::AirPassengers |> 
  as_tibble() |> 
  rename("y" = x) |> 
  mutate(
    y = as.double(y),
    Ano = rep(1949:1960, each = 12),
    Mês = rep(1:12, times = 12),
    Data = lubridate::make_date(year = Ano, month = Mês)
  )


pl <- heterocedastico |> 
  ggplot() +
  geom_line(
    aes(x = Data, y = y),
    size = 0.8
  ) +
  theme_void()


ragg::agg_png("Figuras/icon-heterocedastico.png", width = 600, height = 280, res = 120)
print(pl)
dev.off()




### SÉRIE ESTACIONÁRIA ----------------------------
##

usd <- read_csv("Dados/USDC-USD.csv")
usd <- usd[800:nrow(usd), ]

pl <- usd |> 
  rename("Data" = "Date") |> 
  ggplot() +
  geom_line(
    aes(x = Data, y = Close)
  ) +
  theme_void()


ragg::agg_png("Figuras/icon-stationarity.png", width = 600, height = 280, res = 120)
print(pl)
dev.off()






### SÉRIE COM SAZONALIDADE ------------------------
##

set.seed(10)
base <- rnorm(70, mean = 500, sd = 50)
fatores <- c(1, 0.9, 0.82, 0.68, 0.61, 0.55, 0.2)
fatores <- rep(fatores, times =  10)
base <- base * fatores

serie_sazonal <- tibble(
  Data = as.Date("2022-02-01") + 0:69,
  y = base
)


pl <- serie_sazonal |> 
  ggplot() +
  geom_line(
    aes(x = Data, y = y), 
    size = 0.8
  ) +
  scale_y_continuous(limits = c(-100, 700)) +
  theme_void()


ragg::agg_png("Figuras/icon-sazonal-series.png", width = 600, height = 280, res = 120)
print(pl)
dev.off()





### SÉRIE QUE POSSUI UMA QUEBRA ----------------------------
##
## https://cran.r-project.org/web/packages/fpp2/fpp2.pdf
##

serie <- tibble(
  Data = as.Date("2013-02-25") + seq_along(goog200) - 1,
  y = as.double(goog200)
)

pl <- serie |> 
  ggplot() +
  geom_line(
    aes(x = Data, y = y), 
    size = 0.8
  ) +
  theme_void()


ragg::agg_png("Figuras/icon-series-with-break.png", width = 600, height = 280, res = 120)
print(pl)
dev.off()





### GRÁFICOS FAC -------------------
##

queda_lenta <- acf(goog200)$acf
queda_lenta <- as_tibble(queda_lenta)
colnames(queda_lenta) <- "y"
queda_lenta$x <- seq_along(queda_lenta$y)

pl <- queda_lenta |> 
  ggplot() +
  geom_col(
    aes(x = x, y = y),
    fill = "#222222",
    width = 0.5
  ) +
  scale_y_continuous(limits = c(0, 1.2)) +
  theme_void()


ragg::agg_png("Figuras/icon-acf-slow-fall.png", width = 600, height = 280, res = 120)
print(pl)
dev.off()





set.seed(10)
serie <- arima.sim(list(order = c(2, 0, 0), ar = c(0.7, -0.49)), n = 120)
queda_alternada <- acf(serie)$acf
queda_alternada <- as_tibble(queda_alternada)
colnames(queda_alternada) <- "y"
queda_alternada$x <- seq_along(queda_alternada$y)

pl <- queda_alternada |> 
  ggplot() +
  geom_col(
    aes(x = x, y = y),
    fill = "#222222",
    width = 0.5
  ) +
  scale_y_continuous(limits = c(-0.3, 1.1)) +
  theme_void()


ragg::agg_png("Figuras/icon-acf-alternate.png", width = 600, height = 280, res = 120)
print(pl)
dev.off()
