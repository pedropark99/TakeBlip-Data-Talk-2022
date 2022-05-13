library(tidyverse)
load(file = "Dados/goog200.rda")


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

