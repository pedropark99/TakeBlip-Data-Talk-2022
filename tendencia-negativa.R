library(ggplot2)
library(ragg)
library(tibble)

set.seed(10)
random <- rnorm(52, 70, 120)

x <- 1:52
y <- -(x^2) + (30 * x) + 1500
y <- y + random

serie <- ts(y, start = c(2016, 4), frequency = 12)
plot.ts(serie)



pl <- autoplot(serie) +
  labs(
    title = "Número mensal de propostas aceitas",
    subtitle = "Maquininha para cartões",
    x = "Tempo"
  ) +
  theme(
    text = element_text(family = "Segoe UI", size = 13, color = "#222222"),
    plot.subtitle = element_text(family = "Roboto Condensed"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_blank()
  )


agg_png("Figuras/serie-tendencia-negativa.png", width = 2000, height = 1000, res = 300)
print(pl)
dev.off()







#### Quedas muito abruptas ------------------------------------

set.seed(250)
serie <- rnorm(150, 50, 2)
choques <- rep(0, times = 150)
choques[125:150] <- choques[125:150] - rnorm(26, 42)
serie <- serie + choques
serie <- ts(serie)


pl <- autoplot(serie) +
  labs(
    title = "Número diário de propostas aceitas",
    subtitle = "Porque o número de propostas aceitas cai subtamente?",
    x = "Dias"
  ) +
  theme(
    text = element_text(family = "Segoe UI", size = 13, color = "#222222"),
    plot.subtitle = element_text(family = "Roboto Condensed"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_blank()
  ) 


agg_png("Figuras/serie-queda-abrupta.png", width = 2000, height = 1000, res = 300)
print(pl)
dev.off()


