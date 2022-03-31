library(forecast)
library(urca)
library(tseries)
library(ggplot2)


### Importando os dados
load(file = "Dados/goog200.rda")


### Visualizando a série pela primeira vez
autoplot(goog200)



### A primeira vista, a série parece ser
### uma série temporal NÃO ESTACIONÁRIA.

### Podemos agora, analisar os gráficos FAC e FACP
### para identificar qual o processo gerador que mais se encaixa
### nos padrões dessa série.

### O gráfico FAC da série começa alto, e decai bem lentamente.
### Isso é um indício forte de um processo auto-regressivo.
forecast::Acf(goog200, main = "FAC", ylab = NA, lag.max = 15)


### O gráfico FACP da série começa alto na primeira lag, porém
### ele cai bruscamente, perdendo a sua signifância logo na 
### segunda lag. Esse padrão também se encaixa muito em um
### processo autoregressivo.
forecast::Pacf(goog200, main = "FACP", ylab = NA)


# png("Figuras/fac-ar.png", width = 2400, height = 1200, res = 300, type = "cairo")
# forecast::Acf(goog200, main = "FAC", ylab = NA, lag.max = 15)
# dev.off()
# 
# png("Figuras/facp-ar.png", width = 2400, height = 1200, res = 300, type = "cairo")
# forecast::Pacf(goog200, main = "FACP", ylab = NA)
# dev.off()
