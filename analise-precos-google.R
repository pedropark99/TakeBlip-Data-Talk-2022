library(forecast)
library(urca)
library(tseries)

load(file = "Dados/goog200.rda")


autoplot(goog200)


summary(ur.df(goog200, "none"))



dgoog <- diff(goog200)


### Teste de inferência para a estacionariedade da série 
summary(ur.df(dgoog, type = "none"))


forecast::Acf(goog200)

forecast::Pacf(goog200)
