# Series Temporais

#pacotes de series temporais
install.packages("forecast")
install.packages("ggplot2")
install.packages("seasonal")
install.packages("seasonalview")

library("forecast")
library(ggplot2)

# importar a serie temporal
Faturamento = read.csv("  ", sep=";",header = T)

#formatar a serie temporal e informar o periodo de tempo (frequency)
serie_fat = ts(Faturamento[3],start=c(2011,1), end=c(2013,12), frequency=12)
length(serie_fat)
print(serie_fat)
autoplot(serie_fat)

#decomposição da série
dec =   decompose(serie_fat)
autoplot(dec)

#análise da tendência
autoplot(dec$trend)

#análise sazonal
ggseasonplot(serie_fat)  


#Modelo de alisamento exponencial 
modelo_ets = ets(serie_fat)
print(modelo_ets)
 
previsao_ets = forecast(modelo_ets, h=12)
print(previsao_ets)
autoplot(previsao_ets)


# Modelos Arima
tsdisplay(serie_fat)

modelo_arima = auto.arima(serie_fat, trace = T,stepwise = F, approximation = F )
print(modelo_arima)

checkresiduals(modelo_arima)

# Teste de normalidade dos residuos
# Residuo tem distribuicao Normal se p-valor >= 0.05
# Caso contrário o modelo não está adequado
shapiro.test(modelo_arima$residuals)

#previsao para 2 anos
previsao_arima = forecast(modelo_arima,h=12)
print(previsao_arima)
autoplot(previsao_arima)

plot(previsao_arima)
lines(previsao_ets$mean, col="red")

accuracy(previsao_arima)
accuracy(previsao_ets)





