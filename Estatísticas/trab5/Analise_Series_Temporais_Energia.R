# Series Temporais

#pacotes de séries temporais
install.packages("forecast")
install.packages("ggplot2")
install.packages("urca")
install.packages("lmtest")
install.packages("seasonal")
install.packages("seasonalview")

library("forecast")
library(ggplot2)

# importar a serie temporal
dados3 = read.csv(" ", sep=";",header = T)

#formatar a série temporal e informar o período de tempo (frequency)
serie_energia = ts(dados3[3],start=c(2007,1), end=c(2008,12), frequency=12)
length(serie_energia)
print(serie_energia)
autoplot(serie_energia)

#decomposição da serie
dec =   decompose(serie_energia)
autoplot(dec)

#análise da tendencia
autoplot(dec$trend)

#análise sazonal
ggseasonplot(serie_energia)  




#Modelo de alisamento exponencial 
modelo_ets = ets(serie_energia,model="ZZZ")
print(modelo_ets)
 
previsao_ets = forecast(modelo_ets, h=12)
print(previsao_ets)
autoplot(previsao_ets)



# Modelo alisamento exponencial de Holter Winter
?hw
modelo_hw = hw(serie_energia, seasonal=c("additive"),h=12)
modelo_hw$model
autoplot(modelo_hw)
previsao_hw = forecast(modelo_hw, h=12)
print(previsao_hw)
autoplot(previsao_hw)

# Modelos Arima
tsdisplay(serie_energia)

modelo_arima = auto.arima(serie_energia, trace = T,stepwise = F, approximation = F )
print(modelo_arima)

checkresiduals(modelo_arima)

shapiro.test(modelo_arima$residuals)

#previsao para 2 anos
previsao_arima = forecast(modelo_arima,h=12)
print(previsao_arima)
autoplot(previsao_arima)

plot(previsao_arima)
lines(previsao_ets$mean, col="red")
lines(previsao_hw $mean, col="green")
#legenda
legend("topright",legend=c("Arima","ETS","HW"), col = c("blue","red","green"), lty=1:2, cex=0.5,)

accuracy(previsao_arima)
accuracy(previsao_ets)
accuracy(previsao_hw)
