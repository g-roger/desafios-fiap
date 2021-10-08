# S?ries Temporais no R

#pacotes de s?ries temporais
install.packages("forecast")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("seasonal")
install.packages("seasonalview")

library(ggplot2)
library(forecast)
library(seasonal)
library(seasonalview)


# importar uma s?rie temporal
?ts



#########################################################

#Exercicio 1: Importar as s?ries temporais no RStudio
#a) Faturamento.csv

# importar a serie temporal
Faturamento = read.csv("  ", sep=";",header = T)

#formatar a serie temporal e informar o periodo de tempo (frequency)
serie_fat = ts(Faturamento[3],start=c(2011,1), end=c(2013,12), frequency=12)
length(serie_fat)
print(serie_fat)



# formato do dado ? uma s?rie temporal
class(serie)

# grafico da serie temporal
autoplot(serie_fat)

#b) Consumo_Energia_Agricultura.csv

library(readr)
Consumo_Energia_Agricultura <- read_csv("Consumo_Energia_Agricultura.csv")


#formatar a s?rie temporal e informar o per?odo de tempo (frequency)
serie_energia = ts(Consumo_Energia_Agricultura[3],start=c(2007,1), end=c(2008,12), frequency=12)
length(serie_energia)
print(serie_energia)

# formato do dado ? uma s?rie temporal
class(serie_energia)

# grafico da serie temporal
autoplot(serie_energia)


#c) Bike_Sharing_day.csv

library(readr)
Bike_Sharing_day <- read_csv("Bike_Sharing_day.csv")

# Excluir a data 2012-02-29 correspondente a linha 425 
Bike_Sharing_day<-Bike_Sharing_day[(-425),] 

serie_Bike = ts(Bike_Sharing_day[16],start=2011, frequency=365)
length(serie_Bike)
print(serie_Bike)



# formato do dado ? uma s?rie temporal
class(serie)


# grafico da serie temporal
autoplot(serie_Bike)


