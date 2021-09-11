

install.packages("tidyverse")
library(tidyverse)
library(tidyr) 

rm(list=ls(all=TRUE))
arquivo <- read.csv("~/fiap/desafios-fiap/Estatísticas/trab3/Bike_Sharing.csv")
View(arquivo)

#Attach Set Of R Objects To Search Path
attach(arquivo)


# Tipo de variáveis
str(arquivo)

# Medidas resumo
summary(arquivo)


# Mudar o formato da variável numérica para texto
arquivo$season<-as.factor(arquivo$season)
arquivo$yr<-as.factor(arquivo$yr)
arquivo$holiday<-as.factor(arquivo$holiday)
arquivo$weekday<-as.factor(arquivo$weekday)
arquivo$workingday<-as.factor(arquivo$workingday)
arquivo$weathersit<-as.factor(arquivo$weathersit)
arquivo$mnth<-as.factor(arquivo$mnth)



# Tipo de variáveis
str(arquivo)

attach(arquivo)

# Medidas resumo
summary(arquivo)


# Número de observações e variáveis
dim(arquivo)

# Criar o dataframe com todas as variáveis quantitativa
dadosquant=subset(arquivo,select=c(temp,atemp, hum, windspeed, cnt))

# Medidas resumo
summary(dadosquant)

 # Tabela de frequência
fa_holiday= table(holiday) 
fa_holiday
fr_holiday=fa_holiday/sum(fa_holiday)
fr_holiday
dist_holiday=cbind(fa_holiday,fr_holiday) 
dist_holiday

fa_weekday= table(weekday) 
fa_weekday
fr_weekday=fa_weekday/sum(fa_weekday)
fr_weekday
dist_weekday=cbind(fa_weekday,fr_weekday) 
dist_weekday

fa_workingday= table(workingday) 
fa_workingday
fr_workingday=fa_workingday/sum(fa_workingday)
fr_workingday
dist_workingday=cbind(fa_workingday,fr_workingday) 
dist_workingday

fa_weathersit= table(weathersit) 
fa_weathersit
fr_weathersit=fa_weathersit/sum(fa_weathersit)
fr_weathersit
dist_weathersit=cbind(fa_weathersit,fr_weathersit) 
dist_weathersit

fa_season= table(season) 
fa_season
fr_season=fa_season/sum(fa_season)
fr_season
dist_season=cbind(fa_season,fr_season) 
dist_season

                  
# Gráficos        
hist(cnt, xlab="cnt", ylab="Freq abs", main="Histograma do cnt")
boxplot(cnt, ylab="cnt", main="Box Plot do cnt")

#Multiple Plot
par(mfrow=c(1,2))
par(mar=c(10,4,8,2))
hist(cnt, xlab="Total de bikes alugadas", ylab="Freq. Absoluta", main="Histograma do Total")
boxplot(cnt,ylab="Total de bikes alugadas", main="Box Plot Total")


boxplot(windspeed, ylab="windspeed", main="Box Plot do windspeed")
boxplot(temp, ylab="temp", main="Box Plot da temp")
boxplot(atemp, ylab="atemp", main="Box Plot da atemp")
boxplot(hum, ylab="hum", main="Box Plot da hum")


# Box Plot cnt versys varíaveis qualitativas
boxplot(arquivo$cnt~season)
boxplot(arquivo$cnt~yr)
boxplot(arquivo$cnt~mnth)
boxplot(arquivo$cnt~holiday)
boxplot(arquivo$cnt~weekday)
boxplot(arquivo$cnt~workingday)
boxplot(arquivo$cnt~weathersit)

# Correlação de Pearson
mc = cor(dadosquant);mc

# Gráfico de dispersão
par(mfrow=c(2,2))
par(mar=c(3,4,8,2))
plot(arquivo$cnt ~arquivo$temp)
plot(arquivo$cnt ~arquivo$atemp)
plot(arquivo$cnt ~arquivo$hum)
plot(arquivo$cnt ~arquivo$windspeed)


