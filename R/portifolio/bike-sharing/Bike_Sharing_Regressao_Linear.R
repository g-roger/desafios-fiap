

install.packages("tidyverse")
library(tidyverse)
library(tidyr) 

library(readr)
arquivo <- read.csv("~/fiap/desafios-fiap/Estatísticas/trab4/Bike_Sharing.csv")
View(arquivo)


# nao mostrar os resultados na notacao cientifica
options(scipen = 999)


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


# seleção das variáveis quantitativas
dadosquant=subset(arquivo,select=c(cnt,temp,atemp,
                                   hum,windspeed))

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
hist(cnt, xlab="Total de arquivos alugadas", ylab="Freq. Absoluta", main="Histograma do Total")
boxplot(cnt,ylab="Total de arquivos alugadas", main="Box Plot Total")



boxplot(windspeed, ylab="windspeed", main="Box Plot do windspeed")
boxplot(temp, ylab="temp", main="Box Plot da temp")
boxplot(atemp, ylab="atemp", main="Box Plot da atemp")
boxplot(hum, ylab="hum", main="Box Plot da hum")


# Box Plot cnt versys varíaveis qualitativas
boxplot(arquivo$cnt~arquivo$season)
boxplot(arquivo$cnt~yr)
boxplot(arquivo$cnt~arquivo$mnth)
boxplot(arquivo$cnt~holiday)
boxplot(arquivo$cnt~weekday)
boxplot(arquivo$cnt~workingday)
boxplot(arquivo$cnt~weathersit)


# Mateiz de correlação

# selecao das variaveis quantitativas
dadosquant=subset(arquivo,select=c(cnt,temp,atemp,
                                   hum,windspeed))


mc = cor(dadosquant);mc

install.packages("corrplot")
library(corrplot)

corrplot(mc)
corrplot(mc, type="upper", method="number")

install.packages("Hmisc")
library(Hmisc)

mcorr = rcorr(as.matrix(dadosquant));mcorr
mcorr$r # correlação
mcorr$P # p-valor
mcorr$n # número de observações


corrplot(mcorr$r,p.mat=mcorr$P, sig.level=0.05, method="number", type="upper")

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(dadosquant,histogram=TRUE)







# Pacote fdth transforma variável quantitativa em qualitativa
install.packages("RcmdrMisc")
require("RcmdrMisc")

arquivo$fx_cnt <-binVariable(arquivo$cnt, bins = 4, method = c("natural"),labels=FALSE)
table(arquivo$fx_cnt)



# analise bivariada

boxplot(arquivo$cnt~arquivo$holiday)
tab1 = table(arquivo$fx_cnt, arquivo$holiday)
tab1




boxplot(arquivo$cnt~arquivo$mnth)
tab2 = table(arquivo$fx_cnt, arquivo$mnth)
tab2
chq2 = chisq.test(tab2)
chq2

boxplot(arquivo$cnt~arquivo$weekday)
tab3 = table(arquivo$fx_cnt, arquivo$weekday)
tab3
chq3 = chisq.test(tab3)
chq3

boxplot(arquivo$cnt~arquivo$season)
tab4 = table(arquivo$fx_cnt, arquivo$season)
tab4
chq4 = chisq.test(tab4)
chq4





#Pre-processamento dos dados
# Apaga a coluna 
arquivo$instant = NULL
arquivo$dteday = NULL
arquivo$casual = NULL
arquivo$registered = NULL
arquivo$fx_cnt = NULL


# Regresslão linear simples
modelo <- lm(cnt ~ temp,data=arquivo)
summary(modelo)

# Regresslão linear múltipla
modelo <- lm(cnt ~ temp +atemp -1 ,data=arquivo)
summary(modelo)

arquivo$temp_atemp = temp*atemp

# Regresslão linear múltipla 2
modelo <- lm(cnt ~ temp +atemp + temp_atemp ,data=arquivo)
summary(modelo)

arquivo$temp_atemp = NULL


#Bike$season1 =season
#categoria de referencia
#Bike$season1 =ifelse(season==1,"1","0")
arquivo$season2 =arquivo$season
arquivo$season2 =ifelse(arquivo$season==2,"1","0")
arquivo$season3 =arquivo$season
arquivo$season3 =ifelse(arquivo$season==3,"1","0")
arquivo$season4 =arquivo$season
arquivo$season4 =ifelse(arquivo$season==4,"1","0")

# Criar variaveis dicotomicas
#arquivo$season1 =season
#categoria de referencia
#arquivo$season1 =ifelse(season==1,"1","0")
#arquivo$season2 =season
#arquivo$season2 =ifelse(season==2,"1","0")
#arquivo$season3 =season
#arquivo$season3 =ifelse(season==3,"1","0")
#arquivo$season4 =season
#arquivo$season4 =ifelse(season==4,"1","0")
#categoria de referencia
#arquivo$weathersit1 =weathersit
#arquivo$weathersit1 =ifelse(weathersit==1,"1","0")
arquivo$weathersit2 =weathersit
arquivo$weathersit2 =ifelse(weathersit==2,"1","0")
arquivo$weathersit3 =weathersit
arquivo$weathersit3 =ifelse(weathersit==3,"1","0")


#categoria de referencia
#arquivo$mnth1 =mnth
#arquivo$mnth1 =ifelse(mnth==1,"1","0")
arquivo$mnth2 =mnth
arquivo$mnth2 =ifelse(mnth==2,"1","0")
arquivo$mnth3 =mnth
arquivo$mnth3 =ifelse(mnth==3,"1","0")
arquivo$mnth4 =mnth
arquivo$mnth4 =ifelse(mnth==4,"1","0")
arquivo$mnth5 =mnth
arquivo$mnth5 =ifelse(mnth==5,"1","0")
arquivo$mnth6 =mnth
arquivo$mnth6 =ifelse(mnth==6,"1","0")
arquivo$mnth7 =mnth
arquivo$mnth7 =ifelse(mnth==7,"1","0")
arquivo$mnth8 =mnth
arquivo$mnth8 =ifelse(mnth==8,"1","0")
arquivo$mnth9 =mnth
arquivo$mnth9 =ifelse(mnth==9,"1","0")
arquivo$mnth10 =mnth
arquivo$mnth10 =ifelse(mnth==10,"1","0")
arquivo$mnth11 =mnth
arquivo$mnth11 =ifelse(mnth==11,"1","0")
arquivo$mnth12 =mnth
arquivo$mnth12 =ifelse(mnth==12,"1","0")


# Apaga a coluna 
arquivo$season = NULL
arquivo$mnth = NULL
arquivo$weathersit = NULL
arquivo$weekday = NULL # o ideal era manter



view(arquivo)



# Regressão linear múltipla quanti
modelo <- lm(cnt ~ season2 + season3 + season4,data=arquivo)
summary(modelo)

# Modelo 

#Dividir em duas amostras
set.seed(2021)
train <- sample(nrow(arquivo), 0.7*nrow(arquivo), replace = FALSE)
TrainSet <- arquivo[train,]
ValidSet <- arquivo[-train,]

summary(TrainSet$cnt)
summary(ValidSet$cnt)

attach(TrainSet)

str(TrainSet)

# Regresslão linear múltipla
modelo1 <- lm(cnt ~ . ,data=TrainSet)
summary(modelo1)

# selecionando variáveis por método automático
# direction = c("both", "backward", "forward")
modelo1_stepwise<-step(modelo1,direction="both")
summary(modelo1_stepwise)


TrainSet$Val_pred <- predict(modelo1_stepwise,interval = "prediction", level = 0.95,data=TrainSet) 
TrainSet$residuo  <- resid(modelo1_stepwise)
TrainSet$rp <- rstandard(modelo1_stepwise)

# Erro quadratico medio na amostra de treino
mse <- mean((TrainSet$cnt - TrainSet$Val_pred)^2)
sqrt(mse)


# grafico residuo
plot(predict(modelo1_stepwise),TrainSet$residuo, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


#observa-se SE violacao da suposicao de que os erros aleatarios tem distribuicao Normal

qqnorm(residuals(modelo1_stepwise), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(modelo1_stepwise))

#Teste de Normalidade de Shapiro Wilk

# sE Valor P do teste eh pequeno, rejeita-se a hipotese de normalidade dos residuos e,
# por consequencia, conclui-se que os erros nao sao normalmente distribuidos.

# Teste de hipóteses
# H0: Distribuicao Normal
# H1: Distribuição diferente da Normal

shapiro.test(residuals(modelo1_stepwise))


#Excluir os outliers
TrainSet_1 <-filter(TrainSet,TrainSet$rp>=-2&TrainSet$rp<=2) 

#Pre-processamento dos dados
# Apaga a coluna 
TrainSet_1$Val_pred = NULL
TrainSet_1$residuo = NULL
TrainSet_1$rp = NULL

# Regresslão linear múltipla
modelo2 <- lm(cnt ~ . ,data=TrainSet_1) 
summary(modelo2)

modelo2_stepwise<-step(modelo2,direction="both")
summary(modelo2_stepwise)

TrainSet_1$Val_pred <- predict(modelo2_stepwise,interval = "prediction", level = 0.95,data=TrainSet) 
TrainSet_1$residuo  <- resid(modelo2_stepwise)
TrainSet_1$rp <- rstandard(modelo2_stepwise)

# Erro quadratico medio na amostra de treino
mse_1 <- mean((TrainSet_1$cnt - TrainSet_1$Val_pred)^2)
sqrt(mse_1)


# grafico residuo
plot(predict(modelo2_stepwise),TrainSet_1$residuo, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


#observa-se SE violacao da suposicao de que os erros aleatarios tem distribuicao Normal

qqnorm(residuals(modelo2_stepwise), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(modelo2_stepwise))

#Teste de Normalidade de Shapiro Wilk

# sE Valor P do teste eh pequeno, rejeita-se a hipotese de normalidade dos residuos e,
# por consequencia, conclui-se que os erros nao sao normalmente distribuidos.

# Teste de hipóteses
# H0: Distribuicao Normal
# H1: Distribuição diferente da Normal

shapiro.test(residuals(modelo2_stepwise))



#Excluir os outliers
TrainSet_2 <-filter(TrainSet_1,TrainSet_1$rp>=-2&TrainSet_1$rp<=2) 


#Pre-processamento dos dados
# Apaga a coluna 
TrainSet_2$Val_pred = NULL
TrainSet_2$residuo = NULL
TrainSet_2$rp = NULL

# Regresslão linear múltipla
modelo3 <- lm(cnt ~ . ,data=TrainSet_2)
summary(modelo3)

modelo3_stepwise<-step(modelo3,direction="both")
summary(modelo3_stepwise)

TrainSet_2$Val_pred <- predict(modelo3_stepwise,interval = "prediction", level = 0.95,data=TrainSet) 
TrainSet_2$residuo  <- resid(modelo3_stepwise)
TrainSet_2$rp <- rstandard(modelo3_stepwise)

# Erro quadratico medio na amostra de treino
mse_2 <- mean((TrainSet_2$cnt - TrainSet_2$Val_pred)^2)
sqrt(mse_2)


# grafico residuo
plot(predict(modelo3_stepwise),TrainSet_2$residuo, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


#observa-se SE violacao da suposicao de que os erros aleatarios tem distribuicao Normal

qqnorm(residuals(modelo2_stepwise), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(modelo2_stepwise))

#Teste de Normalidade de Shapiro Wilk

# sE Valor P do teste eh pequeno, rejeita-se a hipotese de normalidade dos residuos e,
# por consequencia, conclui-se que os erros nao sao normalmente distribuidos.

# Teste de hipóteses
# H0: Distribuicao Normal
# H1: Distribuição diferente da NormaL

shapiro.test(residuals(modelo2_stepwise))




# Amostra de validacao

cnt_pred <- predict(modelo3,interval = "prediction", level = 0.95,
                    newdata = ValidSet, se.fit = T) 

cnt_pred1 <-cnt_pred$fit
ValidSet_pred=cbind(ValidSet,cnt_pred1)

# Residuo na amostra de validacao
residuo_valid <- ValidSet_pred$cnt - ValidSet_pred$fit
hist(residuo_valid)
qqnorm(residuo_valid, ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuo_valid)

x = 0.099 + 1.308 - 0.105 + 1.067 + 0.580 + 0.455 - 0.264
p1 = exp(x)/(1+exp(x))

y = 0.099 - 1.276 - 0.105 - 1.069 - 0.264 - 0.413 + 0.581
p2 = exp(y)/(1+exp(y))

z = 0.099 - 0.611 + 0.262 + 0.371 + 0.580 + 0.080 + 0.401
p3 = exp(z)/(1+exp(z))

# Erro quadratico medio na amostra de validacao
mse2 <- mean((ValidSet_pred$cnt - ValidSet_pred$fit)^2)
sqrt(mse2)

kk = 0.099 - 1.276 -0.718 - 0.261 -0.718 -1.069 - 0.413
propensao = exp(kk)/1+exp(kk)

c = 0.099 + 1.308+0.580+0.262+0.581+1.067+0.455
propensao = exp(c)/(1+exp(c) )

propensao = ex

exp(c)
?exp
