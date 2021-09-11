# Instalar os pacotes do R

# Instalar os pacotes para Ciencia de Vendas
install.packages("ggplot2")
install.packages("tidyverse")

#carregar pacotes
library(ggplot2)
library(dbplyr)
library(tidyverse)
require(tidyverse)

#pacotes de séries temporais
install.packages("forecast")

#carregar pacotes
library(forecast)


# Modelo de Regressao Linear Simples

# Importar os dados
library(readr)
Vendas <- read_delim("Vendas_2016a2018.csv",";", escape_double = FALSE, trim_ws = TRUE)
View(Vendas)

# Formato das variaveis
str(Vendas)



attach(Vendas)


#Media e desvio padrao das vendas e budget por ano
group_by(Vendas,ano) %>% summarise(mean(Vendas),sd(Vendas), mean(Budget_Advertising),sd(Budget_Advertising)) 


# Medidas resumo
summary(Vendas)



# Grafico de dispersao
plot (Vendas ~ Budget_Advertising)

# selecao das variaveis quantitativas
Vendasquant=subset(Vendas,select=c(Vendas,Budget_Advertising))

# Matriz de correlacao
mc = cor(Vendasquant);mc

# Modelo de regressao linear

# nao mostrar os resultados na notacao cientifica
options(scipen = 999)

modelo <- lm(Vendas$Vendas ~ Vendas$Budget_Advertising)
summary(modelo)

plot (Vendas$Vendas ~ Vendas$Budget_Advertising)
abline(modelo)

Vendas$predito <- predict(modelo,interval = "prediction", level = 0.95) 
Vendas$residuo <- resid(modelo,interval = "prediction", level = 0.95) 
Vendas$residuop <- rstandard(modelo,interval = "prediction", level = 0.95) 

hist(Vendas$residuo)
hist(Vendas$residuop)

# Grafico 
qqnorm(residuals(modelo), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(modelo))

# Teste de Normalidade
# H0: Distribuição = Normal
# H1: Distribuição <> Normal
# Se p-valor < 0.05 entao rejeito H0
# Se p-valor >= 0.05 entao nao rejeito H0

shapiro.test(residuals(modelo))

media_residuo=(mean(Vendas$residuo));media_residuo
desvio_padrao_residuo =sd(Vendas$residuo) ;desvio_padrao_residuo

# Aplicacao do modelo ajustado

# Importar os Vendas
library(readr)
Budget <- read_delim("Budget_2019.csv", ";", 
                     escape_double = FALSE, trim_ws = TRUE)
View(Budget)

attach(Budget)

summary(budget)

Budget$ano=2019

# aplicacao do modelo de regressao ajustado
budget$Vendas = A + B *Budget_Advertising

# Projecao de vendas (R$) para 2019
vendas_2019_regressao = sum(budget$Vendas)

plot(Budget$Vendas ~ Budget_Advertising)

# selecao das variaveis quantitativas
Vendas_1=select(Vendas, 1:4)
Vendas_2=select(Budget,1:4)

# juntar os dois arquivos
Vendas_hist <- rbind(Vendas_1, Vendas_2)

?ts
# criando uma serie temporal
serie = ts(Vendas_hist, start=c(2016,1),end=c(2019,12),frequency=12)
autoplot(serie)

serie_budget = ts(Vendas_hist[4], start=c(2016,1),end=c(2019,12),frequency=12)
autoplot(serie_budget)