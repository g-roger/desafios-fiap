library(tidyverse)
library(readr)

# cadastro <- read.csv("Data/Estatísticas/trab1/cadastro.csv")
cadastro <- read_csv("Data/Estatísticas/trab1/cadastro.csv") 

view(cadastro)
length(cadastro)

# Estrutura
str(cadastro)

# Medidas Resumo
summary(cadastro)

# Mudar o formato da variável numérica para qualitativa 
cadastro$CEP <- factor(cadastro$CEP)
cadastro$CEP_A <- factor(cadastro$CEP_A)
cadastro$NUMERO <- factor(cadastro$NUMERO)
cadastro$LATITUDE <- factor(cadastro$LATITUDE)
cadastro$LONGITUDE <- factor(cadastro$LONGITUDE)
cadastro$DDD_CELULAR <- factor(cadastro$DDD_CELULAR)
cadastro$CELULAR <- factor(cadastro$CELULAR)
cadastro$DDD_CELULAR_2 <- factor(cadastro$DDD_CELULAR_2)
cadastro$CELULAR_2 <- factor(cadastro$CELULAR_2)
cadastro$COD_BANCO <- factor(cadastro$COD_BANCO)
cadastro$NUM_AGENCIA<- factor(cadastro$NUM_AGENCIA)
cadastro$NUM_CONTA <-factor(cadastro$NUM_CONTA)
cadastro$CNPJ_CREDOR <- factor(cadastro$CNPJ_CREDOR)
cadastro$STATUS_CONSENTIMENTO <- factor(cadastro$STATUS_CONSENTIMENTO)


str(cadastro)
summary(cadastro)

summarise(cadastro, media=mean(cadastro$RENDA_PRESUMIDA), dp=sd(cadastro$RENDA_PRESUMIDA))

# coeficiente de variação (detecção de fraudes)
cv = sd(cadastro$RENDA_PRESUMIDA)/mean(cadastro$RENDA_PRESUMIDA)
cv
# O desvio padrão é 10x o valor da média, isso demonstra a presença de outliers