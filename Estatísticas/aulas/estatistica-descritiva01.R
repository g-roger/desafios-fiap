library(tidyverse)
library(readr)

# cadastro <- read.csv("Data/Estat�sticas/trab1/cadastro.csv")
cadastro <- read_csv("Data/Estat�sticas/trab1/cadastro.csv") 

view(cadastro)
length(cadastro)

# Estrutura
str(cadastro)

# Medidas Resumo
summary(cadastro)

# Mudar o formato da vari�vel num�rica para qualitativa 
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

# coeficiente de varia��o (detec��o de fraudes)
cv = sd(cadastro$RENDA_PRESUMIDA)/mean(cadastro$RENDA_PRESUMIDA)
cv
# O desvio padr�o � 10x o valor da m�dia, isso demonstra a presen�a de outliers