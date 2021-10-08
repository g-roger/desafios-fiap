library(tidyverse)
library(readr)

# cadastro <- read.csv("Data/Estatísticas/trab1/cadastro.csv")
cadastro <- read_csv("Data/Estatísticas/trab1/cadastro.csv") 

view(cadastro)
length(cadastro)

# Estrutura
str(cadastro)

# Medidas Resumo
summary(cadastro$RENDA_PRESUMIDA)

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
media = mean(cadastro$RENDA_PRESUMIDA)
dp = sd(cadastro$RENDA_PRESUMIDA)
cv = dp/media
cv
# O desvio padrão é 10x o valor da média, isso demonstra a presença de outliers

# histograma
hist(cadastro$RENDA_PRESUMIDA)

# boxplot (para ter certeza dos outliers)
boxplot(cadastro$RENDA_PRESUMIDA)

# filtro para melhorar visualização
renda <- filter(cadastro, RENDA_PRESUMIDA < 10000)
hist(renda$RENDA_PRESUMIDA)
boxplot(renda$RENDA_PRESUMIDA)

par(mfrow=c(1,2))
par(mar=c(10,4,5,2))
hist(renda$RENDA_PRESUMIDA, xlab='Renda Presumida (R$)', ylab='Frequência Absoluta', 
     main='Histrograma da renda presumida')
boxplot(renda$RENDA_PRESUMIDA, ylab="Renda Presumida (R$)", 
        main="Box Plot da Renda Presumida (R$)")

summary(renda$RENDA_PRESUMIDA)

summary(cadastro$RENDA_PRESUMIDA)

iq = 856
q1 = 2068
q2 = 2432
q3 = 2924


quantile(cadastro$RENDA_PRESUMIDA)

limite1 = q3 + 3*iq
limite2 = q3 + 1.5*iq
limite3 = q1 - 1.5*iq
limite4 = q1 - 3*iq

limite1
limite2
limite3
limite4

cadastro$outlier_renda =cadastro$RENDA_PRESUMIDA
cadastro$outlier_renda =ifelse(cadastro$outlier_renda< -500,"2",ifelse(cadastro$outlier_renda>=-500&cadastro$outlier_renda<784,"1",ifelse(cadastro$outlier_renda>=784&cadastro$outlier_renda<=4208,"0",ifelse(cadastro$outlier_renda>4208&cadastro$outlier_renda<=5492,"1","2"))))

table(cadastro$outlier_renda)

renda_sout = filter(cadastro, cadastro$outlier_renda == 0)
summarise(renda_sout, media=mean(RENDA_PRESUMIDA), dp=sd(RENDA_PRESUMIDA))
cv = sd(renda_sout$RENDA_PRESUMIDA)/mean(renda_sout$RENDA_PRESUMIDA)
cv


d