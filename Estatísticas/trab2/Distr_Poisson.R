install.packages("tidyverse")
library(tidyverse)
library(tidyr) 
library(dplyr) 

install.packages("ggplot2")
library(ggplot2)

install.packages("reshape")
library(reshape)

install.packages("scales")
library(scales)

library(readxl)
cartola2018 <- read_excel("cartola_fc_2017_2018.xlsx", 
                          sheet = "cartola fc 2018")
View(cartola2018)

cartola2018$gols=factor(cartola2018$gols)
barplot(cartola2018$gols)

options(digits=5)


ggplot(cartola2018, aes(x = as.factor(cartola2018$gols))) + 
  geom_bar()+
  labs(title = 'Distribuição do número de gols do Campeonato Brasileiro de 2018',
       y = 'Frequência absoluta',
       x = 'Número de gols')

summary(cartola2018$gols)

mean(cartola2018$gols)

# Calcular a probabilidade de gols
#dpois(x, lambda)
dpois(0, 2.18) 
dpois(1, 2.18) 
dpois(2, 2.18) 
dpois(3, 2.18) 
dpois(4, 2.18) 
dpois(5, 2.18) 
dpois(6, 2.18) 
dpois(7, 2.18) 



