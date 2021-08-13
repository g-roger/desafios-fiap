install.packages("explore")
install.packages("ggplot2")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(explore)

# dplyr
head(mtcars)
dim(mtcars)
names(mtcars)

# lib de exploração de dados
mtcars %>% explore_tbl()
mtcars %>% describe()
mtcars %>% explore_all()
mtcars %>% explore(gear)

# trabalhando com fatores
varCarros <- c("Golf", "Fusca", "Golf")
factor_carros <-factor(varCarros)
table(varCarros)
is.factor(varCarros)
is.factor(factor_carros)

# repetir linhas
rep("A", 10)
rep(c("A", "B"), times=5)
rep(c("A", "B"), each=5)

view(mtcars) # visualize df

summary(mtcars)
view(aggregate(mtcars, by=list(mtcars$cyl), FUN=mean))

# Filtros
# lado esquerdo linha e direito coluna
mtcars[mtcars$mpg >= 15,]
