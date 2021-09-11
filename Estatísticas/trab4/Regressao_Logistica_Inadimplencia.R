install.packages("tidyverse")
library(tidyverse)
library(tidyr) 
library(dplyr) 
library(ROCit)
library(readr)

# foram excluidos os casos missing de excpeso
library(readr)
dados <- read_delim("arq_inadimplencia.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)
View(dados)


# nao mostrar os resultados na notacao cientifica
options(scipen = 999)

attach(dados)

str(dados)

# Preprocessamento da base dos dados
# Apaga a coluna 
dados$cliente = NULL

# Mudar o formato da variável para qualitativa
dados$Resposta = factor(dados$Resposta)
dados$atrasos = factor(dados$atrasos)
dados$temporel = factor(dados$temporel)
dados$valorfatura = factor(dados$valorfatura)
dados$p_gastoalim = factor(dados$p_gastoalim)
dados$regiaorisco = factor(dados$regiaorisco)
dados$rendamensal = factor(dados$rendamensal)


set.seed(2019)
train <- sample(nrow(dados), 0.7*nrow(dados), replace = FALSE)
TrainSet <- dados[train,]
ValidSet <- dados[-train,]

fa1 = table(TrainSet$Resposta)
fr1=fa1/sum(fa1) ##frequencia relativa
fr1


fa2 = table(ValidSet$Resposta)
fr2=fa2/sum(fa2) ##frequencia relativa
fr2


# Regressao Logistica

Mod_Log<- glm(Resposta ~ .,
              TrainSet, family=binomial(link=logit))
summary(Mod_Log)


# aplicando na base de treino
TrainSet$predito = fitted(Mod_Log)



install.packages("ROCit")
library(ROCIT)

## rocit object
rocit_emp <- rocit(score = TrainSet$predito, 
                   class = TrainSet$Resposta, 
                   method = "emp")
rocit_bin <- rocit(score = TrainSet$predito, 
                   class = TrainSet$Resposta, 
                   method = "bin")
rocit_non <- rocit(score = TrainSet$predito, 
                   class = TrainSet$Resposta, 
                   method = "non")

summary(rocit_emp)
summary(rocit_bin) #maior AUC
summary(rocit_non)

## Plot ROC curve
plot(rocit_emp, col = c(1,"gray50"), 
     legend = FALSE, YIndex = FALSE)
lines(rocit_bin$TPR~rocit_bin$FPR, 
      col = 2, lwd = 2)
lines(rocit_non$TPR~rocit_non$FPR, 
      col = 4, lwd = 2)
legend("bottomright", col = c(1,2,4),
       c("Empirical ROC", "Binormal ROC",
         "Non-parametric ROC"), lwd = 2)

#AUC (Area Under the Curve) da curva ROC
ciAUC(rocit_emp)


# Teste Kolmogorov-Smirnov (KS)
#A estatística de Kolmogorov-Smirnov quantifica a distância entre a função distribuição 
#empírica da amostra e a função distribuição acumulada da distribuição de referência ou 
#entre as funções distribuição empírica de duas amostras.

rocit = rocit(score=TrainSet$predito, TrainSet$Resposta)
ksplot =ksplot(rocit)

class <-Mod_Log$y
score <- Mod_Log$fitted.values

roc_empirical <- rocit(score = score, class = class) 

methods(class="rocit")


summary(roc_empirical)
names(roc_empirical)

teste = cbind(roc_empirical$Cutoff, 
              roc_empirical$TPR, 
              roc_empirical$FPR)

TrainSet$fx_predito <- cut(TrainSet$predito, breaks=c(0,0.50,1), right=F)

table (TrainSet$fx_predito)
table (TrainSet$Resposta)

MC_log_treino <- table(TrainSet$Resposta, TrainSet$fx_predito , deparse.level = 2) # montar a matriz de confusão  
show(MC_log_treino) # mostra os resultados  
ACC_log = sum(diag(MC_log_treino))/sum(MC_log_treino)*100 # calcula a acurácia  
show(ACC_log) # mostra a acurácia  
print(prop.table(table(TrainSet$Resposta,TrainSet$fx_predito),1),digits=2)


# Amostra de validacao
predito <- predict(Mod_Log,ValidSet,type = "response")
summary(predito)   

hist(predito)

validSet1 = cbind(ValidSet,predito)

ROCit_obj =rocit(score=validSet1$predito,class=validSet1$Resposta)
plot(ROCit_obj)



## rocit object
rocit_emp1 <- rocit(score = validSet1$predito, 
                   class = validSet1$Resposta, 
                   method = "emp")
rocit_bin1 <- rocit(score = validSet1$predito, 
                   class = validSet1$Resposta, 
                   method = "bin")
rocit_non1 <- rocit(score = validSet1$predito, 
                   class = validSet1$Resposta, 
                   method = "non")

summary(rocit_emp1)
summary(rocit_bin1) #maior AUC
summary(rocit_non1)

## Plot ROC curve
plot(rocit_emp1, col = c(1,"gray50"), 
     legend = FALSE, YIndex = FALSE)
lines(rocit_bin1$TPR~rocit_bin1$FPR, 
      col = 2, lwd = 2)
lines(rocit_non1$TPR~rocit_non1$FPR, 
      col = 4, lwd = 2)
legend("bottomright", col = c(1,2,4),
       c("Empirical ROC", "Binormal ROC",
         "Non-parametric ROC"), lwd = 2)

#AUC (Area Under the Curve) da curva ROC
ciAUC(rocit_emp1)


teste1 = cbind(ROCit_obj$Cutoff, 
               ROCit_obj$TPR, 
               ROCit_obj$FPR)  

validSet1$fx_predito <- cut(predito, breaks=c(0, 0.50, 1), right=F)

### Matriz de confusãoTeste

MC_test_log <- table(validSet1$Resposta, validSet1$fx_predito , deparse.level = 2) # montar a matriz de confusão  
show(MC_test_log) # mostra os resultados  
ACC_test_log = sum(diag(MC_test_log))/sum(MC_test_log)*100 # calcula a acurácia  
show(ACC_test_log) # mostra a acurácia  
print(prop.table(table(validSet1$Resposta,validSet1$fx_predito),1),digits=2)

