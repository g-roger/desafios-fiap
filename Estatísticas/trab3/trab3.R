library(readxl)
library(dplyr)
data <- read_excel("fiap/desafios-fiap/Estatísticas/trab3/sal_lab1.xlsx")

# Calcula z-score
data = data %>% 
  mutate(
    Ingestao_SalZ = (Ingestao_Sal - mean(Ingestao_Sal, na.rm = TRUE)) / sd(Ingestao_Sal, na.rm = TRUE)
  )


# Permite exibir graficos juntos
par(
  mfrow = c(1, 2)
) 

# Histograma 
hist(
  data$Ingestao_Sal,
  breaks = 10, 
  xlab = "Ingestao_Sal", 
  col = "gray", 
  main = "Histograma Sal Lab"
) 

# Histograma z-score
hist(
  data$Ingestao_SalZ,
  breaks = 10,
  xlab = "Ingestao_SalZ",
  col = "gray",
  main = "Histograma Sal Lab Z-score"
)

sum(data$Ingestao_SalZ)
