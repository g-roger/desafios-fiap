# Criação de vetores
matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, byrow=TRUE)
?matrix()

# Fatores
temp <- c('alta', 'alta', 'media', 'alta', 'alta')
temp3 <- factor(temp, levels = c(0,1,2), labels =c('baixa', 'media', 'alta'))
temp3


# Vetores

nomes <- c("okleber", "cthomaz", "rpiveta", "groger")
sal_anual <- c(82200L, 11600L, 100L, 45000L)
eh_trabalhador <- c(TRUE, TRUE, FALSE, TRUE)
idade <- c(40, 22, 12, 25)

array(c("Natal dez", 25, 00.00, FALSE))

matrix(2 * 1:16,nrow=4, ncol=4, byrow=TRUE)

?data.frame

data <- data.frame(nomes,sal_anual,eh_trabalhador,idade)
view(data)

# renomear colunas
# colnames(data) <- c ('pri', 'seg', 'ter', 'qua')

# loopings
seq(1, 3, length.out=11)
seq_along(c(1,2,3))
seq_len(10)
seq.int(210)          

rep(0, times=40)
rep(c(0,1,2,3), times=10)
rep(c(0,1,2,3), each=10)

# Crie uma variável my_seq com 30 valores entre 5 e 10
help(":")
my_seq <- seq(5,10, length.out=30)
my_seq

# Qual a diferença das instruções
pi:10
10:pi

# Como consultar o tamanho do vetor my_seq
length(my_seq)

# Como fazer uma sequencia que acompanhe o tamanho do vetor my_seq?
  
seq_along(my_seq)
for(i in my_seq) print(i)

# valores especiais

inf_var <-  19/0
nan_var <- 0/0
fin_var <- 3
null_var <- NULL
na_var = NA
sqrt_error_var <- sqrt(-4)

vall <- c(inf_var, nan_var, fin_var, null_var, na_var)
vall
class(vall)

is.na(na_var)
is.infinite(inf_var)
is.nan(nan_var)
is.null(null_var)
is.finite(fin_var)


# Filtros
# lado esquerdo linha e direito coluna
mtcars[mtcars$mpg >= 15,]
mtcars[mtcars$mpg >= 15,c("hp")]


# CSV
german_credit <- read.csv2("Data/R/german_credit.csv")
nrow(german_credit)
