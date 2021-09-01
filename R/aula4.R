set.seed(123)
datapsi <- data.frame(faixamodelo=factor(1:10),
                      safra01 = round(abs(rnorm(10,20,2)), 2),
                      safra02 = round(abs(rnorm(10, 20, 2)), 2))

datapsi$volSafra1 = datapsi$safra01/sum(datapsi$safra01)
datapsi$volSafra2 = datapsi$safra02/sum(datapsi$safra02)

psi = function(data){
  print(data)
  return ((data[,1]-data[,2]) * log(data[,1]/data[,2]))
}

datapsi$vPsi = psi(datapsi[,4:5])
datapsi
sum(datapsi$vPsi)

