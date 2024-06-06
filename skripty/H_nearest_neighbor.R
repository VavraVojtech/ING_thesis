
##########################################
# Nejblizsi soused
# Nearest neighbor - NN
##########################################

NejSoused <- function(vzdalenost, start = 1){
  n <- dim(vzdalenost)[1]
  #min = which(vzdalenost == min(vzdalenost), arr.ind = TRUE)
  #start <- min[1,1]
  zbyva <- setdiff(1:n, start)
  cesta <- start
  
  #cesta <- list()
  Z <- 0
  i <- 1
  pozice <- start # soucasna pozice obchodnika
  
  while(i < n){
    i <- i + 1
    # hledani nejkratsi cesty mezi zbyvajicimi
    vzd <- vzdalenost[pozice, zbyva]
    Z <- Z + min(vzd)
    novapozice <- zbyva[which.min(vzd)]
    # uprava parametru
    pozice <- novapozice
    cesta <- c(cesta, novapozice)
    #useknu, kam uz nemohu jit
    zbyva <- setdiff(zbyva, pozice)
  }
  Z <- Z + vzdalenost[pozice, start]

  return(list(Z = Z, cesta = cesta))
}

################################
# konec NN - NejSoused
################################
