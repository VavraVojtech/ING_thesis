
# ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"
# source(paste0(ROOT_2,"M_GA_fce.R"))
# source(paste0(ROOT_2,"NahodReseni.R"))
# source(paste0(ROOT_2,"H_greedy.R"))
# source(paste0(ROOT_2,"vypocti_Z.R"))

GeneticAlg <- function(vzdalenost, ex1 = 8, ex2 = 6,
                       coefmin = 1/7, coefmax = 1/2,
                       prst = 0.5, vahy_mutaci = c(0.6,0.2,0.2),
                       parR = 100, strop_nezmeny = 200){
  
  # ex1 - 2^(ex1) je velikost populace
  # ex2 - 2^(ex2) je pocet rodicu (deti) pro vyber
  
  if(ex1 < ex2){
    stop("ex1 musi byt vyssi nez ex2")
  }
  
  if(prst > 1 || prst < 0){
    stop("prst je pravdepodobnost provedeni mutace v intervalu (0,1)")
  }
  
  n <- dim(vzdalenost)[1]
  N <- 2^ex1
  Ztka <- numeric(N)
  cesty <- matrix(0,nrow = N, ncol = n)
  
  # inicializace populace - nahodnymi pripustnymi resenimi
  
  for (i in 1:(N-1)){
    reseni <- NahodReseni(vzdalenost)
    Ztka[i] <- reseni$Z
    cesty[i,] <- reseni$cesta
  }
  
  # jedno reseni bude z Hladoveho algoritmu - greedy
  greedy <- Hladovy(vzdalenost)
  Ztka[N] <- greedy$Z
  cesty[N,] <- greedy$cesta
  
  
  # nalezeni nejlepsiho reseni - fitness člen
  
  fitness <- which.min(Ztka)
  Z_nej <- Ztka[fitness]
  cesta_nej <- cesty[fitness,]
  
  R <- n * parR
  r <- 0
  kdy_naposled <- 0
  
  while(r < R && kdy_naposled < strop_nezmeny){
    
    rodice <- VyberTurnaj(Ztka, N, ex2)
    
    deti <- KrizeniPMX(cesty, rodice, n, coefmin, coefmax)
    
    mam_mutovat <- rbinom(1,1,prst)
    if(mam_mutovat){
      deti <- MutujCestu(cesty = deti, n, p = vahy_mutaci)
    }
    
    Z_deti <- VypoctiZtka(deti,vzdalenost)
    
    cesty <- rbind(cesty, deti)
    Ztka <- c(Ztka, Z_deti)
    
    poradi <- order(Ztka)
    cesty <- cesty[poradi[1:N],]
    Ztka <- Ztka[poradi[1:N]]
    
    # Nove nejlepsi reseni?
    Z_novenej <- Ztka[1]
    cesta_novanej <- cesty[1,]
    if(Z_novenej == Z_nej){
      kdy_naposled <- kdy_naposled + 1
    }else{
      Z_nej <- Z_novenej
      cesta_nej <- cesta_novanej
      kdy_naposled <- 0
    }
    r <- r + 1
  }
  return(list(Z = Z_nej, cesta = cesta_nej))
}




