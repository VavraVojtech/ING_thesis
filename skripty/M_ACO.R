
# ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"
# source(paste0(ROOT_2,"vypocti_Z.R"))

ACO <- function(vzdalenosti, coefm = 1, eps = 1e-10,
                alpha = 1, beta = 2, rho = 0.15,
                R = 100, nezlepseno = 20){
  
  n <- dim(vzdalenosti)[1]
  m <- floor(n*coefm)
  # také vhodné
  # m = 50
  
  if (m<4){
    stop("Malo mravencu, zvys coefm.")
  }
  
  
  # kdyby prisla nula ve vzdalenostech, proto priceteme tot male cislo
  # po deleni nulou by pak byl prusvih
  # eps <- 1e-10 # (1*(10^(-10)))
  
  # pro prav
  eta <- 1/(vzdalenosti+eps)
  feromon <- matrix(1, nrow = n, ncol = n)
  diag(feromon) <- 0
  
  cesta_nej <- c()
  Z_nej <- Inf
  r <- 1
  aktualizace <- 0
  
  # mene nez R iteraci ++ pokud se po 50 iteracich nezlepsi, zrusit
  while(r < R && aktualizace < nezlepseno){
    doslo_ke_zmene <- FALSE
    for(k in 1:m){
      i <- sample(1:n,1)
      povol_seznam <- setdiff(1:n,i)
      cesta <- i
      while(length(cesta)<n-1){
        prsti <- (feromon[i,povol_seznam])^alpha * (eta[i,povol_seznam])^beta
        #prsti <- prsti/sum(prsti)
        j <- sample(povol_seznam, size = 1, prob = prsti)
        # uprav seznam, cestu a i
        povol_seznam <- setdiff(povol_seznam,j)
        cesta <- c(cesta,j)
        i <- j
      } # end while cesty
      
      # pridej posledni uzel
      cesta <- c(cesta, povol_seznam)
      
      # aktualizace nejlepsi trasy
      Z <- VypoctiZtka(cesta,vzdalenosti)
      if (Z < Z_nej){
        Z_nej <- Z
        cesta_nej <- cesta
        doslo_ke_zmene = TRUE
      }
      
      # zanech feromon, lokalne
      # for(i in 1:(n-1)){
      #   feromon[cesta[i], cesta[i+1]] <- feromon[cesta[i], cesta[i+1]] + 1/Z
      # }
      # feromon[cesta[n], cesta[1]] <- feromon[cesta[n], cesta[1]] + 1/Z
      hrany_cesty <- matrix(c(cesta, cesta[2:n], cesta[1]), nrow = n)
      feromon[hrany_cesty] <- feromon[hrany_cesty]+1/Z
      
    } # end for mravence
    # globalni aktualizace feromonu, vyparovani
    feromon <- (1-rho) * feromon
    
    if (doslo_ke_zmene){
      aktualizace <- 0
    } else {
      aktualizace <- aktualizace + 1
    }
    
    r <- r + 1
  }
  return(list(Z = Z_nej, cesta = cesta_nej))
}

