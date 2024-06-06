

# ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"
# source(paste0(ROOT_2,"H_2-opt_1_step.R"))
# source(paste0(ROOT_2,"H_nearest_neighbor.R"))
# source(paste0(ROOT_2,"vypocti_Z.R"))



BCO <- function(vzdalenosti, eps = 1e-10,
                howtheta = "Zweighted", inic_W = 50,
                K = 20, povolit_2opt = TRUE,
                alpha = 1, beta = 8, lambda = 0.9,
                R = 100, nezlepseno = 20){
  
  # howtheta = uniform --> vybirej theta rovnomerne z listu waggle
  # howtheta = Zweighted --> vybirej theta umerne k 1/Z
  
  n <- dim(vzdalenosti)[1]
  
  
  # kdyby prisla nula ve vzdalenostech, proto pricteme toto male cislo
  # po deleni nulou by pak byl prusvih
  # eps <- 1e-10 # (1*(10^(-10)))
  
  # pro prav
  eta <- 1/(vzdalenosti+eps)
  
  waggle <- as.data.frame(matrix(1:(n+3), nrow = 1))
  colnames(waggle) <- c(paste0("cesta", 1:n), "tanecnik", "Z", "D")
  vcely_nej <- matrix(0, nrow = n, ncol = n)
  Zvcely_nej <- rep(Inf, n)
  
  for(i in 1:n){
    NN <- NejSoused(vzdalenosti, start = i)
    waggle <- rbind(waggle, c(NN$cesta, i, NN$Z, inic_W))
    vcely_nej[i, ] <- NN$cesta
    Zvcely_nej[i] <- NN$Z
  }
  waggle <- waggle[-1,]
  
  
  cesta_nej <- c()
  Z_nej <- Inf
  r <- 1
  aktualizace <- 0
  waggle_neprazdny <- TRUE
  
  # mene nez R iteraci ++ pokud se po 50 iteracich nezlepsi, zrusit
  while(r < R && aktualizace < nezlepseno && waggle_neprazdny){
    doslo_ke_zmene <- FALSE
    for(k in 1:n){
      # sniz dobu waggle[[i]]
      indexy <- which(waggle$tanecnik==k)
      waggle[indexy, "D"] <- waggle[indexy, "D"]-1
      odstran <- indexy[waggle[indexy,"D"] <= 0]
      if(length(odstran) >0){
        waggle <- waggle[-odstran,]
      }
      
      i <- k
      povol_seznam <- setdiff(1:n,i)
      cesta <- i
      # vyber theta z listu waggle
      if(dim(waggle)[1] == 0){
        waggle_neprazdny <- FALSE
        break
      }
      
      theta <- switch(howtheta,
                      "uniform" = sample(1:dim(waggle)[1], size = 1),
                      "Zweighted" = sample(1:dim(waggle)[1], size = 1, prob = 1/waggle$Z))
      theta_cesta <- waggle[theta, 1:n]
      
      while(length(cesta)<n-1){
        # najdi F_i, což je vrchol, co následuje po i v theta
        pozice <- which(theta_cesta == i) # kde je i
        F_i <- theta_cesta[(pozice %% n) + 1] # co nasleduje po i
        # urcujeme podle zbytku po deleni n-kem a pricteme jednicku
        
        # pro uzly v povol_seznam nejprve napocitej vhodnost hrany rho_ij
            # vsichni dostanou:
            # (1-lambda*is.element(F_i,povol_seznam))/length(setdiff(povol_seznam,F_i))
            # pak vrchol F_i dostane lambda (prepsat), jestli F_i v povol_seznam je
        F_je_v_povol <- is.element(F_i,povol_seznam)
        rho_ij <- (1-lambda*F_je_v_povol)/length(setdiff(povol_seznam,F_i))
        if(F_je_v_povol){
          rho_ij[which(povol_seznam==F_i)] <- lambda
        }
        
        # prepocitej pravdepod z i do uzlu povol_seznam
        # misto feromon rho_ij
        
        prsti <- (rho_ij)^alpha * (eta[i,povol_seznam])^beta
        #prsti <- prsti/sum(prsti)
        j <- sample(povol_seznam, size = 1, prob = prsti)
        # uprav seznam, cestu a i
        povol_seznam <- setdiff(povol_seznam,j)
        cesta <- c(cesta,j)
        i <- j
      } # end while cesty
      
      # pridej posledni uzel
      cesta <- c(cesta, povol_seznam)
      Z <- VypoctiZtka(cesta,vzdalenosti)
      
      if(povolit_2opt == TRUE){
        nove <- LK_2_opt_1_step(vzdalenosti, cesta, Z)
        cesta <- nove$cesta
        Z <- nove$Z
      }
      
      # aktualizace pro konkretni vcelu i
      # pokud ano, tak pridej do waggle listu (+ dalsi propocty)
      if (Z < Zvcely_nej[k]){
        Zvcely_nej[k] <- Z
        vcely_nej[k,] <- cesta
        # vypocty pro waggle
        Pfcolony <- mean(1/waggle$Z)
        Pfi <- 1/Z
        Di <- K * Pfi/Pfcolony
        # uloz waggle
        waggle <- rbind(waggle, 
                        c(cesta, k, Z, Di))
      }
      
      # aktualizace pro globalne nej cestu
      if (Z < Z_nej){
        Z_nej <- Z
        cesta_nej <- cesta
        doslo_ke_zmene = TRUE
      }
      
    } # end for včelek
    
    if (doslo_ke_zmene){
      aktualizace <- 0
    } else {
      aktualizace <- aktualizace + 1
    }
    
    r <- r + 1
  } # end while hlavniho cyklu
  return(list(Z = Z_nej, cesta = cesta_nej, r = r, aktualizace = aktualizace,
              dimenze = dim(waggle)[1]))
}
