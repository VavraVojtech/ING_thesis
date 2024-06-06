# ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"
# source(paste0(ROOT_2,"M_GA_fce.R"))
# source(paste0(ROOT_2,"NahodReseni.R"))
# source(paste0(ROOT_2,"vypocti_Z.R"))
# source(paste0(ROOT_2,"H_2-opt_1_step.R"))

PrahAkcept <- function(vzdalenosti, procento = 0.05, p = c(0.3,0.4,0.3),
                       r = 0.95, parR = 200, strop_nezmeny = 1300){
  n <- dim(vzdalenosti)[1]
  nahoda <- NahodReseni(vzdalenosti)
  
  cesta <- nahoda$cesta
  cesta_nej <- cesta
  
  Z <- nahoda$Z
  Z_nej <- Z
  
  # T = Praha
  Praha <- Z_nej * procento
  
  R = parR * n
  iter = 0
  kdy_naposled <- 0
  
  while(iter < R && kdy_naposled < strop_nezmeny){
    
    modifikace <- sample(1:3, 1, prob = p)
    nove <- switch(modifikace,
                   "1" = Swap(cesta,n),
                   "2" = Inverze(cesta,n),
                   "3" = LK_2_opt_1_step(vzdalenosti, cesta, Z)
    )
    if(class(nove)=="list"){
      cesta_nova <- nove$cesta
      Z_nova <- nove$Z
    } else{
      cesta_nova <- nove
      Z_nova <- VypoctiZtka(cesta_nova, vzdalenosti)
      }
    
    if(Z_nova - Z < Praha){
      cesta <- cesta_nova
      Z <- Z_nova
      if(Z < Z_nej){
        cesta_nej <- cesta
        Z_nej <- Z
        kdy_naposled <- 0
        Praha <- Z_nej * procento
      }
    }
    Praha <- Praha*r
    iter <- iter + 1
    kdy_naposled <- kdy_naposled + 1
  } # end while
  return(list(Z = Z_nej, cesta = cesta_nej, iter = iter, kdy_naposled = kdy_naposled))
}
