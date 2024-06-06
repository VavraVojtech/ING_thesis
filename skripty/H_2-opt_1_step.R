
# Lin Kernighen, metoda zamen, pouze 1 krok

LK_2_opt_1_step <- function(vzdal, cesta, Z){
  i = 1
  j = 3
  neobjel_z5 = TRUE
  posunuta_cesta = cesta
  posl_i <- 1
  posl_j <- 3
  n <- dim(vzdal)[1]
  
  while(neobjel_z5){
    
    # symboly + a - musi byt na konci radku!
    delta <- vzdal[posunuta_cesta[1],posunuta_cesta[j]] + 
      vzdal[posunuta_cesta[2],posunuta_cesta[j+1]] - 
      vzdal[posunuta_cesta[1],posunuta_cesta[2]] - 
      vzdal[posunuta_cesta[j],posunuta_cesta[j+1]]
    
    if(delta<0){
      
      zac <- 1
      kon <- (j+1):n
      
      posunuta_cesta <- c(posunuta_cesta[zac],
                          rev(posunuta_cesta[2:j]),
                          posunuta_cesta[kon])
      Z <- Z + delta 
      posl_i <- i
      posl_j <- j
      break
      #neobjel_z5 <- TRUE
    }
    
    # posouvame celou cestu
    # posun i,j dopredu (pripadne na zacatek)
    if(j == n-1){
      if(i == n){
        i = 1
        j = 3
      }else{
        i = i+1
        j = 3
      }
      # musime posunout cestu
      posunuta_cesta <- c(posunuta_cesta[2:n], posunuta_cesta[1])
    }else{
      # i zustava
      j = j+1
    }
    
    # mame nove i,j ... nedosli jsme nahodou do mista, kde se naposledy neco delo?
    neobjel_z5 = !((i==posl_i) && (j==posl_j))
  }
  return(list(Z = Z, cesta = posunuta_cesta))
}
