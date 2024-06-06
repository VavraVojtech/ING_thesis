
# vypocti ucelove fnunkce pro vlozeny data.frame() cest pomoci matice vzdalenosti

VypoctiZtka <- function(cesty, vzdalenosti){
  
  n <- dim(vzdalenosti)[2]
  
  # kdyz poslu jen jednu cestu
  cesty <- matrix(cesty, ncol = n)
  
  Ztka <- numeric(dim(cesty)[1])
  
  for(j in 1:dim(cesty)[1]){
    cesta <- cesty[j,]
    Ztka[j] = 0
    for(i in 1:(n-1)){
      Ztka[j] <- Ztka[j] + vzdalenosti[cesta[i],cesta[i+1]]
    }
    Ztka[j] <- Ztka[j] + vzdalenosti[cesta[n],cesta[1]]
  }
  return(Ztka)
}
