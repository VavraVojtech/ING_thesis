

#######################################
# Geneticky algoritmus, GA, funkce
#######################################

################
# rodice
################

  # turnajovy vyber

VyberTurnaj <- function(Ztka, N, ex2){
  pocet <- 2^(ex2)
  aktualni_pocet <- N
  postup <- 1:N
  
  while(pocet < aktualni_pocet){
    #pom <- 1:(length(postup))
    pom <- postup
    vitezove <- NULL
  
    for(z in 1:(aktualni_pocet/2)){
      a <- sample(pom, 1)
      pom <- setdiff(pom, a)
      
      if(length(pom) == 1){
        b <- pom
      }else{
        b <- sample(pom, 1)
        pom <- setdiff(pom,b)
      }
      
      # vitezi a
      if(Ztka[a] < Ztka[b]){
        vitezove <- c(vitezove, a)
      # vitezi b
      }else{
        vitezove <- c(vitezove, b)
      }
    }
    postup <- vitezove
    aktualni_pocet <- length(postup)
  }
  
  vzor <- sample(postup, length(postup)/2)
  otec <- as.data.frame(vzor)
  
  postup <- postup[!(postup %in% vzor)]
  
  matka <- as.data.frame(sample(postup, length(postup)/2))
  
  rodice <- data.frame(otec,matka)
  colnames(rodice) <- c("otec", "matka")
  return(rodice)
}

####################################
# geneticke zmeny
###################################


###############
# KRIZENI PMX
###############
KrizeniPMX <- function(cesty, rodice, n, coefmin = 1/7, coefmax = 1/2){
  deti <- matrix(ncol = n)
  deti <- deti[-1,]
  
  for(i in 1:dim(rodice)[1]){
    # cesty[rodice[i,"otec"],]
    # cesty[rodice[i,"matka"],]
    
    min_jadro <- max(floor(n*coefmin),2)
    max_jadro <- ceiling(n*coefmax)
    
    j1 <- sample(1:(n-min_jadro),1)
    if((j1+min_jadro) == min(n,j1+max_jadro)){
      j2 = n
    }else{
      j2 <- sample((j1+min_jadro):min(n,j1+max_jadro),1)
    }
    
    jadro <- j1:j2
    nejadro <- setdiff(1:n, jadro)
    
    jadro_o <- cesty[rodice[i, "otec"],j1:j2]
    jadro_m <- cesty[rodice[i,"matka"],j1:j2]
    names(jadro_o) <- as.character(jadro_m)
    names(jadro_m) <- as.character(jadro_o)
    
    if(j1 == 1){
      zac <- c()
    }else{
      zac <- 1:(j1-1)
    }
    
    if(j2 == n){
      kon <- c()
    }else{
      kon <- (j2+1):n
    }
    
    dite_1 <-  c(cesty[rodice[i,"matka"],zac],
                 cesty[rodice[i, "otec"],j1:j2],
                 cesty[rodice[i,"matka"],kon])
    dite_2 <-  c(cesty[rodice[i, "otec"],zac],
                 cesty[rodice[i,"matka"],j1:j2],
                 cesty[rodice[i, "otec"],kon])
    
    
    while(length(unique(dite_1)) < n){
      ind <- nejadro[which(is.element(dite_1[nejadro], dite_1[jadro]))]
      dite_1[ind] <- jadro_m[as.character(dite_1[ind])]
    }
    
    while(length(unique(dite_2)) < n){
      ind <- nejadro[which(is.element(dite_2[nejadro], dite_2[jadro]))]
      dite_2[ind] <- jadro_o[as.character(dite_2[ind])]
    }
    
    deti <- rbind(deti, dite_1, dite_2)
  }
  
  rownames(deti) <- NULL
  return(deti)
}








###############################
# MUTACE
###############################


# SWAP
Swap <- function(cesticka, n){
  s <- sample(n,2)
  novacesta <- cesticka
  novacesta[s] <- novacesta[rev(s)]
  return(novacesta)
}


# INVERZE
Inverze <- function(cesticka, n){
  dvojice <- sample(n,2)
  i1 <- min(dvojice)
  i2 <- max(dvojice)
  
  if(i1 == 1){
    zac <- c()
  }else{
    zac <- 1:(i1-1)
  }
  
  if(i2 == n){
    kon <- c()
  }else{
    kon <- (i2+1):n
  }
  
  novacesta <- c(cesticka[zac],rev(cesticka[i1:i2]), cesticka[kon])
  return(novacesta)
}


# SCRAMBLE
Scramble <- function(cesticka, n){
  dvojice <- sample(1:n,2)
  sc1 <- min(dvojice)
  sc2 <- max(dvojice)
  l <- sc2-sc1+1
  
  if(sc1 == 1){
    zac <- c()
  }else{
    zac <- 1:(sc1-1)
  }
  
  if(sc2 == n){
    kon <- c()
  }else{
    kon <- (sc2+1):n
  }
  
  novacesta <- c(cesticka[zac],sample(cesticka[sc1:sc2],l), cesticka[kon])
  return(novacesta)
}


#### funkce s vyberem MUTACE
MutujCestu <- function(cesty, n, p = rep(1/3, 3)){
  mutovane_deti <- cesty
  for(i in 1:dim(cesty)[1]){
    modifikace <- sample(1:3, 1, prob = p)
    mutovane_deti[i,] <- switch(modifikace,
                        "1" = Swap(cesty[i,],n),
                        "2" = Inverze(cesty[i,],n),
                        "3" = Scramble(cesty[i,],n)
                        )
  }
  
  return(mutovane_deti)
}

