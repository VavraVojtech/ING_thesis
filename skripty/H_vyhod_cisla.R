
VyhodCisla <- function(vzdalenost){
  
  n <- dim(vzdalenost)[1]
  S <- data.frame()
  k = 0
  Z = 0
  i = 0
  j = 0
  cesta <- data.frame(0,0,0)
  colnames(cesta) <- c("z", "do", "cena")
  stupen = rep(0,n)
  
  # vypocet vyhodnostnich cisel
  for (i in 1:n){
    for (j in 1:n){
      if (i==j || i == 1 || j == 1){
        S[i,j] = -Inf
        next
      }
      S[i,j] = vzdalenost[i,1] + vzdalenost[1,j] - vzdalenost[i,j]
    }
  }
  colnames(S) <- 1:n
  
  # serad od nejvyssi po nejnizsi
  S_sort <- subset(melt(as.matrix(S)), value!=-Inf)
  S_sort <- S_sort[order(S_sort$value, decreasing = TRUE),1:3]
  rownames(S_sort) <- 1:(n^2-3*n+2)
  
  while (k < (n-2)){
    # nalezeni indexu max hodnoty
    maxi <- S_sort[1,]
    i <- as.numeric(maxi[1,1])
    j <- as.numeric(maxi[1,2])
    stupen[i] = stupen[i] + 1
    stupen[j] = stupen[j] + 1
    
    #print(c(i,j,stupen[i],stupen[j]))
    
    continue = TRUE
    continue2 = TRUE
    
    # kontrola nevytvoreni podcyklu
    if((stupen[i] == 2) && (stupen[j] == 2)){
      tail <- i
      head <- j
      
      while(continue == TRUE){
        # Je hlava jiz pouzita jako ocas?
        if(head %in% cesta[,1]){
          next_node = cesta[which(cesta[,1] == head),2]
          if(next_node == tail){
            # vytvoren podcyklus, eliminuj
            # print("ven")
            S_sort <- S_sort[-1,]
            stupen[i] = stupen[i] - 1
            stupen[j] = stupen[j] - 1
            continue = FALSE
            continue2 = FALSE
          }
          else{
            continue = TRUE
            head = next_node
          }
        }
        else{
          continue = FALSE
        }
      } # end while
    }
    if (continue2 == FALSE){
      next
    }
    # pridani hrany do reseni a pojdme dalsi hranu
    k <- k+1
    cesta[k,1] <- i
    cesta[k,2] <- j
    cesta[k,3] <- vzdalenost[i,j]
    Z = Z + vzdalenost[i,j]
    
    # co jiz nesmim pouzit pryc ze seznamu
    S_sort <- S_sort[-1,]
    S_sort <- S_sort[-which(S_sort[,1] == i),]
    S_sort <- S_sort[-which(S_sort[,2] == j),]
  }
  
  # finalni uprava
  # setrid cestu
  start = setdiff(cesta$z, cesta$do)
  ted = start
  konec = setdiff(cesta$do, cesta$z)
  cesta2 = ted
  
  while(ted != konec){
    ted = cesta[which(cesta$z == ted),2]
    cesta2 = c(cesta2,ted)
  }
  cesta2 <- c(1,cesta2)
  Z = Z + vzdalenost[konec,1] + vzdalenost[1,start]
  
  return(list(Z = Z, cesta = cesta2))
}
