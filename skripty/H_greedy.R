
# library(reshape2)

Hladovy <- function(vzdalenost){
  
  # inicializace
  n <- dim(vzdalenost)[1]
  Z <- 0
  k <- 0
  i <- 0
  j <- 0
  cesta <- data.frame(0,0,0)
  colnames(cesta) <- c("z", "do", "cena")
  stupen = rep(0,n)
  
  # na hl diagonalu plus nekonecno pro vymazani ze seznamu
  vzdalenost <- data.frame(as.matrix(vzdalenost) + diag(Inf, nrow = n, ncol = n))
  colnames(vzdalenost) <- 1:n
  
  # serad od nejmensi po nejvyssi
  vzdal_sort <- subset(melt(as.matrix(vzdalenost)), value!=Inf)
  vzdal_sort <- vzdal_sort[order(vzdal_sort$value),1:3]
  rownames(vzdal_sort) <- 1:(n^2-n)
  
  while (k < (n-1)){
    # which.min(vzdalenost)
    # nalezeni indexu min hodnoty
    # mini <- which(vzdalenost == min(vzdalenost, na.rm = TRUE), arr.ind = TRUE)
    mini <- vzdal_sort[1,]
    i <- as.numeric(mini[1,1])
    j <- as.numeric(mini[1,2])
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
            vzdal_sort <- vzdal_sort[-1,]
            stupen[i] = stupen[i] - 1
            stupen[j] = stupen[j] - 1
            continue = FALSE
            continue2 = FALSE
          } # end if
          else{
            continue = TRUE
            head = next_node
          }
        } # end if
        else{continue = FALSE}
      } # end while
    }
    if (continue2 == FALSE){next}
    
    # pridani hrany do reseni a pojdme dalsi hranu
    k <- k+1
    cesta[k,1] <- i
    cesta[k,2] <- j
    cesta[k,3] <- vzdalenost[i,j]
    Z = Z + vzdalenost[i,j]

    # co jiz nesmim pouzit pryc ze seznamu
    vzdal_sort <- vzdal_sort[-1,]
    vzdal_sort <- vzdal_sort[-which(vzdal_sort[,1] == i),]
    vzdal_sort <- vzdal_sort[-which(vzdal_sort[,2] == j),]
  } # end while
  
  # setrid cestu
  start = setdiff(cesta$z, cesta$do)
  ted = start
  konec = setdiff(cesta$do, cesta$z)
  cesta2 = ted
  
  while(ted != konec){
    ted = cesta[which(cesta$z == ted),2]
    cesta2 = c(cesta2,ted)
  }
  Z = Z + vzdalenost[konec,start]
  
  return(list(Z = Z, cesta = cesta2))
}
