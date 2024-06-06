
# ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"
# source(paste0(ROOT_2,"SeradEuler.R"))
# source(paste0(ROOT_2,"Vypocti_Z.R"))


# minimalni kostra
# library(igraph)

MetMinKostra <- function(vzdalenosti){
  n <- dim(vzdalenosti)[1]
  
  # co vyhodit?
  pomoc <- data.frame(as.matrix(vzdalenosti) + diag(Inf, nrow = n, ncol = n))
  pomoc[upper.tri(pomoc)] <- Inf
  colnames(pomoc) <- 1:n
  
  # vyhodit!
  vzdal_sort <- subset(melt(as.matrix(pomoc)), value!=Inf)
  vzdal_sort <- vzdal_sort[order(vzdal_sort$value),1:3]
  rownames(vzdal_sort) <- 1:((n^2-n)/2)
  
  # udelej minimalni kostru grafik
  grafik <- graph_from_data_frame(vzdal_sort)
  MST <- as_data_frame(minimum.spanning.tree(grafik, weights = vzdal_sort$value))
  MST2 <- MST[,c(2,1,3)]
  colnames(MST2) <- colnames(MST)
  double_hrany <- rbind(MST, MST2)
  
  # pripadne se lze podivat
  # plot(minimum.spanning.tree(grafik, weights = vzdal_sort$value))
  
  # funkce na serazeni Eulerova cyklu
  euler <- SeradEuler(double_hrany)
  
  # uloz cestu aneb Euler na Hamiltona
  cesta <- as.numeric(unique(euler$from))
  
  # spocti ucelovku
  Z <- VypoctiZtka(cesta, vzdalenosti)
  
  return(list(Z = Z, cesta = cesta))
}




