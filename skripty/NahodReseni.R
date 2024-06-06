

NahodReseni <- function(vzdalenosti){
  
  n <- dim(vzdalenosti)[1]
  cesta <- sample(1:n,n)
  Z <- 0
  for(i in 1:(n-1)){
    Z <- Z + vzdalenosti[cesta[i],cesta[i+1]]
  }
  Z <- Z + vzdalenosti[cesta[n],cesta[1]]
  
  return(list(Z = Z, cesta = cesta))
}


# Mnohem slozitejsi zapis
# Vyse mnohem jednodussi

# NahodReseni <- function(vzdalenosti){
#   
#   n <- dim(vzdalenosti)[1]
#   start <- sample(1:n,1)
#   cesta <- start
#   zbyle <- setdiff(1:n,start)
#   Z <- 0
#   i = 0
#   
#   while(length(zbyle) > 1){
#     i = i + 1
#     uzel <- sample(zbyle, size = 1)
#     cesta <- c(cesta,uzel)
#     zbyle <- setdiff(zbyle,uzel)
#     Z <- Z + vzdalenosti[cesta[i],cesta[i+1]]
#   }
#   #predposledni uzel
#   cesta <- c(cesta,zbyle)
#   Z <- Z + vzdalenosti[cesta[i+1],cesta[i+2]]
#   
#   #posledni uzel
#   Z <- Z + vzdalenosti[cesta[n],cesta[1]]
#   
#   return(list(Z = Z, cesta = cesta))
# }
