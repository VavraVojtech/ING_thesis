
# library(TSP)
# library(scatterplot3d)
# library(rgl)

###########################################
# definice funkce pro vypocet vzdalenosti - start
###########################################
vzdalenost <- function(data){
  distance <- data.frame()
  n <- as.numeric(nrow(data))
  for(i in 1:n){
    for(j in 1:n){
      x = data[i,1] - data[j,1]
      y = data[i,2] - data[j,2]
      z = data[i,3] - data[j,3]
      distance[i,j] <- round(sqrt(x^2 + y^2 + z^2)) #, digits = 4
    }
  }
  rownames(distance) <- c(1:n)
  colnames(distance) <- c(1:n)
  return(distance)
}




###########################################
# definice funkce pro vypocet vzdalenosti - konec
###########################################





###########################################
# definice funkce pro graf 3d statický - start
###########################################
# balíčky
# library(scatterplot3d)
# library(rgl)

graf_staticky <- function(data, tour){
  n <- as.numeric(nrow(data))
  obraz <- scatterplot3d(data)
  
  # start
  t1 <- obraz$xyz.convert(data[1,1],
                          data[1,2],
                          data[1,3])
  t2 <- obraz$xyz.convert(data[tour[[1]],1],
                          data[tour[[1]],2],
                          data[tour[[1]],3])
  segments(t1$x,t1$y,t2$x,t2$y,lwd=2,col=2)
  
  text(t1$x, 
       t1$y,             
       labels = "SUN",
       cex = .5, 
       pos = 4)  
  
  # stred
  for (i in 1:(n-1)){
    t1 <- obraz$xyz.convert(data[tour[[i]],1],
                            data[tour[[i]],2],
                            data[tour[[i]],3])
    t2 <- obraz$xyz.convert(data[tour[[i+1]],1],
                            data[tour[[i+1]],2],
                            data[tour[[i+1]],3])
    segments(t1$x,t1$y,t2$x,t2$y,lwd=2,col=2)
  }
  
  #konec
  t1 <- obraz$xyz.convert(data[tour[[n]],1],
                          data[tour[[n]],2],
                          data[tour[[n]],3])
  t2 <- obraz$xyz.convert(data[1,1],
                          data[1,2],
                          data[1,3])
  segments(t1$x,t1$y,t2$x,t2$y,lwd=2,col=2)
}
###########################################
# definice funkce pro graf 3d statický - konec
###########################################





###########################################
# definice funkce pro graf 3d statický - start
###########################################
# balíčky
# library(scatterplot3d)
# library(rgl)

graf_staticky2 <- function(data, tour){
  n <- as.numeric(nrow(data))
  data2 <- data[tour,]
  
  obraz <- scatterplot3d(data)
  # ham. cyklus
  for (i in 1:(n)){
    t1 <- obraz$xyz.convert(data2[i,1],
                            data2[i,2],
                            data2[i,3])
    t2 <- obraz$xyz.convert(data2[i+1,1],
                            data2[i+1,2],
                            data2[i+1,3])
    segments(t1$x,t1$y,t2$x,t2$y,lwd=2,col=2)
    
    # ocislovani poradi
    text(t1$x, 
         t1$y,             
         labels = i,
         cex = .5, 
         pos = 4)
    
    # ocislovani podle umisteni v tabulce
    
    #t0 <- obraz$xyz.convert(data[i,1],
    #                        data[i,2],
    #                        data[i,3])
    #text(t0$x, 
    #     t0$y,             
    #     labels = i,
    #     cex = .5, 
    #     pos = 4)
  }
  
  # uzavreni ham. cyklu
  t1 <- obraz$xyz.convert(data2[n,1],
                          data2[n,2],
                          data2[n,3])
  t2 <- obraz$xyz.convert(data2[1,1],
                          data2[1,2],
                          data2[1,3])
  segments(t1$x,t1$y,t2$x,t2$y,lwd=2,col=2)
}
###########################################
# definice funkce pro graf 3d statický2 - konec
###########################################












###########################################
# definice funkce pro graf 3d staticky 3 - start
###########################################


graf_staticky3 <- function(data, tour){
  n <- as.numeric(nrow(data))
  data2 <- data[tour,]
  
  obraz <- scatterplot3d(data2,
                         xlab = "X", ylab = "Y", zlab = "Z")
  # ham. cyklus
  body <- sapply(1:(length(tour)),
                 function(i){obraz$xyz.convert(data2[i,])})
  body2 <- data.frame(x = unlist(body["x",]),
                      y = unlist(body["y",]))
  t1 <- body2[1:(length(tour)),]
  t2 <- body2[c(2:(length(tour)),1),]
  segments(t1$x, t1$y,
           t2$x, t2$y,
           lwd=2,col=2)
  
  # ocislovani poradi
  text(t1$x, 
       t1$y,             
       labels = (1:n),
       cex = .75, 
       pos = 4)
}

###########################################
# definice funkce pro graf 3d staticky 3 - konec
###########################################







###########################################
# definice funkce pro graf 3d dynamický - start
###########################################
# balíčky
# rgl
# dplyr
graf_dynamicky <- function(data, tour){
  n <- as.numeric(nrow(data))
  data2 <- data[tour,]
  
  obraz <- plot3d(data) %>%
  + (for(i in 1:(n-1)){
    + segments3d(x=as.vector(t(data2[c(i,i+1),1])),
                 y=as.vector(t(data2[c(i,i+1),2])),
                 z=as.vector(t(data2[c(i,i+1),3])),
                 col=2,lwd=2)
  })
  + segments3d(x=as.vector(t(data2[c(n,1),1])),
               y=as.vector(t(data2[c(n,1),2])),
               z=as.vector(t(data2[c(n,1),3])),
               col=2,lwd=2)
  + texts3d(x = data[1,1],y = data[1,2], z = data[1,3], text = "SUN", cex = .75, pos = 4)
  #+ texts3d(x = data[,1],y = data[,2], z = data[,3], text = (1:n), cex = .75, pos = 4)
}
###########################################
# definice funkce pro graf 3d dynamický - konec
###########################################



###########################################
# definice funkce pro graf 3d dynamický2 - start
###########################################
graf_dynamicky2 <- function(data, tour){
  n <- as.numeric(nrow(data))
  data2 <- data[tour,]
  
  obraz <- open3d(scale=c(2,2,2))
  points3d(x = data2[,1],
           y = data2[,2],
           z = data2[,3])
  
  for(i in 1:(n-1)){
    segments3d(x=as.vector(t(data2[c(i,i+1),1])),
               y=as.vector(t(data2[c(i,i+1),2])),
               z=as.vector(t(data2[c(i,i+1),3])),
               col=2,lwd=2)
  }
  segments3d(x=as.vector(t(data2[c(n,1),1])),
             y=as.vector(t(data2[c(n,1),2])),
             z=as.vector(t(data2[c(n,1),3])),
             col=2,lwd=2)
  #texts3d(x = data[1,1],y = data[1,2], z = data[1,3], text = "SUN", cex = 1)
  texts3d(x = data2[,1],y = data2[,2], z = data2[,3],
          text = (1:n), cex = .75, pos = 4)
  axes3d()
  title3d(xlab="X", ylab="Y", zlab="Z")
}

###########################################
# definice funkce pro graf 3d dynamický2 - konec
###########################################


