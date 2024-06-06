
rm(list = ls())
ROOT <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/data"
setwd(ROOT)
ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"


#### Doporucuji sjet níže na řádek cca. 190 a nejprve zkusit načíst soubory
# pokud zde, mohou se spustit zdlouhavé výpočty
# pro n = 7500 a n = 10000 zakomentováno

if(file.exists("nactene_funkce.RData")){
  load("nactene_funkce.RData")
}else{

# pomocne funkce
source(paste0(ROOT_2,"NahodReseni.R"))
source(paste0(ROOT_2,"M_GA_fce.R"))
source(paste0(ROOT_2,"SeradEuler.R"))
source(paste0(ROOT_2,"vypocti_Z.R"))

# heuristiky
source(paste0(ROOT_2,"H_nearest_neighbor.R"))
source(paste0(ROOT_2,"H_greedy.R"))
source(paste0(ROOT_2,"H_vyhod_cisla.R"))
source(paste0(ROOT_2,"H_min_kostra.R"))
source(paste0(ROOT_2,"H_2-opt.R"))
source(paste0(ROOT_2,"H_2-opt_1_step.R"))

# metaheuristiky
source(paste0(ROOT_2,"M_prah_akcept.R"))
source(paste0(ROOT_2,"M_SIAM.R"))
source(paste0(ROOT_2,"M_GA.R"))
source(paste0(ROOT_2,"M_ACO.R"))
source(paste0(ROOT_2,"M_BCO.R"))

save.image("nactene_funkce.RData")
}


if(file.exists("vypoctene_vzdalenosti2.RData")){
  load("vypoctene_vzdalenosti2.RData")
}else{
  # heu, metaheu
  print("Pocitam vzdalenosti, vse v pameti mazu, nacti funkce znovu.")
  
  rm(list = ls())
  ROOT <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/data"
  setwd(ROOT)
  ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"
  source(paste0(ROOT_2,"input.R"))
  source(paste0(ROOT_2,"data.R"))
  
  heuristiky <- c("NN",
                  "greedy",
                  "vyhod_c",
                  "mst",
                  "LK2")
  
  metaheuristiky <- c("TA",
                      "SIAM",
                      "GA",
                      "ACO",
                      "BCO")
  
  metody <- list()
  metody$heu <- heuristiky
  metody$meta <- metaheuristiky
  
  typy_metod <- c("heu", "meta")
  
  # rozlozeni velikosti dat
  velikost <- c("mala", "stredni", "velka")
  HEU_nka <- list()
  HEU_nka[[velikost[1]]] <- c(20, 30, 50, 75, 100)
  HEU_nka[[velikost[2]]] <- c(200, 300, 500, 750, 1000)
  HEU_nka[[velikost[3]]] <- c(2000, 3000, 5000)# 7500, 10000)
  
  
  ##################################
  
  #heuristiky
  
  data <- list()
  data[[velikost[1]]] <- list()
  data[[velikost[1]]][[1]] <- data20
  data[[velikost[1]]][[2]] <- data30
  data[[velikost[1]]][[3]] <- data50
  data[[velikost[1]]][[4]] <- data75
  data[[velikost[1]]][[5]] <- data100
  
  data[[velikost[2]]] <- list()
  data[[velikost[2]]][[1]] <- data200
  data[[velikost[2]]][[2]] <- data300
  data[[velikost[2]]][[3]] <- data500
  data[[velikost[2]]][[4]] <- data750
  data[[velikost[2]]][[5]] <- data1000
  
  data[[velikost[3]]] <- list()
  data[[velikost[3]]][[1]] <- data2000
  data[[velikost[3]]][[2]] <- data3000
  data[[velikost[3]]][[3]] <- data5000
  # data[[velikost[3]]][[4]] <- data7500
  # data[[velikost[3]]][[5]] <- data10000
  
  ####################################
  HEU_data_dist <- list()
  HEU_data_dist[[velikost[1]]] <- list()
  HEU_data_dist[[velikost[1]]][[1]] <- vzdalenost(data20)
  HEU_data_dist[[velikost[1]]][[2]] <- vzdalenost(data30)
  HEU_data_dist[[velikost[1]]][[3]] <- vzdalenost(data50)
  HEU_data_dist[[velikost[1]]][[4]] <- vzdalenost(data75)
  HEU_data_dist[[velikost[1]]][[5]] <- vzdalenost(data100)
  
  HEU_data_dist[[velikost[2]]] <- list()
  HEU_data_dist[[velikost[2]]][[1]] <- vzdalenost(data200)
  HEU_data_dist[[velikost[2]]][[2]] <- vzdalenost(data300)
  HEU_data_dist[[velikost[2]]][[3]] <- vzdalenost(data500)
  HEU_data_dist[[velikost[2]]][[4]] <- vzdalenost(data750)
  HEU_data_dist[[velikost[2]]][[5]] <- vzdalenost(data1000)
  
  HEU_data_dist[[velikost[3]]] <- list()
  HEU_data_dist[[velikost[3]]][[1]] <- vzdalenost(data2000)
  HEU_data_dist[[velikost[3]]][[2]] <- vzdalenost(data3000)
  HEU_data_dist[[velikost[3]]][[3]] <- vzdalenost(data5000)
  # HEU_data_dist[[velikost[3]]][[4]] <- vzdalenost(data7500)
  # HEU_data_dist[[velikost[3]]][[5]] <- vzdalenost(data10000)
  
  ##########################################################
  ##########################################################
  
  META_nka <- list()
  META_nka[[velikost[1]]] <- c(10, 20, 30, 40, 50)
  META_nka[[velikost[2]]] <- c(60, 70, 80, 90, 100)
  META_nka[[velikost[3]]] <- c(110, 120, 130, 140, 150)
  
  META_data <- list()
  META_data[[velikost[1]]] <- list()
  META_data[[velikost[1]]][[1]] <- data10
  META_data[[velikost[1]]][[2]] <- data20
  META_data[[velikost[1]]][[3]] <- data30
  META_data[[velikost[1]]][[4]] <- data40
  META_data[[velikost[1]]][[5]] <- data50
  
  META_data[[velikost[2]]] <- list()
  META_data[[velikost[2]]][[1]] <- data60
  META_data[[velikost[2]]][[2]] <- data70
  META_data[[velikost[2]]][[3]] <- data80
  META_data[[velikost[2]]][[4]] <- data90
  META_data[[velikost[2]]][[5]] <- data100
  
  META_data[[velikost[3]]] <- list()
  META_data[[velikost[3]]][[1]] <- data110
  META_data[[velikost[3]]][[2]] <- data120
  META_data[[velikost[3]]][[3]] <- data130
  META_data[[velikost[3]]][[4]] <- data140
  META_data[[velikost[3]]][[5]] <- data150
  
  META_data_dist <- list()
  META_data_dist[[velikost[1]]] <- list()
  META_data_dist[[velikost[1]]][[1]] <- vzdalenost(data10)
  META_data_dist[[velikost[1]]][[2]] <- vzdalenost(data20)
  META_data_dist[[velikost[1]]][[3]] <- vzdalenost(data30)
  META_data_dist[[velikost[1]]][[4]] <- vzdalenost(data40)
  META_data_dist[[velikost[1]]][[5]] <- vzdalenost(data50)
  
  META_data_dist[[velikost[2]]] <- list()
  META_data_dist[[velikost[2]]][[1]] <- vzdalenost(data60)
  META_data_dist[[velikost[2]]][[2]] <- vzdalenost(data70)
  META_data_dist[[velikost[2]]][[3]] <- vzdalenost(data80)
  META_data_dist[[velikost[2]]][[4]] <- vzdalenost(data90)
  META_data_dist[[velikost[2]]][[5]] <- vzdalenost(data100)
  
  META_data_dist[[velikost[3]]] <- list()
  META_data_dist[[velikost[3]]][[1]] <- vzdalenost(data110)
  META_data_dist[[velikost[3]]][[2]] <- vzdalenost(data120)
  META_data_dist[[velikost[3]]][[3]] <- vzdalenost(data130)
  META_data_dist[[velikost[3]]][[4]] <- vzdalenost(data140)
  META_data_dist[[velikost[3]]][[5]] <- vzdalenost(data150)
  
  save.image("vypoctene_vzdalenosti3.RData")
}




######################################################
######################################################
########     ------->>>>>>>   <<<<<<-------    #######
######################################################
######################################################

# zacneme nacitat tady -- je to bezpecnejsi
rm(list = ls())
ROOT <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/data"
setwd(ROOT)
ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"
source(paste0(ROOT_2,"balicky.R"))
source(paste0(ROOT_2,"TSPdanouMetodou.R"))
load("nactene_funkce.RData")
load("vypoctene_vzdalenosti2.RData")

# do odevzdávárny se tohle fakt nevešlo...
# load("vypoctene_vzdalenosti3.RData")

# vypocet poští k výpočtům další jádra počítače, pozor!
# před spuštěním nastudovat library(parallel)
# pocet tvých/vašich jader zjištíš/te následovně:
library(parallel)
detectCores(all.tests = FALSE, logical = TRUE)


# při novém spuštění inicializujeme vysledky znovu

vysledky <- data.frame()
# pres velikost datasetu
for(v in velikost[1:2]){  # velikost - vse, velikost[1] - jen male
  # velikost[1] - jen male
  # velikost[2] - jen stredni
  # velikost[3] - jen velke
  
  # volba typu metod "heu" nebo "meta"
  for(typ in typy_metod){
  # typ = "meta"
  # typ = "heu"
  
    if(typ == "meta"){nnn <- META_nka
    } else{           nnn <-  HEU_nka}
    
    # pres jednotliva nka
    # hlavni cyklus!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for(iin in 1:length(nnn[[v]])){
      n <- nnn[[v]][iin]
      if(typ == "meta"){distance <- META_data_dist[[v]][[iin]]
      } else{           distance <-  HEU_data_dist[[v]][[iin]]}
      
      cat("------Started: ", date(), "\n\n", sep = "")
        # seriove heuristiky
        if(typ == "heu"){
          for(metod in metody[[typ]]){
            print(paste0("v=", v, ", n=",n, ", metoda=",metod))
            
            vysledek <- TSPdanouMetodou(metod, distance, ROOT, ROOT_2)
            print(paste0("Trvalo mi to: ", vysledek[1], " sekund."))
            radek <- c(v, as.numeric(n), typ, metod, as.numeric(vysledek))
            
            vysledky <- rbind(vysledky, radek)
          } # end for metod
        } else{
          # paralelne metaheuristiky
          ncores <- length(metody[[typ]])
          myCluster <- makeCluster(ncores, nnodes = ncores)
          
          # paralelizovany system time -> jak dlouho tva cela paralelizace
          parst <- system.time(
            # napocitej si zvolene metody paralelne
            vysledek <- parLapply(cl = myCluster, # na jakem "clusteru" pocitat
                                  # hlavni parametr, kterym se odlisuji jednotlive paralelni behy
                                  # odpovida tomu prvnimu v pouzivane funkci
                                  X = metody[[typ]],
                                  # jakou funkci delat paralelne
                                  fun = TSPdanouMetodou,
                                  distance = distance,
                                  ROOT = ROOT,
                                  ROOT_2 = ROOT_2)
          )
          # ukonci cluster
          print(paste0("paralelne mi to trvalo ", parst[3], " sec"))
          stopCluster(myCluster)
          for(i in 1:ncores){
            radek <- c(v, as.numeric(n), typ, metody[[typ]][i],as.numeric(vysledek[[i]]))
            vysledky <- rbind(vysledky, radek)
        } # end for i
      } # end else
      colnames(vysledky) <- c("velikost", "n", "typ", "metoda", "time", "Z")
      vysledky$n <- as.numeric(vysledky$n)
      vysledky$time <- as.numeric(vysledky$time)
      vysledky$Z <- as.numeric(vysledky$Z)
      save(vysledky, file = "mezi_vysledky.RData")
    } # end for iin (n) ---- hlavni cyklus
  } # konec for typ (metody)
} # konec for v     (velikost)



# saving

colnames(vysledky) <- c("velikost", "n", "typ", "metoda", "time", "Z")
vysledky$n <- as.numeric(vysledky$n)
vysledky$time <- as.numeric(vysledky$time)
vysledky$Z <- as.numeric(vysledky$Z)
save(vysledky, file = "vysledky_mal_str_meta2.RData")



# loading
rm(list = ls())
ROOT <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/data"
setwd(ROOT)
ROOT_2 <- "D:/Documents/FIS VŠE/02 MAGISTR/diplomka - TSP/scripty/TSP/"


load("vysledky_mal_heu.RData")
mal_heu <- vysledky
load("vysledky_str_heu.RData")
str_heu <- vysledky


