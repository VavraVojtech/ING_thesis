
library(parallel)

TSPdanouMetodou <- function(metod,
                            distance,
                            ROOT, ROOT_2){
  # cluster si musi vse potrebne znovu nasourcovat
  # a taky musi vse potrebne dostat!
  
  source(paste0(ROOT_2,"balicky.R"))
  #load(paste0(ROOT,"/nactene_funkce.RData"))
  
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
  
  st <- system.time(
    vysledek <- switch(metod,
                       "NN" = NejSoused(distance),
                       "greedy" = Hladovy(distance),
                       "vyhod_c" = VyhodCisla(distance),
                       "mst" = MetMinKostra(distance),
                       "LK2"= LK_2_opt_nahoda(distance),
                       "TA" = PrahAkcept(distance),
                       "SIAM" = SIAM(distance),
                       "GA" = GeneticAlg(distance),
                       "ACO" = ACO(distance),
                       "BCO" = BCO(distance)
    )
  )
  cas_a_Z <- c(st[3], vysledek$Z)
  # vraci cas vypoctu a vyslednou ucelovku
  return(cas_a_Z)
}
