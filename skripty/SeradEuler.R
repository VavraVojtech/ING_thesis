
SeradEuler <- function(double_hrany){
  # pojmenuj hrany
  double_hrany$name <- apply(double_hrany[,c("from","to")], 1,
                              function(ft){paste0(min(ft), "_", max(ft))})
  # index zacatecni hrany
  start <- which.min(double_hrany$from)
  # to co zbyva projit
  nepouzite <- double_hrany[-start,]
  # co jsme uz prosli
  pouzite <- double_hrany[start,]
  # kde se nachazim
  pozice <- double_hrany[start,"to"]
  # 2x, 1x, 0x -> kolikrat muzu hranu pouzit
  stupnehran <- table(nepouzite$name)
 
  # pocet vsech zdvojeny hran krome prvni jiz pouzite
  maxi <- dim(double_hrany)[1]-1
  
  for(i in 1:maxi){
    kammuzu <- nepouzite[nepouzite$from==pozice,]
    if(sum(stupnehran[kammuzu$name])>=2){
      # existuje hrana se stupnem 2 -> tam se da jit
      kammuzu <- kammuzu[stupnehran[kammuzu$name]==2,]
    }
    # kdyz ne, tak je v zasobniku jen jedna hrana
    kam <- kammuzu$to[1]
    hrana <- which((nepouzite$from==pozice) & (nepouzite$to == kam))
    
    # zapis a vymaz
    pouzite <- rbind(pouzite, nepouzite[hrana,])
    pozice2 <- nepouzite[hrana,"to"]
    hrananame <- paste0(min(pozice,pozice2), "_", max(pozice, pozice2))
    pozice <- pozice2
    stupnehran[hrananame] <- stupnehran[hrananame]-1
    nepouzite <- nepouzite[-hrana,]
  }
  
  return(pouzite)
}
























