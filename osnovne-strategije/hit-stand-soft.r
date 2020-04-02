# Iskanje optimalne strategije za hit & stand, soft hand

vsota_karte <- function(roka) { 
  asi <- FALSE
  as_lokacije <- numeric()
  vsota <- 0
  
  # poišči ase
  for (i in 1:length(roka)) {
    if(roka[i] == "A") {
      asi <- TRUE
      as_lokacije <- c(as_lokacije, i)
    }
  }
  
  # roka brez asov
  roka <- as.numeric(roka)
  if (asi == FALSE) {
    for (i in roka) {
      vsota <- vsota + i
    }
    return(vsota)
  }
  
  # v roki so asi
  ne_asi <- roka[!roka %in% "A"]
  ne_asi <- ne_asi[!is.na(ne_asi)] #ko odstranimo as namesto njega vsili NA???? V konzoli pa ne
  ne_asi <- as.numeric(ne_asi)
  vsota <- vsota + sum(ne_asi)
  
  # če vsota brez asov > 10, imajo vsi asi vrednost 1
  if (vsota > 10) {
    vsota <- vsota + length(as_lokacije)
    return(vsota)
  }
  
  # če vsota brez asov <= 9, je pomembno njihovo število. En je lahko 11, ostali pa 1.
  if (vsota <= 9 & length(as_lokacije) >= 1) {
    vsota <- vsota + 11
    vsota <- vsota + length(as_lokacije) - 1
    if (vsota > 21) {
      vsota <- vsota - 10 #ce imamo npr. 9 in 3 ase, so vsi vredni 1
    }
    return(vsota)
  }
  
  # če je vsota brez asov 10 in je 1 as => 11
  # če jih je več => vsi so 1
  if (vsota == 10 & length(as_lokacije) == 1) {
    vsota <- vsota + 11
    return(vsota)
  } 
  else {
    vsota <- vsota + length(as_lokacije)
    return(vsota)
  }
  
}


# "Strategija" dealerja. Dokler je vsota kart manjša od 17 vleče nove karte. Stand on soft 17
dealer_str <- function(d_roka) {
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    #cat(paste(c("Dealerjeva roka: ", d_roka, "\n"), collapse=" "))
    trenutna_vsota <- vsota_karte(d_roka)
    
    if (trenutna_vsota < 17) {
      d_roka <- c(d_roka, sample(paket_kart,1))
    }
    else {
      stand <- TRUE
    }
    
  }
  return(trenutna_vsota)
}


# Strategija igralca. 
igralec_str <- function(igr_roka, hit) {
  stevec <- 0
  d_odkrita <- d_roka[1]
  stand <- FALSE
  trenutna_vsota <- vsota_karte(igr_roka)
  if (hit == FALSE) {
    #cat(paste(c("Igralčeva roka (stand): ", igr_roka, "\n"), collapse=" "))
    return(c(trenutna_vsota, i))
  }
  
  while (stand != TRUE) {
    #cat(paste(c("Igralčeva roka (hit): ", igr_roka, "\n"), collapse=" "))
    trenutna_vsota <- vsota_karte(igr_roka)
    vrstica <- as.character(trenutna_vsota)
    stolpec <- as.character(d_odkrita)
    
    if ("AS" %in% igr_roka) {
      indeks <- match("A",igr_roka)
      vsota_brez <- vsota_karte(igr_roka[-indeks])
      if (vsota_brez == trenutna_vsota - 11) { # v roki je as vreden 11 -> soft hand
        element <- soft1[vrstica, stolpec]
      }
      else {
        element <- hit.stand[vrstica, stolpec] #hard hand
      }
    }
    else {
      element <- hit.stand[vrstica, stolpec] #hard hand
    }
    element[is.na(element)] <- 0
    if (stevec == 0) { # če je hit == TRUE, 1. vedno vzammeo novo karto
      igr_roka <- c(igr_roka, sample(paket_kart,1))
      stevec <- stevec + 1
    }
    else if (element == "H") {
      igr_roka <- c(igr_roka, sample(paket_kart,1))
    } 
    else {
      stand <- TRUE
    }
    
  }
  return(trenutna_vsota)
}



igra <- function(gr_roka, d_roka, hit) {
  zmage <- 0
  
  # strategija za igralca
  igralec <- igralec_str(igr_roka,hit)
  
  # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
  if (igralec[1] <= 21) {
    dealer <- dealer_str(d_roka)
  }
  
  ### možni rezultati:
  if (igralec[1] > 21) { #bust
    zmage <- -1
  }
  else if (dealer[1] > 21) {
    zmage <- 1
  }
  else if (igralec[1] > dealer[1]) {
    zmage <- 1
  }
  else if (igralec[1] < dealer[1]) {
    zmage <- -1
  }
  #cat(paste(c(zmage,"\n"),sep = " "))
  return(zmage)
}

############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)
n <- 10000 #stevilo iteracij

soft1 <- data.frame(matrix(NA, nrow = 9, ncol = 10))
colnames(soft1) <- c(2:10, "A")
rownames(soft1) <- c(13:21)
soft1[c("21","20"),] <- "S" # ce je vsota 21 (as + 10), ne bomo vzeli nove karte
vrstice <- list(c("A",8),c("A",7),c("A",6),c("A",5),c("A",4),c("A",3),c("A",2))
stolpci <- colnames(soft1)


for (j in vrstice) {
  print(j)
  vs <- 11 + as.numeric(j[2]) 
  igr_roka <- j #določimo karti za igralca (pomembna je vsota)
  for (k in stolpci) {
    zmaga.hit <- 0
    zmaga.stand <- 0
    for (z in c(TRUE, FALSE)) {
      hit = z
      for (iter in 1:n) {
        
        igr_roka <- j #karte igralca
        d_roka <- k #določimo odkrito karto dealerja
        if (d_roka != "A") {
          d_roka <- as.numeric(d_roka)
        }
        
        d_roka <- c(d_roka, sample(paket_kart, 1)) # zakrita karta dealerja
        #print(d_roka)
        
        if (hit == TRUE) {
          zmaga.hit <- zmaga.hit + igra(igr_roka, d_roka, hit)
        }
        else {
          zmaga.stand <- zmaga.stand + igra(igr_roka, d_roka, hit)
        }
      }
    }
    cat(paste(c("hit:",zmaga.hit,"stand",zmaga.stand, "\n"), sep=""))
    if (zmaga.hit > zmaga.stand) {
      soft1[as.character(vs), as.character(k)] <- "H"
    }
    else {
      soft1[as.character(vs), as.character(k)] <- "S"
    }
  }
}

#save(soft1,file="hit-stand-soft.Rda")
