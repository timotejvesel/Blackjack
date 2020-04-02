### Optimalna strategija za hit, stand & double (dealer stands on soft 17)
### Za soft hand!


vsota_karte <- function(roka) { #popravi, da bo ok za vse primere
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
igralec_str <- function(igr_roka, strategija, stava) {
  stevec <- 0
  d_odkrita <- d_roka[1]
  stand <- FALSE
  trenutna_vsota <- vsota_karte(igr_roka)
  if (strategija == "stand") { # če je strategija stand, ne vzamemo nobene karte.
    #cat(paste(c("Igralčeva roka (stand): ", igr_roka, "\n"), collapse=" "))
    return(c(trenutna_vsota, i, stava))
  }
  if (strategija == "double") { # če je strategija stand, moramo vzeti natanko 1 novo karto, poleg tega se podvoji še stava
    igr_roka <- c(igr_roka, sample(paket_kart,1))
    trenutna_vsota <- vsota_karte(igr_roka)
    nova_stava <- 2 * stava
    return(c(trenutna_vsota, i, nova_stava))
  }
  
  # ce je strategija hit, vsaj enkrat vzamemo novo karto
  while (stand != TRUE) {
    soft <- FALSE
    #cat(paste(c("Igralčeva roka (hit): ", igr_roka, "\n"), collapse=" "))
    trenutna_vsota <- vsota_karte(igr_roka)
    vrstica <- as.character(trenutna_vsota)
    stolpec <- as.character(d_odkrita)
    if ("AS" %in% igr_roka) {
      indeks <- match("A",igr_roka)
      vsota_brez <- vsota_karte(igr_roka[-indeks])
      if (vsota_brez == trenutna_vsota - 11) { # v roki je as vreden 11 -> soft hand
        element <- double_soft[vrstica, stolpec]
        element[is.na(element)] <- 0
        soft <- TRUE
      }
      else {
        element <- double[vrstica, stolpec]
        element[is.na(element)] <- 0
      }
    }
    else {
      element <- double[vrstica, stolpec]
      element[is.na(element)] <- 0
    }
    if (stevec == 0) { # če je strategija == hit, 1. vedno vzamemo novo karto
      igr_roka <- c(igr_roka, sample(paket_kart,1))
      stevec <- stevec + 1
    }
    else if (element == "H") {
      igr_roka <- c(igr_roka, sample(paket_kart,1))
    }
    else if (element == "D" & soft == FALSE) { #ker lahko podvojimo samo na začetku, moramo sicer pogledati v tabelo samo za hit & stand
      element <- hit.stand[vrstica, stolpec]
      if (element == "H") {
        igr_roka <- c(igr_roka, sample(paket_kart,1))
      }
      else if (element == "S") {
        stand <- TRUE
      }
    }
    else if (element == "D" & soft == TRUE) { #ker lahko podvojimo samo na začetku, moramo sicer pogledati v tabelo samo za hit & stand
      element <- soft1[vrstica, stolpec]
      if (element == "H") {
        igr_roka <- c(igr_roka, sample(paket_kart,1))
      }
      else if (element == "S") {
        stand <- TRUE
      }
    }
    else {
      stand <- TRUE
    }
    
  }
  return(c(trenutna_vsota,i,stava))
}



igra <- function(igr_roka, d_roka, strategija) {
  zmage <- 0
  stava <- 1
  # strategija za igralca
  igralec <- igralec_str(igr_roka,strategija, stava)
  
  # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
  if (igralec[1] <= 21) {
    dealer <- dealer_str(d_roka)
  }
  stava <- igralec[3]
  ### možni rezultati:
  if (igralec[1] > 21) { #bust
    zmage <- -stava
  }
  else if (dealer[1] > 21) {
    zmage <- stava
  }
  else if (igralec[1] > dealer[1]) {
    zmage <- stava
  }
  else if (igralec[1] < dealer[1]) {
    zmage <- -stava
  }
  #cat(paste(c(zmage,"\n"),sep = " "))
  return(zmage)
}

############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

#tabela optimalne strategije
double_soft <- data.frame(matrix(NA, nrow = 9, ncol = 10))
colnames(double_soft) <- c(2:10, "A")
rownames(double_soft) <- c(13:21)
double_soft[c("21"),] <- "S" # ce je vsota 21 (as + 10), ne bomo vzeli nove karte
vrstice <- list(c("A",9),c("A",8),c("A",7),c("A",6),c("A",5),c("A",4),c("A",3),c("A",2))
stolpci <- colnames(double_soft)

n <- 100000 #stevilo iteracij

for (j in vrstice) {
  vs <- 11 + as.numeric(j[2]) 
  igr_roka <- j #določimo karti za igralca (pomembna je vsota)
  print(j)
  for (k in stolpci) {
    zmaga.hit <- 0
    zmaga.stand <- 0
    zmaga.double <- 0
    for (z in c("hit", "stand", "double")) {
      strategija <- z
      for (iter in 1:n) {
        
        igr_roka <- j #karte igralca
        d_roka <- k #določimo odkrito karto dealerja
        if (d_roka != "A") {
          d_roka <- as.numeric(d_roka)
        }
        
        d_roka <- c(d_roka, sample(paket_kart, 1)) # zakrita karta dealerja
        #print(d_roka)
        
        if (strategija == "hit") {
          zmaga.hit <- zmaga.hit + igra(igr_roka, d_roka, strategija)
        }
        else if (strategija == "stand") {
          zmaga.stand <- zmaga.stand + igra(igr_roka, d_roka, strategija)
        }
        else {
          zmaga.double <- zmaga.double + igra(igr_roka, d_roka, strategija)
        }
      }
    }
    cat(paste(c(k,zmaga.hit,zmaga.stand,zmaga.double,"\n")))
    v <- c(zmaga.hit, zmaga.stand, zmaga.double)
    if (max(v) == get("zmaga.hit")) {
      double_soft[as.character(vs), as.character(k)] <- "H"
    }
    else if (max(v) == get("zmaga.stand")) {
      double_soft[as.character(vs), as.character(k)] <- "S"
    }
    else {
      double_soft[as.character(vs), as.character(k)] <- "D"
    }
  }
}

#save(double_soft,file="double-soft.Rda")

