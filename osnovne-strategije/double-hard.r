### V pravila dodan double
### Izračun optimalne strategije (hit, stand & double; dealer stands on soft 17)


source("funkcije.r") #karte, vsota_kart, strategija dealerja


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
    #cat(paste(c("Igralčeva roka (hit): ", igr_roka, "\n"), collapse=" "))
    trenutna_vsota <- vsota_karte(igr_roka)
    vrstica <- as.character(trenutna_vsota)
    stolpec <- as.character(d_odkrita)
    element <- double[vrstica, stolpec]
    element[is.na(element)] <- 0
    if (stevec == 0) { # če je strategija == hit, 1. vedno vzamemo novo karto
      igr_roka <- c(igr_roka, sample(paket_kart,1))
      stevec <- stevec + 1
    }
    else if (element == "H" || element == "D") { # ce po hit dobimo vsoto za strategijo double, lahko le hitamo.
      igr_roka <- c(igr_roka, sample(paket_kart,1))
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
    dealer <- dealer_str_opt(d_roka)
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
double <- data.frame(matrix(NA, nrow = 19, ncol = 10))
colnames(double) <- c(2:10, "A")
rownames(double) <- c(3:21)

# če je vstota >= 19 ocitno ne bomo vzeli nove karte (neodvisno od odkrite karte dealerja).
double[c("19","20","21"),] <- "S"
double[c("3","4"),] <- "H" 

n <- 100000 #stevilo iteracij

stolpci <- colnames(double)
vrstice <- c(18:5)


for (j in vrstice) {
  vs <- sum(j)
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
    print(zmaga.hit)
    print(zmaga.stand)
    print(zmaga.double)
    v <- c(zmaga.hit, zmaga.stand, zmaga.double)
    if (max(v) == get("zmaga.hit")) {
      double[as.character(vs), as.character(k)] <- "H"
      }
    else if (max(v) == get("zmaga.stand")) {
      double[as.character(vs), as.character(k)] <- "S"
      }
    else {
      double[as.character(vs), as.character(k)] <- "D"
    }
  }
}


# save(double, file="double-hard.Rda")


