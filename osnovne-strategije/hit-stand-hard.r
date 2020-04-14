# Iskanje optimalne strategije za hit & stand, hard hand

source("funkcije.r") #karte, vsota_kart, strategija dealerja

# Strategija igralca. 
igralec_str <- function(igr_roka, hit) {
  stevec <- 0
  d_odkrita <- d_roka[1]
  stand <- FALSE
  trenutna_vsota <- vsota_karte(igr_roka)
  if (hit == FALSE) {
    #cat(paste(c("Igralčeva roka (stand): ", igr_roka, "\n"), collapse=" "))
    return(trenutna_vsota)
  }
  
  while (stand != TRUE) {
    #cat(paste(c("Igralčeva roka (hit): ", igr_roka, "\n"), collapse=" "))
    trenutna_vsota <- vsota_karte(igr_roka)
    vrstica <- as.character(trenutna_vsota)
    stolpec <- as.character(d_odkrita)
    element <- hit.stand[vrstica, stolpec]
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


igra <- function(igr_roka, d_roka, hit) {
  zmage <- 0
  
  # strategija za igralca
  igralec <- igralec_str(igr_roka,hit)
  
  # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
  if (igralec[1] <= 21) {
    dealer <- dealer_str_opt(d_roka)
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

#tabela optimalne strategije
hit.stand <- data.frame(matrix(NA, nrow = 19, ncol = 10))
colnames(hit.stand) <- c(2:10, "A")
rownames(hit.stand) <- c(3:21)

# če je vstota >= 19 ne bomo vzeli nove karte (neodvisno od odkrite karte dealerja).
hit.stand[c("19","20","21"),] <- "S"
hit.stand[c("3","4"),] <- "H" 

stolpci <- 2
vrstice <- c(18:5)

n <- 100000 #stevilo iteracij


for (j in vrstice) {
  vs <- sum(j)
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
    #print(zmaga.hit)
    print(zmaga.stand)
    if (zmaga.hit > zmaga.stand) {
      hit.stand[as.character(vs), as.character(k)] <- "H"
    }
    else {
      hit.stand[as.character(vs), as.character(k)] <- "S"
    }
  }
}

#save(hit.stand,file="hit-stand-hard.Rda")
