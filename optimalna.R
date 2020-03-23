

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
  if (vsota <= 9 & length(as_lokacije) > 1) {
    vsota <- vsota + 11
    vsota <- vsota + length(as_lokacije) - 1
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
dealer_str <- function(d_roka, karte, i) {
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    #cat(paste(c("Dealerjeva roka: ", d_roka, "\n"), collapse=" "))
    trenutna_vsota <- vsota_karte(d_roka)
    
    if (trenutna_vsota < 17) {
      i <- i + 1
      d_roka <- c(d_roka, karte[i])
    }
    else {
      stand <- TRUE
    }
    
  }
  return(c(trenutna_vsota,i))
}


# Strategija igralca. 
igralec_str <- function(igr_roka, karte, i, hit) {
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
    element <- hit.stand[vrstica, stolpec]
    element[is.na(element)] <- 0
    if (stevec == 0) { # če je hit == TRUE, 1. vedno vzammeo novo karto
      i <- i + 1
      igr_roka <- c(igr_roka, karte[i])
      stevec <- stevec + 1
    }
    else if (element == "H") {
      i <- i + 1
      igr_roka <- c(igr_roka, karte[i])
    } 
    else {
      stand <- TRUE
    }
    
  }
  return(c(trenutna_vsota, i))
}



igra <- function(karte, igr_roka, d_roka, hit) {
  i <- 0
  zmage <- 0

  # strategija za igralca
  igralec <- igralec_str(igr_roka, karte, i, hit)
  i <- igralec[2] #posodobimo indeks
    
  # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
  if (igralec[1] <= 21) {
    dealer <- dealer_str(d_roka, karte, i)
    i <- dealer[2]
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

# Uporablja se med 1 in 8 (ponavadi 6) paketov kart. Več paketov seveda pomeni večji house edge.
karte1 <- function(n) {
  vse_karte <- rep(paket_kart, n)
  return(vse_karte)
}


# 2 paketa kart
vse_karte <- karte1(8)

#tabela optimalne strategije
hit.stand <- data.frame(matrix(NA, nrow = 17, ncol = 10))
colnames(hit.stand) <- c(2:10, "A")
rownames(hit.stand) <- c(5:21)

# če je vstota >= 19 ne bomo vzeli nove karte (neodvisno od odkrite karte dealerja).
hit.stand[c("17","18","19","20","21"),] <- "S"

n <- 10000 #stevilo iteracij  

#stolpci <- colnames(hit.stand)
#vrstice <- list(c(,7),c(10,6),c(10,5),c(10,4),c(10,3),c(10,2),c(5,6),c(8,2),c(4,5),c(6,2),c(4,3),c(4,2),c(3,2))
stolpci <- 2

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
        
        # odstranimo karte, ki so v rokah iz vseh kart
        zmesane_karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
        zmesane_karte <- zmesane_karte[-match(igr_roka[1], zmesane_karte)]
        zmesane_karte <- zmesane_karte[-match(igr_roka[2], zmesane_karte)]
        zmesane_karte <- zmesane_karte[-match(d_roka[1], zmesane_karte)]
        
        d_roka <- c(d_roka, sample(zmesane_karte, 1)) # zakrita karta dealerja
        zmesane_karte <- zmesane_karte[-match(d_roka[2], zmesane_karte)]
        #print(d_roka)
        
        if (hit == TRUE) {
          zmaga.hit <- zmaga.hit + igra(zmesane_karte, igr_roka, d_roka, hit)
            }
        else {
          zmaga.stand <- zmaga.stand + igra(zmesane_karte, igr_roka, d_roka, hit)
        }
      }
    }
    print(zmaga.hit)
    print(zmaga.stand)
    if (zmaga.hit > zmaga.stand) {
      hit.stand[as.character(vs), as.character(k)] <- "H"
    }
    else {
      hit.stand[as.character(vs), as.character(k)] <- "S"
    }
  }
}
