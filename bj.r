
# Paket 52 kart. J, Q in K so vredni 10, as je lahko vreden 1 ali 11.

paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

# Uporablja se med 1 in 8 (ponavadi 6) paketov kart. Več paketov seveda pomeni večji house edge.
karte <- function(n) {
  vse_karte <- rep(paket_kart, n)
  return(vse_karte)
}


# Funkcija sprejme karte, ki so v roki (igralca ali dealerja). Asom v roki se določi vrednost 1 ali pa 11.
# To je odvisno od vsote ostalih kart v roki.

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


# "Strategija" dealerja. Dokler je vsota kart manjša od 17 vleče nove karte. Kaj če ima "soft 17"?
dealer_str <- function(d_roka, karte, i) {
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(d_roka)
    
    # Če zmanjka kart, se ustavimo. To se da verjetno narediti bolje.
    if (i == st_kart) {
      stand <- TRUE
      
    }
    else if (trenutna_vsota < 17) {
      i <- i + 1
      d_roka <- c(d_roka, karte[i])
    } 
    else {
      stand <- TRUE
    }
    
  }
  return(c(trenutna_vsota, i))
}


# Strategija igralca. 
# Za začetek bo vzel novo karto, če bo vsota manjša od 16
igralec_str <- function(igr_roka, karte, i) {
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(igr_roka)
    
    # Kaj če zmanjka kart????
    if (i == st_kart) {
      stand <- TRUE
      
    }
    else if (trenutna_vsota < 16) {
      i <- i + 1
      igr_roka <- c(igr_roka, karte[i])
    } 
    else {
      stand <- TRUE
    }
    
  }
  return(c(trenutna_vsota, i))
}


igra <- function(karte, stava) {
  
  st_kart <- length(karte)
  i <- 0
  zmage <- 0
  
  # dokler se karte ne porabijo. To je treba izboljšati.
  while (i < st_kart) {
    if (i <= (st_kart - 4)) {
      i <- i + 2
      igr_roka <- c(karte[i-1], karte[i])
      
      i <- i + 2
      d_roka <- c(karte[i-1], karte[i])
    }
    else {
      # ni dovolj kart (tj.4)??????
      return(zmage)
    }
    
    # strategija za igralca
    igralec <- igralec_str(igr_roka, karte, i)
    i <- igralec[2] #posodobimo indeks
    
    # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
    if (igralec[1] <= 21) {
      dealer <- dealer_str(d_roka, karte, i)
      i <- dealer[2]
    }
    
    
    ### možni rezultati:
    if (igralec[1] > 21) { #bust
      zmage <- zmage - 1
    }
    else if (dealer[1] > 21) {
      zmage <- zmage + 1
    }
    else if (igralec[1] > dealer[1]) {
      zmage <- zmage + 1
    }
    else if (igralec[1] < dealer[1]) {
      zmage <- zmage - 1
    }
    
  }
  return(zmage)
}

############################################
N <- 12

stava <- 1

# 2 paketa kart
vse_karte <- karte(2)

#tabela zmag
rezultati <- data.frame(N = 1:12, zmage = numeric(N))

for (i in 1:N) {
  zmesane_karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
  rezultati$zmage[i] <- igra(zmesane_karte, stava)
}


