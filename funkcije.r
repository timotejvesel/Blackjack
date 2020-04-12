
##############################################################
# karte
paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

# paketi kart
st_paketov <- function(n) {
  vse_karte <- rep(paket_kart, n)
  return(vse_karte)
}

#############################################################
# funkcija, ki doloci vrednost roke
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
##################################################################################
# "Strategija" dealerja. Dokler je vsota kart manjša od 17 vleče nove karte. Stand on soft 17. 
# Za tabele opt. strategij
dealer_str_opt <- function(d_roka) {
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


##################################################################
# strategija dealerja (brez stetja)
dealer_str_bs <- function(d_roka, karte, i, paketi) {
  vse_karte <- st_paketov(paketi)
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(d_roka)
    
    if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
      karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
      i <- 0
    }
    if (trenutna_vsota < 17) {
      i <- as.numeric(i)
      i <- i + 1
      d_roka <- c(d_roka, karte[i])
    } 
    else {
      stand <- TRUE
    }
    
  }
  return(list(c(trenutna_vsota, i),karte))
}


###################################################################################
# Strategija dealerja (stetje)
dealer_str_s <- function(d_roka, karte, i, paketi, running_count, stetje) {
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(d_roka)
    
    # Če zmanjka kart, se ustavimo. To se da verjetno narediti bolje.
    if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
      vse_karte <- st_paketov(paketi)
      karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
      i <- 0
      running_count <- 0
    }
    if (trenutna_vsota < 17) {
      i <- as.numeric(i)
      i <- i + 1
      d_roka <- c(d_roka, karte[i])
      running_count <- running_count + stetje(karte[i])
    } 
    else {
      stand <- TRUE
    }
    #cat(c("d roka 2:",d_roka,"\n"))
    #cat(c("running count d:",running_count,"\n"))
    
  }
  return(list(c(trenutna_vsota, i, running_count),karte))
}


#########################################################################
# strategija stetja hi-lo
hi_lo <- function(karta) {
  vrednost <- -1
  if (karta %in% c("2","3","4", "5", "6")) {
    vrednost <- 1 
  }
  else if (karta %in% c("7","8","9")) {
    vrednost <- 0
  }
  return(vrednost)
}


########################################################################
# strategija stetja Hi-Opt II
hi_opt2 <- function(karta) {
  vrednost <- 0
  if (karta %in% c("2","3","6", "7")) {
    vrednost <- 1 
  }
  else if (karta %in% c("4","5")) {
    vrednost <- 2
  }
  else if (karta %in% c("10")) {
    vrednost <- -2
  }
  return(vrednost)
}

####################################################################
# strategija stetja Wong Halves
wong_halves <- function(karta) {
  vrednost <- -1
  if (karta == "5") {
    vrednost <- 1.5
  }
  else if (karta %in% c("3","4","6")) {
    vrednost <- 1
  }
  else if (karta %in% c("2","7")) {
    vrednost <- 0.5
  }
  else if (karta == "8") {
    vrednost <- 0
  }
  else if (karta == "9") {
    vrednost <- -0.5
  }
  return(vrednost)
}


##################################################################
#koliko paketov je še ostalo
decks_remaining <- function(vse_karte, trenutna_karta) {
  ste_paketov <- vse_karte / 52
  meje <- seq(0, vse_karte, 52)
  if (trenutna_karta < meje[2]) {
    return(ste_paketov)
  }
  else if (trenutna_karta >= meje[ste_paketov]) {
    return(1)
  }
  for (i in 2:(ste_paketov - 1)) {
    if (trenutna_karta >= meje[i] & trenutna_karta < meje[i + 1]){
      return(ste_paketov - (i - 1))
    } 
  }
}
