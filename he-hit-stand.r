###  Simulacije BJ, kjer igralec igra po optimalni (osnovni strategiji). Samo hit & stand. Dealer stands on soft 17.


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

st_paketov <- function(n) {
  vse_karte <- rep(paket_kart, n)
  return(vse_karte)
}

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


dealer_str <- function(d_roka, karte, i) {
  vse_karte <- st_paketov(8)
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(d_roka)
    
    # Če zmanjka kart, se ustavimo. To se da verjetno narediti bolje.
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


# Strategija igralca. Strategiji iz double & double_soft ter hit.stand & soft1
igralec_str <- function(igr_roka, karte, i, d_roka, stava) {
  vse_karte <- st_paketov(8)
  st_kart <- length(karte)
  d_odkrita <- d_roka[1]#odkrita karta dealerja
  trenutna_vsota <- vsota_karte(igr_roka)
  vrstica <- as.character(trenutna_vsota) # vrednost igralčeve roke
  stolpec <- as.character(d_odkrita)
  if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
    karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
    i <- 0
  }
  
  if (length(igr_roka) == 2 & trenutna_vsota == 21) { #ce imamo blackjack (natural 21) koncamo, izplaca se 3:2
    stava <- 1.5 * stava
    return(list(c(trenutna_vsota, i, stava),karte))
  }
  
  if ("A" %in% igr_roka) {
    indeks <- match("A",igr_roka)
    vsota_brez <- vsota_karte(igr_roka[-indeks])
    if (vsota_brez == trenutna_vsota - 11) { # v roki je as vreden 11 -> soft hand
      element <- soft1[vrstica, stolpec]
    }
    else {
      element <- hit.stand[vrstica,stolpec]
    }
  }
  else {
    element <- hit.stand[vrstica, stolpec]
  }
  if (element == "S") { #če stand, ne vzamemo nobene nove karte
    return(list(c(trenutna_vsota, i, stava),karte))
  }
  
  # ce hit, vzamemo 1 karto in nadaljujemo
  else if (element == "H") {
    i <- i + 1
    igr_roka <- c(igr_roka, karte[i])
  }
  trenutna_vsota <- vsota_karte(igr_roka)
  if (trenutna_vsota > 21) { #ce vrednost roke > 21, ne moremo vec igrati (smo izgubili)
    return(list(c(trenutna_vsota, i, stava),karte))
  }
  stand <- FALSE
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(igr_roka)
    vrstica <- as.character(trenutna_vsota)
    
    if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
      karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
      i <- 0
    }
    
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
    if (element == "H") {
      i <- i + 1
      igr_roka <- c(igr_roka, karte[i])
      trenutna_vsota <- vsota_karte(igr_roka)
      if (trenutna_vsota > 21) { #ce vrednost roke > 21, ne moremo vec igrati (smo izgubili)
        stand <- TRUE
      }
    }
    else if (element == "S") {
      stand <- TRUE
    }
  }
  return(list(c(trenutna_vsota, i, stava),karte))
}


igra <- function(stava, iter) {
  zacetni_denar_igralec <- 100000000
  denar_igralec <- 100000000
  total_initial_bet <- 0
  vse_karte <- st_paketov(8)
  karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
  st_kart <- length(karte)
  i <- 0
  zasluzek_izguba <- 0
  
  for (j in 1:iter) {
    total_initial_bet <- total_initial_bet + stava
    i <- as.numeric(i)
    if (i <= (st_kart - 4)) {
      i <- i + 2
      igr_roka <- c(karte[i-1], karte[i])
      
      i <- i + 2
      d_roka <- c(karte[i-1], karte[i])
    }
    else {
      karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
      i <- 0
      i <- i + 2
      igr_roka <- c(karte[i-1], karte[i])
      
      i <- i + 2
      d_roka <- c(karte[i-1], karte[i])
    }
    
    # strategija za igralca
    igralec <- igralec_str(igr_roka, karte, i, d_roka, stava)
    i <- igralec[[1]][2] #posodobimo indeks
    karte <- igralec[[2]] #karte s katerimi igramo (ce jih vmes ne zmanjka, so prvotne)
    
    # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
    if (igralec[[1]][1] <= 21) {
      dealer <- dealer_str(d_roka, karte, i)
      i <- dealer[[1]][2]
      karte <- dealer[[2]]
    }
    
    nova_stava <- igralec[[1]][3]
    nova_stava <- as.numeric(nova_stava)
    ### možni rezultati:
    if (igralec[[1]][1] > 21) { #bust
      denar_igralec <- denar_igralec - nova_stava
    }
    else if (dealer[[1]][1] > 21) {
      denar_igralec <- denar_igralec + nova_stava
    }
    else if (igralec[[1]][1] > dealer[[1]][1]) {
      denar_igralec <- denar_igralec + nova_stava
    }
    else if (igralec[[1]][1] < dealer[[1]][1]) {
      denar_igralec <- denar_igralec - nova_stava
    }
    
  }
  total_lost <- zacetni_denar_igralec - denar_igralec
  house_edge <- total_lost / total_initial_bet
  return(house_edge)
}

############################################
iter <- 1000000
stava <- 100

load("hit-stand.Rda")
load("soft-pravilna.Rda")

#house edge
he <- igra(stava,iter)
