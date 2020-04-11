###  Simulacije BJ, kjer igralec igra po optimalni (osnovni strategiji). Hit, stand & double. Dealer stands on soft 17.
# program vrne house edge te igre.

source("funkcije.r") #karte, vsota_kart, strategija dealerja
load("tabele-strategij/double-hard.Rda") #tabele optimalne strategije
load("tabele-strategij/hit-stand-hard.Rda")
load("tabele-strategij/double-soft.Rda")
load("tabele-strategij/hit-stand-soft.Rda")


# Strategija igralca. Strategiji iz double & double_soft ter hit.stand & soft1
igralec_str3 <- function(igr_roka, karte, i, d_roka, stava, paketi, natural) {
  vse_karte <- st_paketov(paketi)
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
    stava <- natural * stava
    return(list(c(trenutna_vsota, i, stava),karte))
  }
  
  if ("A" %in% igr_roka) {
    indeks <- match("A",igr_roka)
    vsota_brez <- vsota_karte(igr_roka[-indeks])
    if (vsota_brez == trenutna_vsota - 11) { # v roki je as vreden 11 -> soft hand
      element <- double_soft[vrstica, stolpec]
      }
    else {
      element <- double[vrstica,stolpec]
      }
    }
  else {
    element <- double[vrstica, stolpec]
    }
  if (element == "S") { #če stand, ne vzamemo nobene nove karte
    return(list(c(trenutna_vsota, i, stava),karte))
  }
  else if (element == "D") { #če double, vzamemo 1 novo karto
    i <- i + 1
    igr_roka <- c(igr_roka, karte[i])
    stava <- stava * 2
    trenutna_vsota <- vsota_karte(igr_roka)
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
    
    if ("A" %in% igr_roka) {
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


he.double <- function(stava, iter, paketi, natural) {
  #zacetni_denar_igralec <- 100000000
  #denar_igralec <- 100000000
  denar_igralec <- 0
  #total_initial_bet <- 0
  vse_karte <- st_paketov(paketi)
  karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
  st_kart <- length(karte)
  i <- 0
  zasluzek_izguba <- 0
  
 for (j in 1:iter) {
   #total_initial_bet <- total_initial_bet + stava
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
    igralec <- igralec_str3(igr_roka, karte, i, d_roka, stava, paketi, natural)
    i <- igralec[[1]][2] #posodobimo indeks
    karte <- igralec[[2]] #karte s katerimi igramo (ce jih vmes ne zmanjka, so prvotne)
    
    # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
    if (igralec[[1]][1] <= 21) {
      dealer <- dealer_str_bs(d_roka, karte, i, paketi)
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
  #total_lost <- zacetni_denar_igralec - denar_igralec
  #house_edge <- total_lost / total_initial_bet
  house_edge <- denar_igralec / (iter * stava)
  return(house_edge)
}

############################################

# iter <- 100000
# stava <- 1 #zacetna stava
# paketi <- 8
# natural <- 1.5
# 
# he_double <- he.double(stava,iter, paketi, natural)
