###  Simulacije BJ. Igralec igra po slabi strategiji, tj. vedno hita do 15, neodvisno od kart dealerja

source("funkcije.r") #karte, vsota_kart, strategija dealerja

##################################################################
# Strategija igralca
igralec_str1 <- function(igr_roka, karte, i, d_roka, stava, meja, paketi, natural) {
  vse_karte <- st_paketov(paketi)
  st_kart <- length(karte)
  stand <- FALSE
  
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(igr_roka)
    if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
      karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
      i <- 0
    }
    if (length(igr_roka) == 2 & trenutna_vsota == 21) { #ce imamo blackjack (natural 21) koncamo, izplaca se 3:2
      stava <- natural * stava
      return(list(c(trenutna_vsota, i, stava),karte))
    }
    if (trenutna_vsota < meja) {
      i <- i + 1
      igr_roka <- c(igr_roka, karte[i])
    } 
    else {
      stand <- TRUE
    }
  }
  return(list(c(trenutna_vsota, i, stava),karte))
}


he.slaba <- function(stava, iter, meja, paketi, natural) {
  denar_igralec <- 0
  vse_karte <- st_paketov(paketi)
  karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
  st_kart <- length(karte)
  i <- 0
  zasluzek_izguba <- 0
  
  for (j in 1:iter) {
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
    igralec <- igralec_str1(igr_roka, karte, i, d_roka, stava, meja, paketi, natural)
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
  house_edge <- denar_igralec / (iter * stava)
  return(house_edge)
}

############################################

# paketi <- 8
# iter <- 100000
# stava <- 1
# meja <- 15
# natural <- 1
# he_slaba <- he.slaba(stava,iter,meja, paketi, natural)

