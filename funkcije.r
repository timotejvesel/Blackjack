
#tabele optimalne strategije

load("tabele-strategij/double-hard.Rda")
load("tabele-strategij/hit-stand-hard.Rda")
load("tabele-strategij/double-soft.Rda")
load("tabele-strategij/hit-stand-soft.Rda")



##############################################################
##############################################################

# FUNKCIJE ZA KARTE IN VREDNOST ROKE

##############################################################
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


##############################################################
##############################################################

# STRATEGIJE DEALERJA

##############################################################
##############################################################


# "Strategija" dealerja. Dokler je vsota kart manjša od 17 vleče nove karte. Stand on soft 17. 
# Za tabele opt. strategij
dealer_str_opt <- function(d_roka) {
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



##############################################################
##############################################################

# RAZLICNE STRATEGIJE STETJA KART

##############################################################
##############################################################

##############################################################
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


#############################################################
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

#############################################################
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


##############################################################
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





##############################################################
##############################################################

# STRATEGIJE IGRALCA IN IGRE ZA VERZIJI DOUBLE, HIT & STAND
# S STETJEM

##############################################################
##############################################################


#igralec strategija; za igri stetja

igralec_str_st <- function(igr_roka, karte, i, d_roka, stava, paketi, running_count, stetje, natural, tip) {
  vse_karte <- st_paketov(paketi)
  st_kart <- length(karte)
  d_odkrita <- d_roka[1]#odkrita karta dealerja
  trenutna_vsota <- vsota_karte(igr_roka)
  vrstica <- as.character(trenutna_vsota) # vrednost igralčeve roke
  stolpec <- as.character(d_odkrita)
  
  if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
    karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
    i <- 0
    running_count <- 0
  }
  
  if (length(igr_roka) == 2 & trenutna_vsota == 21) { #ce imamo blackjack (natural 21) koncamo, izplaca se 3:2
    stava <- natural * stava
    return(list(c(trenutna_vsota, i, stava, running_count),karte))
  }
  
  # pogledamo tip igre in strategijo glede na karte
  if (tip == "double") {
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
  }
  
  else if (tip == "hit_stand") {
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
  }
  
  #če stand, ne vzamemo nobene nove karte
  if (element == "S") { 
    return(list(c(trenutna_vsota, i, stava, running_count),karte))
  }
  
  #če double, vzamemo 1 novo karto; ce je igra hit_stand se ta moznost ne bo nikoli zgodila
  else if (element == "D") { 
    i <- i + 1
    igr_roka <- c(igr_roka, karte[i])
    stava <- stava * 2
    trenutna_vsota <- vsota_karte(igr_roka)
    return(list(c(trenutna_vsota, i, stava, running_count),karte))
  }
  
  # ce hit, vzamemo 1 karto in nadaljujemo
  else if (element == "H") {
    i <- i + 1
    igr_roka <- c(igr_roka, karte[i])
    running_count <- running_count + stetje(karte[i])
  }
  
  
  trenutna_vsota <- vsota_karte(igr_roka)
  if (trenutna_vsota > 21) { #ce vrednost roke > 21, ne moremo vec igrati (smo izgubili)
    return(list(c(trenutna_vsota, i, stava, running_count),karte))
  }
  
  
  # od tu naprej je enako za obe vrsti igre
  stand <- FALSE
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(igr_roka)
    vrstica <- as.character(trenutna_vsota)
    
    if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
      karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
      i <- 0
      running_count <- 0
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
      running_count <- running_count + stetje(karte[i])
      trenutna_vsota <- vsota_karte(igr_roka)
      if (trenutna_vsota > 21) { #ce vrednost roke > 21, ne moremo vec igrati (smo izgubili)
        stand <- TRUE
      }
    }
    else if (element == "S") {
      stand <- TRUE
    }
  }
  return(list(c(trenutna_vsota, i, stava, running_count),karte))
}

########################### 
# igra: stetje, double oz .hit&stand

igra.stetje <- function(stava, paketi, stetje, running_count, denar_igralec, trenutna, karte, natural, tip) {
  i <- trenutna
  i <- as.numeric(i)
  st_kart <- length(karte)
  
  if (i <= (st_kart - 4)) {
    i <- i + 2
    igr_roka <- c(karte[i-1], karte[i])
    #cat(c("igr_roka:",igr_roka,"\n"))
    running_count <- running_count + stetje(karte[i-1]) + stetje(karte[i])
    
    i <- i + 2
    d_roka <- c(karte[i-1], karte[i])
    running_count <- running_count + stetje(karte[i-1]) #vidimo samo dealerjevo odkrito karto
    running_count <- running_count + stetje(karte[i]) #na koncu vidimo tudi njegovo odkrito karto; vseeno kdaj prištejemo 
    #cat(c("d_roka:",d_roka,"\n"))
    #cat(c("running cout:",running_count,"\n"))
    
  }
  else {
    vse_karte <- st_paketov(paketi)
    karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
    running_count <- 0
    i <- 0
    i <- i + 2
    igr_roka <- c(karte[i-1], karte[i])
    running_count <- running_count + stetje(karte[i-1]) + stetje(karte[i])
    
    i <- i + 2
    d_roka <- c(karte[i-1], karte[i])
    running_count <- running_count + stetje(karte[i-1])
    running_count <- running_count + stetje(karte[i])
  }
  
  # strategija za igralca
  igralec <- igralec_str_st(igr_roka, karte, i, d_roka, stava, paketi, running_count, stetje, natural, tip)
  
  i <- igralec[[1]][2] #posodobimo indeks
  karte <- igralec[[2]] #karte s katerimi igramo (ce jih vmes ne zmanjka, so prvotne)
  running_count <- igralec[[1]][4] #posodobimo running count
  
  # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
  if (igralec[[1]][1] <= 21) {
    dealer <- dealer_str_s(d_roka, karte, i, paketi, running_count, stetje)
    i <- dealer[[1]][2]
    karte <- dealer[[2]]
    running_count <- dealer[[1]][3] #posodobimo running_count
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
  #cat(c("denar igralec", denar_igralec, "\n"))
  return(list(c(running_count,i, denar_igralec),karte))
}


################################################################
# koncna funkcija za igro s stetjem kart

counting <- function(stava, paketi, iter, natural, stetje, tip, updateProgress = NULL) {
  trenutna <- 0
  #paketi <- 8 #st. paketov kart s katerimi igramo
  stevilo_kart <- paketi * 52
  vse_karte <- st_paketov(paketi)
  karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
  #print(karte)
  denar_igralec <- 0
  
  #iter <- 100 #koliko iger odigramo
  
  betting_unit <- stava
  running_count <- 0
  vsota_zacetne_stave <- 0
  #stetje <- hi_lo #vrsta stetja kart
  
  stava <- betting_unit
  vsota_zacetne_stave <- stava
  #rc <- c()
  #tc <- c()
  podatki <- data.frame("stevilo" = 0, "denar" = 0)
  for (j in 1:iter) {
    bj <- igra.stetje(stava, paketi, stetje, running_count, denar_igralec, trenutna, karte, natural, tip)
    running_count <- bj[[1]][1]
    trenutna <- bj[[1]][2]
    denar_igralec <- bj[[1]][3]
    karte <- bj[[2]]
    true_count <- floor(running_count / decks_remaining(stevilo_kart, trenutna))
    #rc <- c(rc,running_count)
    #tc <- c(tc,true_count)
    if (true_count <= 1) {
      stava <- 1
    }
    else if (true_count < 6) {
      stava <- (true_count - 1) * betting_unit * 1.25
    }
    else {
      stava <- (true_count - 1) * betting_unit * 1.5
    }
    if (j != iter) {
      #cat(c("stava",stava,"\n"))
      vsota_zacetne_stave <- vsota_zacetne_stave + stava
      
    }
    if (j %% 10 == 0) {
      podatki <- rbind(podatki, c(j, denar_igralec))
    }
    
    house_edge_trenutni <- denar_igralec / vsota_zacetne_stave
    if (is.function(updateProgress)) {
      if (j %% 500 == 0 || j == 1) {
        # HE posodabljamo na vsakih 500 iteracij
        izpis <- paste0(paste("Trenuten house edge:", round(house_edge_trenutni * 100, 2)), "%")
        
        if (iter == 10000) {updateProgress(detail = izpis, delez = 6)}
        else if (iter == 100000) {updateProgress(detail = izpis, delez = 60)}
        else if (iter == 1000000) {updateProgress(detail = izpis, delez = 600)}
        
      }
    }
  }
  house_edge <- denar_igralec / vsota_zacetne_stave
  #cat(c(denar_igralec,"\n"))
  #cat(vsota_zacetne_stave)
  return(list(house_edge, podatki))
}



##############################################################
##############################################################

# STRATEGIJE IGRALCA IN IGRE ZA VERZIJE "SLABA", DOUBLE, HIT & STAND
# BREZ STETJA

##############################################################
##############################################################

# strategija igralca za double ter hit_stand

igralec_str_brez_st <- function(igr_roka, karte, i, d_roka, stava, paketi, natural, tip) {
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
  
  # ce imamo blackjack (natural 21) koncamo
  if (length(igr_roka) == 2 & trenutna_vsota == 21) { 
    stava <- natural * stava
    return(list(c(trenutna_vsota, i, stava),karte))
  }
  
  
  # ce je tip double, gledamo v drugo tabelo kot ce hit_stand
  if (tip == "double") {
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
  }
  else if (tip == "hit_stand") {
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
  }
  
  
  #če stand, ne vzamemo nobene nove karte
  if (element == "S") { 
    return(list(c(trenutna_vsota, i, stava),karte))
  }
  
  #če double, vzamemo 1 novo karto, pri hit_stand ta moznost ni mogoca
  else if (element == "D") { 
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
  
  
  ## od tu napjre je za oba tipa iger enako
  trenutna_vsota <- vsota_karte(igr_roka)
  
  #ce vrednost roke > 21, ne moremo vec igrati (smo izgubili)
  if (trenutna_vsota > 21) { 
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


###############################################
# "slaba" strategija igralca

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

################################################
# igra

he.bs <- function(stava, iter, paketi, natural, tip, updateProgress = NULL, meja = NULL) {
  denar_igralec <- 0
  vse_karte <- st_paketov(paketi)
  karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
  st_kart <- length(karte)
  i <- 0
  vsota_zacetne_stave <- 0
  
  podatki <- data.frame("stevilo" = 0, "denar" = 0)
  
  for (j in 1:iter) {
    vsota_zacetne_stave <- vsota_zacetne_stave + stava
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
    if (tip == "slaba") {
      igralec <- igralec_str1(igr_roka, karte, i, d_roka, stava, meja, paketi, natural) 
    }
    else {
      igralec <- igralec_str_brez_st(igr_roka, karte, i, d_roka, stava, paketi, natural, tip)
    }
      
    
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
    
    if (j %% 10 == 0) {
      podatki <- rbind(podatki, c(j, denar_igralec))
    }
    
    house_edge_trenutni <- denar_igralec / vsota_zacetne_stave
    
    if (is.function(updateProgress)) {
      if (j %% 500 == 0 || j == 1) {
        # HE posodabljamo na vsakih 500 iteracij
        izpis <- paste0(paste("Trenuten house edge:", round(house_edge_trenutni * 100, 2)), "%")
        
        if (tip == "slaba") {
          if (iter == 10000) {updateProgress(detail = izpis, delez = 5)}
          else if (iter == 100000) {updateProgress(detail = izpis, delez = 50)}
          else if (iter == 1000000) {updateProgress(detail = izpis, delez = 500)}
        }
        else {
          if (iter == 10000) {updateProgress(detail = izpis, delez = 5.5)}
          else if (iter == 100000) {updateProgress(detail = izpis, delez = 55)}
          else if (iter == 1000000) {updateProgress(detail = izpis, delez = 550)}
        }
        
      }
    }
    
  }
  house_edge <- denar_igralec / (iter * stava)
  return(list(house_edge,podatki))
}


