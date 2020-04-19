
source("funkcije.r")

load("tabele-strategij/hit-stand-hard.Rda")
load("tabele-strategij/hit-stand-soft.Rda")

##################################################

igralec_str4 <- function(igr_roka, karte, i, d_roka, stava, paketi, running_count, stetje, natural) {
  st_kart <- length(karte)
  d_odkrita <- d_roka[1] #odkrita karta dealerja
  trenutna_vsota <- vsota_karte(igr_roka)
  vrstica <- as.character(trenutna_vsota) # vrednost igralčeve roke
  stolpec <- as.character(d_odkrita)
  vse_karte <- st_paketov(paketi)
  
  if (i == st_kart) { # če porabimo vse karte, zmešamo nove 
    karte <- sample(vse_karte, length(vse_karte), replace = FALSE)
    i <- 0
    running_count <- 0
  }
  
  if (length(igr_roka) == 2 & trenutna_vsota == 21) { #ce imamo blackjack (natural 21) koncamo, izplaca se 3:2
    stava <- natural * stava
    return(list(c(trenutna_vsota, i, stava, running_count),karte))
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
  #cat(c("igr roka 2:",igr_roka,"\n"))
  #cat(c("running count igr:",running_count,"\n"))
  return(list(c(trenutna_vsota, i, stava, running_count),karte))
}

###################################################################################################

igra.hs <- function(stava, paketi, stetje, running_count, denar_igralec, trenutna, karte, natural) {
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
  igralec <- igralec_str4(igr_roka, karte, i, d_roka, stava, paketi, running_count, stetje, natural)
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
#########################################################################

counting.hs <- function(stava, paketi, iter, natural, stetje, updateProgress = NULL) {
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
  # rc <- c()
  # tc <- c()
  podatki <- data.frame("stevilo" = 0, "denar" = 0)
  for (j in 1:iter) {
    bj <- igra.hs(stava, paketi, stetje, running_count, denar_igralec, trenutna, karte, natural)
    running_count <- bj[[1]][1]
    trenutna <- bj[[1]][2]
    denar_igralec <- bj[[1]][3]
    karte <- bj[[2]]
    true_count <- floor(running_count / decks_remaining(stevilo_kart, trenutna))
    # rc <- c(rc,running_count)
    # tc <- c(tc,true_count)
    if (true_count <= 1) {
      stava <- 1
    }
    else if (true_count < 6) {
      stava <- (true_count - 1) * betting_unit * 1.25
    }
    else {
      stava <- (true_count) * betting_unit * 1.5
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
  return(list(house_edge,podatki))
}

##########################################################################
# hes <- c()
# for (k in 1:1000) {
#   hes <- c(hes,counting())
# }


# x <- counting.hs(1,6,10000,1.5, hi_opt2)
