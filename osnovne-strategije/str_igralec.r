#######################################################################################
#######################################################################################

# strategija igralca za iskanje optimalne strategije za hit & stand; hard ter soft hand
# ter sama igra (stetje zasluzka/izgube)

#######################################################################################
#######################################################################################

igralec_str_hs <- function(igr_roka, hit, tip) {
  
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
    
    # ce gledamo za soft hand
    if (tip == "soft") {
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
    }
    
    else if (tip == "hard") {
      element <- hit.stand[vrstica, stolpec]
    }
    
    # to je isto pri obeh tipih
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

###################################################
# igra

igra.hs <- function(igr_roka, d_roka, hit, tip) {
  zmage <- 0
  
  # strategija za igralca
  igralec <- igralec_str_hs(igr_roka,hit, tip)
  
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

#######################################################################################
#######################################################################################

# strategija igralca za iskanje optimalne strategije za double; hard ter soft hand
# ter sama igra (stetje zasluzka/izgube)

#######################################################################################
#######################################################################################

igralec_str_d <- function(igr_roka, strategija, stava, tip) {
  stevec <- 0
  d_odkrita <- d_roka[1]
  stand <- FALSE
  trenutna_vsota <- vsota_karte(igr_roka)
  if (strategija == "stand") { # če je strategija stand, ne vzamemo nobene karte.
    #cat(paste(c("Igralčeva roka (stand): ", igr_roka, "\n"), collapse=" "))
    return(c(trenutna_vsota, stava))
  }
  
  if (strategija == "double") { # če je strategija stand, moramo vzeti natanko 1 novo karto, poleg tega se podvoji še stava
    igr_roka <- c(igr_roka, sample(paket_kart,1))
    trenutna_vsota <- vsota_karte(igr_roka)
    nova_stava <- 2 * stava
    return(c(trenutna_vsota, nova_stava))
  }
  
  # ce je strategija hit, vsaj enkrat vzamemo novo karto
  while (stand != TRUE) {
    
    if (tip == "hard") {
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
    
    else if (tip == "soft") {
      soft <- FALSE
      #cat(paste(c("Igralčeva roka (hit): ", igr_roka, "\n"), collapse=" "))
      trenutna_vsota <- vsota_karte(igr_roka)
      vrstica <- as.character(trenutna_vsota)
      stolpec <- as.character(d_odkrita)
      if ("AS" %in% igr_roka) {
        indeks <- match("A",igr_roka)
        vsota_brez <- vsota_karte(igr_roka[-indeks])
        if (vsota_brez == trenutna_vsota - 11) { # v roki je as vreden 11 -> soft hand
          element <- double_soft[vrstica, stolpec]
          element[is.na(element)] <- 0
          soft <- TRUE
        }
        else {
          element <- double[vrstica, stolpec]
          element[is.na(element)] <- 0
        }
      }
      else {
        element <- double[vrstica, stolpec]
        element[is.na(element)] <- 0
      }
      if (stevec == 0) { # če je strategija == hit, 1. vedno vzamemo novo karto
        igr_roka <- c(igr_roka, sample(paket_kart,1))
        stevec <- stevec + 1
      }
      else if (element == "H") {
        igr_roka <- c(igr_roka, sample(paket_kart,1))
      }
      else if (element == "D" & soft == FALSE) { #ker lahko podvojimo samo na začetku, moramo sicer pogledati v tabelo samo za hit & stand
        element <- hit.stand[vrstica, stolpec]
        if (element == "H") {
          igr_roka <- c(igr_roka, sample(paket_kart,1))
        }
        else if (element == "S") {
          stand <- TRUE
        }
      }
      else if (element == "D" & soft == TRUE) { #ker lahko podvojimo samo na začetku, moramo sicer pogledati v tabelo samo za hit & stand
        element <- soft1[vrstica, stolpec]
        if (element == "H") {
          igr_roka <- c(igr_roka, sample(paket_kart,1))
        }
        else if (element == "S") {
          stand <- TRUE
        }
      }
      else {
        stand <- TRUE
      }
    }
  }
  return(c(trenutna_vsota, stava))
}


################################################
# igra

igra.d <- function(igr_roka, d_roka, strategija, tip) {
  zmage <- 0
  stava <- 1
  # strategija za igralca
  igralec <- igralec_str_d(igr_roka,strategija, stava, tip)
  
  # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
  if (igralec[1] <= 21) {
    dealer <- dealer_str_opt(d_roka)
  }
  stava <- igralec[2]
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

