#######################################################################################
#######################################################################################

# strategija igralca za iskanje optimalne strategije za hit & stand; hard ter soft hand

#######################################################################################
#######################################################################################

igralec_str_hs <- function(igr_roka, d_roka, strategija,pravila, tip) {
  
  stevec <- 0
  d_odkrita <- d_roka[1]
  stand <- FALSE
  trenutna_vsota <- vsota_karte(igr_roka)
  
  if (strategija == "stand") {
    return(trenutna_vsota)
  }
  
  while (stand != TRUE) {
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

#######################################################################################
#######################################################################################

# strategija igralca za iskanje optimalne strategije za double; hard ter soft hand


#######################################################################################
#######################################################################################

igralec_str_d <- function(igr_roka, d_roka, strategija, stava,pravila, tip) {
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

#######################################################################################
#######################################################################################

# strategija igralca za iskanje optimalne strategije za obe razlicici pravil;
# hard ter soft hand
# zdruzeni zgornji strategiji

#######################################################################################
#######################################################################################

igralec_str <- function(igr_roka, d_roka, strategija, stava, pravila, tip) {
  
  stevec <- 0
  d_odkrita <- d_roka[1]
  stand <- FALSE
  trenutna_vsota <- vsota_karte(igr_roka)
  
  if (strategija == "stand") { # če je strategija stand, ne vzamemo nobene karte.
    return(c(trenutna_vsota, stava))
  }
  
  # pri pravilu hit_stand do tega ne bo nikoli prislo
  if (strategija == "double") { # če je strategija stand, moramo vzeti natanko 1 novo karto, poleg tega se podvoji še stava
    igr_roka <- c(igr_roka, sample(paket_kart,1))
    trenutna_vsota <- vsota_karte(igr_roka)
    nova_stava <- 2 * stava
    return(c(trenutna_vsota, nova_stava))
  }
  
  # ce je strategija hit, vsaj enkrat vzamemo novo karto
  while (stand != TRUE) {
    trenutna_vsota <- vsota_karte(igr_roka)
    vrstica <- as.character(trenutna_vsota)
    stolpec <- as.character(d_odkrita)
    
    soft <- FALSE
    if (tip == "soft") {
      
      # v roki je as
      if ("AS" %in% igr_roka) {
        indeks <- match("A",igr_roka)
        vsota_brez <- vsota_karte(igr_roka[-indeks])
        
        # v roki je as vreden 11 -> soft hand
        if (vsota_brez == trenutna_vsota - 11) { 
          
          if (pravila == "double") {
            element <- double_soft[vrstica, stolpec]
          }
          else if (pravila == "hit_stand") {
            element <- soft1[vrstica, stolpec]
          }
          soft <- TRUE
        }
        
        else {
          if (pravila == "double") {
            element <- double[vrstica, stolpec]
          }
          else if (pravila == "hit_stand") {
            element <- hit.stand[vrstica, stolpec]
          }
        }
        
      }
      
      # v roki ni asa
      else {
        if (pravila == "double") {
          element <- double[vrstica, stolpec]
        }
        else if (pravila == "hit_stand") {
          element <- hit.stand[vrstica, stolpec]
        }
      }
    }
      
      else if (tip == "hard") {
        if (pravila == "double") {
          element <- double[vrstica, stolpec]
        }
        else if (pravila == "hit_stand") {
          element <- hit.stand[vrstica, stolpec]
        }
      }
      
      # to je isto pri obeh tipih
      element[is.na(element)] <- 0
      
      if (stevec == 0) { # če je strategija == hit, 1. vedno vzamemo novo karto
        igr_roka <- c(igr_roka, sample(paket_kart,1))
        stevec <- stevec + 1
      }
      else if (element == "H") {
        igr_roka <- c(igr_roka, sample(paket_kart,1))
      }
      
      # pri pravilu hit_stand se to ne bo nikoli zgodilo
      # ce po hit dobimo vsoto za strategijo double, lahko le hitamo ali pa ne vzammeo nove karte
      else if (element == "D" & soft == FALSE) { #ker lahko podvojimo samo na začetku, moramo sicer pogledati v tabelo samo za hit & stand
        element <- hit.stand[vrstica, stolpec]
        if (element == "H") {
          igr_roka <- c(igr_roka, sample(paket_kart,1))
        }
        else if (element == "S") {
          stand <- TRUE
        }
      }
      
      # pri pravilu hit_stand se to ne bo nikoli zgodilo
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
    
  return(c(trenutna_vsota, stava))
}



#######################################################################################
#######################################################################################

# IGRA (za obe razlici)

#######################################################################################
#######################################################################################


igra <- function(igr_roka, d_roka, strategija, pravila, tip) {
  zmage <- 0
  stava <- 1
  # strategija za igralca
  if (pravila == "double") {
    igralec <- igralec_str(igr_roka, d_roka, strategija, stava, pravila, tip)
  }
  else if (pravila == "hit_stand") {
    igralec <- igralec_str(igr_roka, d_roka, strategija, stava, pravila, tip)
  }
  
  # če gre igralec preko 21, v vsakem primeru izgubi, tudi če gre dealer preko 21, zato dealer niti ne igra več.
  if (igralec[1] <= 21) {
    dealer <- dealer_str_opt(d_roka)
  }
  
  # ce igramo igro z double moramo pogledati, ce se je stava spremenila
  # pri hit_stand je stava vedno zacetna (tj. 1)
  
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

