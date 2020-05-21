#### simulacije za osnovno strategijo (pravila double ter hit & stand)

osnovna <- function(pravila, tip) {
  
  for (j in vrstice) {
    if (tip == "soft") {
      vs <- 11 + as.numeric(j[2]) 
    }
    else if (tip == "hard") {
      vs <- sum(j)
    }
    
    igr_roka <- j #določimo karti za igralca (pomembna je vsota)
    print(j)
    for (k in stolpci) {
      zmaga.hit <- 0
      zmaga.stand <- 0
      zmaga.double <- 0 # pri hit_stand tega ne bomo potrebovali
      
      if (pravila == "double") {
        v <- c("hit", "stand", "double")
      }
      else if (pravila == "hit_stand") {
        v <- c("hit", "stand")
      }
      
      for (z in v) {
        strategija <- z
        
        for (iter in 1:n) {
          
          igr_roka <- j #karte igralca
          
          d_roka <- k #določimo odkrito karto dealerja
          if (d_roka != "A") {
            d_roka <- as.numeric(d_roka)
          }
          
          d_roka <- c(d_roka, sample(paket_kart, 1)) # zakrita karta dealerja
          
          
          if (strategija == "hit") {
            zmaga.hit <- zmaga.hit + igra(igr_roka, d_roka, strategija, pravila, tip)
          }
          else if (strategija == "stand") {
            zmaga.stand <- zmaga.stand + igra(igr_roka, d_roka, strategija, pravila, tip)
          }
          else {
            zmaga.double <- zmaga.double + igra(igr_roka, d_roka, strategija, pravila, tip)
          }
        }
      }
      
      if (pravila == "double") {
        cat(paste(c(k,zmaga.hit,zmaga.stand,zmaga.double,"\n")))
        v <- c(zmaga.hit, zmaga.stand, zmaga.double)
        
        if (tip == "soft") {
          if (max(v) == get("zmaga.hit")) {
            double_soft[as.character(vs), as.character(k)] <- "H"
          }
          else if (max(v) == get("zmaga.stand")) {
            double_soft[as.character(vs), as.character(k)] <- "S"
          }
          else {
            double_soft[as.character(vs), as.character(k)] <- "D"
          }
        }
        
        else if (tip == "hard") {
          if (max(v) == get("zmaga.hit")) {
            double[as.character(vs), as.character(k)] <- "H"
          }
          else if (max(v) == get("zmaga.stand")) {
            double[as.character(vs), as.character(k)] <- "S"
          }
          else {
            double[as.character(vs), as.character(k)] <- "D"
          }
        }
      }
      
      else if (pravila == "hit_stand") {
        cat(paste(c("hit:",zmaga.hit,"stand",zmaga.stand, "\n"), sep=""))
        
        if (tip == "soft") {
          if (zmaga.hit > zmaga.stand) {
            soft1[as.character(vs), as.character(k)] <- "H"
          }
          else {
            soft1[as.character(vs), as.character(k)] <- "S"
          }
        }
        else if (tip == "hard") {
          if (zmaga.hit > zmaga.stand) {
            hit.stand[as.character(vs), as.character(k)] <- "H"
          }
          else {
            hit.stand[as.character(vs), as.character(k)] <- "S"
          }
        }
      }
      
    }
  }
  if (pravila == "double") {
    if (tip == "soft") {
      return(double_soft)
    }
    else if (tip == "hard") {
      return(double)
    }
  }
  else if (pravila == "hit_stand") {
    if (tip == "soft") {
      return(soft1)
    }
    else if (tip == "hard") {
      return(hit.stand)
    }
  }
}

