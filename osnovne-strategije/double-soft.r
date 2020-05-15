### Optimalna strategija za hit, stand & double (dealer stands on soft 17)
### Za soft hand

source("funkcije.r") #karte, vsota_kart, strategija dealerja
source("osnovne-strategije/str_igralec.r")

############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

#tabela optimalne strategije
double_soft <- data.frame(matrix(NA, nrow = 9, ncol = 10))
colnames(double_soft) <- c(2:10, "A")
rownames(double_soft) <- c(13:21)
double_soft[c("21"),] <- "S" # ce je vsota 21 (as + 10), ne bomo vzeli nove karte
vrstice <- list(c("A",9),c("A",8),c("A",7),c("A",6),c("A",5),c("A",4),c("A",3),c("A",2))
stolpci <- colnames(double_soft)

n <- 100000
#stevilo iteracij

for (j in vrstice) {
  vs <- 11 + as.numeric(j[2]) 
  igr_roka <- j #določimo karti za igralca (pomembna je vsota)
  print(j)
  for (k in stolpci) {
    zmaga.hit <- 0
    zmaga.stand <- 0
    zmaga.double <- 0
    for (z in c("hit", "stand", "double")) {
      strategija <- z
      for (iter in 1:n) {
        
        igr_roka <- j #karte igralca
        d_roka <- k #določimo odkrito karto dealerja
        if (d_roka != "A") {
          d_roka <- as.numeric(d_roka)
        }
        
        d_roka <- c(d_roka, sample(paket_kart, 1)) # zakrita karta dealerja
        #print(d_roka)
        
        if (strategija == "hit") {
          zmaga.hit <- zmaga.hit + igra.d(igr_roka, d_roka, strategija, "soft")
        }
        else if (strategija == "stand") {
          zmaga.stand <- zmaga.stand + igra.d(igr_roka, d_roka, strategija, "soft")
        }
        else {
          zmaga.double <- zmaga.double + igra.d(igr_roka, d_roka, strategija, "soft")
        }
      }
    }
    cat(paste(c(k,zmaga.hit,zmaga.stand,zmaga.double,"\n")))
    v <- c(zmaga.hit, zmaga.stand, zmaga.double)
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
}

#save(double_soft,file="double-soft.Rda")


