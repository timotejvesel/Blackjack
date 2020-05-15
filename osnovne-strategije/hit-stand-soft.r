# Iskanje optimalne strategije za hit & stand, soft hand

source("funkcije.r")
source ("osnovne-strategije/str_igralec.r")


############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)
n <- 10000 #stevilo iteracij

soft1 <- data.frame(matrix(NA, nrow = 9, ncol = 10))
colnames(soft1) <- c(2:10, "A")
rownames(soft1) <- c(13:21)
soft1[c("21","20"),] <- "S" # ce je vsota 21 (as + 10), ne bomo vzeli nove karte
vrstice <- list(c("A",8),c("A",7),c("A",6),c("A",5),c("A",4),c("A",3),c("A",2))
stolpci <- colnames(soft1)


for (j in vrstice) {
  print(j)
  vs <- 11 + as.numeric(j[2]) 
  igr_roka <- j #določimo karti za igralca (pomembna je vsota)
  for (k in stolpci) {
    zmaga.hit <- 0
    zmaga.stand <- 0
    for (z in c(TRUE, FALSE)) {
      hit = z
      for (iter in 1:n) {
        
        igr_roka <- j #karte igralca
        d_roka <- k #določimo odkrito karto dealerja
        if (d_roka != "A") {
          d_roka <- as.numeric(d_roka)
        }
        
        d_roka <- c(d_roka, sample(paket_kart, 1)) # zakrita karta dealerja
        #print(d_roka)
        
        if (hit == TRUE) {
          zmaga.hit <- zmaga.hit + igra.hs(igr_roka, d_roka, hit, "soft")
        }
        else {
          zmaga.stand <- zmaga.stand + igra.hs(igr_roka, d_roka, hit, "soft")
        }
      }
    }
    cat(paste(c("hit:",zmaga.hit,"stand",zmaga.stand, "\n"), sep=""))
    if (zmaga.hit > zmaga.stand) {
      soft1[as.character(vs), as.character(k)] <- "H"
    }
    else {
      soft1[as.character(vs), as.character(k)] <- "S"
    }
  }
}

#save(soft1,file="hit-stand-soft.Rda")
