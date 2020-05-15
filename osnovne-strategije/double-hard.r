### V pravila dodan double
### Izračun optimalne strategije (hit, stand & double; dealer stands on soft 17)


source("funkcije.r") #karte, vsota_kart, strategija dealerja
source("osnovne-strategije/str_igralec.r")

############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

#tabela optimalne strategije
double <- data.frame(matrix(NA, nrow = 19, ncol = 10))
colnames(double) <- c(2:10, "A")
rownames(double) <- c(3:21)

# če je vstota >= 19 ocitno ne bomo vzeli nove karte (neodvisno od odkrite karte dealerja).
double[c("19","20","21"),] <- "S"
double[c("3","4"),] <- "H" 

n <- 100000 #stevilo iteracij

stolpci <- colnames(double)
vrstice <- c(18:5)


for (j in vrstice) {
  vs <- sum(j)
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
          zmaga.hit <- zmaga.hit + igra.d(igr_roka, d_roka, strategija, "hard")
        }
        else if (strategija == "stand") {
          zmaga.stand <- zmaga.stand + igra.d(igr_roka, d_roka, strategija, "hard")
        }
        else {
          zmaga.double <- zmaga.double + igra.d(igr_roka, d_roka, strategija, "hard")
        }
      }
    }
    print(zmaga.hit)
    print(zmaga.stand)
    print(zmaga.double)
    v <- c(zmaga.hit, zmaga.stand, zmaga.double)
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


# save(double, file="double-hard.Rda")


