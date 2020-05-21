# Iskanje optimalne strategije za hit & stand, hard hand

source("funkcije.r")#karte, vsota_kart, strategija dealerja
source ("osnovne-strategije/str_igralec.r")
source("osnovne-strategije/osnovna-sim.r")

############################################

paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

#tabela optimalne strategije
hit.stand <- data.frame(matrix(NA, nrow = 19, ncol = 10))
colnames(hit.stand) <- c(2:10, "A")
rownames(hit.stand) <- c(3:21)

# če je vstota >= 19 ne bomo vzeli nove karte (neodvisno od odkrite karte dealerja).
hit.stand[c("19","20","21"),] <- "S"
hit.stand[c("3"),] <- "H" 
# hit.stand[c("7","8","9","10","11"),] <- "H"
# hit.stand[c("12"),c("2","3","7","8","9","10","A")] <- "H"
# hit.stand[c("12"),c("4","5","6")] <- "S"
# hit.stand[c("17","18"),] <- "S"
# hit.stand[c("13","14","15","16"),c("2","3","4","5","6")] <- "S"
# hit.stand[c("13","14","15","16"),c("7","8","9","10","A")] <- "H"

stolpci <- colnames(hit.stand)
vrstice <- c(18:4)

n <- 10000 #stevilo iteracij


# simulacija iger
hit.stand <- osnovna("hit_stand", "hard")

#save(hit.stand,file="hit-stand-hard.Rda")
