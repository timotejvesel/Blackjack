### Optimalna strategija za hit, stand & double (dealer stands on soft 17)
### Za soft hand

source("funkcije.r") #karte, vsota_kart, strategija dealerja
source("osnovne-strategije/str_igralec.r")
source("osnovne-strategije/osnovna-sim.r")

############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

#tabela optimalne strategije
double_soft <- data.frame(matrix(NA, nrow = 9, ncol = 10))
colnames(double_soft) <- c(2:10, "A")
rownames(double_soft) <- c(13:21)
double_soft[c("21"),] <- "S" # ce je vsota 21 (as + 10), ne bomo vzeli nove karte
vrstice <- list(c("A",9),c("A",8),c("A",7),c("A",6),c("A",5),c("A",4),c("A",3),c("A",2))
stolpci <- colnames(double_soft)

n <- 100
#stevilo iteracij

# simulacija iger
double_soft <- osnovna("double", "soft")

#save(double_soft,file="double-soft.Rda")


