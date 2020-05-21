### V pravila dodan double
### Izračun optimalne strategije (hit, stand & double; dealer stands on soft 17)


source("funkcije.r") #karte, vsota_kart, strategija dealerja
source("osnovne-strategije/str_igralec.r")
source("osnovne-strategije/osnovna-sim.r")

############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)

#tabela optimalne strategije
double <- data.frame(matrix(NA, nrow = 19, ncol = 10))
colnames(double) <- c(2:10, "A")
rownames(double) <- c(3:21)

# če je vstota >= 19 ocitno ne bomo vzeli nove karte (neodvisno od odkrite karte dealerja).
double[c("19","20","21"),] <- "S"
double[c("3","4"),] <- "H" 

n <- 10000 #stevilo iteracij

stolpci <- colnames(double)
vrstice <- c(18:5)

# simulacija iger
double <- osnovna("double", "hard")


# save(double, file="double-hard.Rda")


