# Iskanje optimalne strategije za hit & stand, soft hand

source("funkcije.r")
source ("osnovne-strategije/str_igralec.r")
source("osnovne-strategije/osnovna-sim.r")

############################################


paket_kart <- rep(c(2:10, 10, 10, 10, "A"), 4)
n <- 100000 #stevilo iteracij

soft1 <- data.frame(matrix(NA, nrow = 9, ncol = 10))
colnames(soft1) <- c(2:10, "A")
rownames(soft1) <- c(13:21)
soft1[c("21","20"),] <- "S" # ce je vsota 21 (as + 10), ne bomo vzeli nove karte

vrstice <- list(c("A",8),c("A",7),c("A",6),c("A",5),c("A",4),c("A",3),c("A",2))
stolpci <- colnames(soft1)

# simulacija iger
soft1 <- osnovna("hit_stand", "soft")

#save(soft1,file="hit-stand-soft.Rda")
