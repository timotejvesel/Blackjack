source("house-edge/he-slaba.R")
source("house-edge/he-hit-stand.r")
source("house-edge/he-double.r")

he_slaba <- paste(abs(he_slaba) * 100,"%", sep="")
he_hs <- paste(abs(he_hs) * 100,"%", sep="")
he_double <- paste(abs(he_double) * 100,"%", sep="")


cat(c("House edge, če igralec do vsote kart 15 jemlje nove karte:", he_slaba, "\n",
      "House edge, če igralec igra optimalno in sta možnosti le hit & stand:", he_hs, "\n",
      "House edge, če igralec igra optimalno in ima še možnost double:", he_double, "\n"))
