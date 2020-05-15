source("funkcije.r")

# mozni parametri
stava <- 1
iter <- 100000
paketi <- 4
natural <- 1.5

# brez stetja
he.bs(stava,iter,meja, paketi, natural, "slaba", NULL, meja)[[1]]
he.bs(stava,iter, paketi, natural, "double")[[1]]
he.bs(stava,iter, paketi, natural, "hit_stand")[[1]]

# s stetjem
counting(stava, paketi, iter, natural, hi_opt2, "hit_stand")[[1]]
counting(stava, paketi, iter, natural, hi_opt2, "double")[[1]]