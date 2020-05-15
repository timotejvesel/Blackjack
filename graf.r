library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

source("funkcije.r")

narisi.graf.d <- function(stava, paketi, iter, natural, stetje) {
  
  counting <- counting(stava, paketi, iter, natural, stetje, "double")
  counting <- counting[[2]]
  
  opt <- he.bs(stava, iter, paketi, natural, "double")
  opt <- opt[[2]]
  
  x <- counting$stevilo
  y1 <- counting$denar
  y2 <- opt$denar
  
  df <- data.frame(x, "Stetje" = y1, "Optimalna" = y2)
  
  df <- df %>% gather(-x, key = "tip", value = "zasluzek_izguba")
  
  theme_update(plot.title = element_text(hjust = 0.5))
  barve <- brewer.pal(5, "Set1")

  g <- ggplot(data = df) + aes(x = x, y = zasluzek_izguba, text = paste(
    "Stevilo iger: ", x, "\n",
    "Zasluzek/izguba igralca ", zasluzek_izguba, "\n",
    "Strategija: ", tip, sep = ""), colour = tip, group = tip) +
    geom_line() +
    ggtitle("Zasluzek oz. izguba igralca") + labs (x = "Stevilo iger", y = "Kolicina denarja")  + 
    scale_color_manual(labels = c("Stetje kart", "Optimalna strategija"), values = barve) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(size = 15, face = "bold"),
          legend.position = c(0.9, 1), legend.title = element_blank()) 
  
  g <- ggplotly(g, tooltip = "text")
  return(g)
}

narisi.graf.hs <- function(stava, paketi, iter, natural, stetje, meja) {
  
  counting <- counting(stava, paketi, iter, natural, stetje, "double")
  counting <- counting[[2]]
  
  opt <- he(stava, iter, paketi, natural, "hit_stand")
  opt <- opt[[2]]
  
  slaba <- he.slaba(stava, iter, meja, paketi, natural, "slaba", NULL, meja)
  slaba <- slaba[[2]]
  
  x <- counting$stevilo
  y1 <- counting$denar
  y2 <- opt$denar
  y3 <- slaba$denar
  
  df <- data.frame(x, "Stetje" = y1, "Optimalna" = y2, "Slaba" = y3)
  
  df <- df %>% gather(-x, key = "tip", value = "zasluzek_izguba")
  
  theme_update(plot.title = element_text(hjust = 0.5))
  barve <- brewer.pal(5, "Set1")
  
  g <- ggplot(data = df) + aes(x = x, y = zasluzek_izguba, text = paste(
    "Stevilo iger: ", x, "\n",
    "Zasluzek/izguba igralca ", zasluzek_izguba, "\n",
    "Strategija: ", tip, sep = ""), colour = tip, group = tip) +
    geom_line() +
    ggtitle("Zasluzek oz. izguba igralca") + labs (x = "Stevilo iger", y = "Kolicina denarja")  + 
    scale_color_manual(labels = c("Stetje kart", "Optimalna strategija"), values = barve) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(size = 15, face = "bold"),
          legend.position = c(0.9, 1), legend.title = element_blank()) 
  
  g <- ggplotly(g, tooltip = "text")
  return(g)
}