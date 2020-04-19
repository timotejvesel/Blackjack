library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(plotly)

source("house-edge/he-double-counting.r")
source("house-edge/he-double.r")
source("house-edge/he-hit-stand-counting.r")
source("house-edge/he-hit-stand.r")
source("house-edge/he-slaba.R")

narisi.graf.d <- function(stava, paketi, iter, natural, stetje) {
  
  counting <- counting.double(stava, paketi, iter, natural, stetje)
  counting <- counting[[2]]
  
  opt <- he.double(stava, iter, paketi, natural)
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
  
  counting <- counting.hs(stava, paketi, iter, natural, stetje)
  counting <- counting[[2]]
  
  opt <- he.hs(stava, iter, paketi, natural)
  opt <- opt[[2]]
  
  slaba <- he.slaba(stava, iter, meja, paketi, natural)
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