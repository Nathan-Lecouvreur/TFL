# Initialisation

library(ggplot2)
library(readxl)
library(dplyr)
library(ggpubr)
library(lattice)

# TEMP : création d'un DF test

data <- data.frame(matrix(c(c(15, 12, 28, 13, 25, 54),
                            c(5000, 8000, 10000, 24000, 32000, 60000),
                            c(4, 2, 3, 1, 4, 8)),
                          nrow = 6,
                          ncol = 3))


# Importation des données du fichier excel

data <- read_excel(
   path = "Z:/Nathan/Resultats/3C-HRS-Valentin/HRS-TAD-2-3/HRS-IMR90-TAD2-3_resultats_COPIE.xls",
   sheet = 4,
   range = "AE7:AF49",
   col_names = FALSE)

colnames(data) <- c("value", "std")

names <- read_excel(
   path = "Z:/Nathan/Resultats/3C-HRS-Valentin/HRS-TAD-2-3/HRS-IMR90-TAD2-3_resultats_COPIE.xls",
   sheet = 4,
   range = "A7:A49",
   col_names = FALSE)


data$name <- names$...1

data <- na.omit(data)


# Importation des sites enzymatiques sty

sites_cur <- read_excel(
   path = "Z:/Nathan/Resultats/3C-HRS-Valentin/HRS-TAD-2-3/Annotations-Enz-Rest_hg38.xls",
   sheet = 2,
   range = "J4:K335",
   col_names = FALSE)

colnames(sites_cur) <- c("start", "end")

sites_cur <- na.omit(sites_cur)

# Importation des sites pour les breaks mineurs en x

sites <- read_excel(
   path = "Z:/Nathan/Resultats/3C-HRS-Valentin/HRS-TAD-2-3/Annotations-Enz-Rest_hg38.xls",
   sheet = 2,
   range = "E4:E330",
   col_names = FALSE)

colnames(sites) <- "sty"


# Ajout de la colonne des positions


data$position <- (((sites_cur$end - sites_cur$start)
                   /2)
                  + sites_cur$start)

data$sty <- sites_cur$start


# Génération de la figure

plot <- ggplot(data, aes(x = position, y = value), xlim(c(max(sites$sty), min(sites$sty)))) +
   geom_col() +
   geom_errorbar(aes(ymin = value - std,
                     ymax = value + std),
                 width = .5) +
   scale_x_continuous(limits = c(min(sites$sty), max(sites$sty)))


plot

# graph sites sty


gg_sites <- ggplot(sites, aes(x=sty, y=0)) +
   geom_point(size = 10, shape = 3)  +
   annotate("segment",x=min(sites$sty),xend=max(sites$sty), y=0, yend=0, size=0.5) +
   # annotate("segment",x=1,xend=min(sites$sty), y=-0.1,yend=0.1, size=2) +
   # annotate("segment",x=max(sites$sty),xend=max(sites$sty), y=-0.1,yend=0.1, size=2) +
   scale_x_continuous(limits = c(min(sites$sty),max(sites$sty))) +
   scale_y_continuous(limits = c(-1,1)) +
   scale_color_manual(values = unname(colours)) +
   theme(panel.background = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank())

gg_sites






