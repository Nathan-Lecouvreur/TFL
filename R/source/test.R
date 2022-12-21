library(readxl)
library(dplyr)
library(colourvalues)

# Importing the excel file containing the 3C data

# Insert the location of the excel file in the next line

my_dat <- read_excel(
  path = "Z:/Nathan/Resultats/3C-Valentin/3C-DATA-v12_TAD 2-3_résultats Valentin_COPIE.xlsx", 
  sheet = 1, 
  range = "H14:J64", 
  col_names = TRUE)

my_dat <- na.omit(my_dat)


colnames(my_dat) <- c("neg", "val", "sem")


neg_pos_list <- lapply(my_dat$neg, sign)

my_dat$sign <- neg_pos_list


my_dat$extrapol <- log(my_dat$val+1)

my_dat$colors <- color_values(my_dat$extrapol, 
                              palette = "viridis")


# rr <- range(my_dat$extrapol)
# my_dat$range <- (my_dat$extrapol-rr[1])/diff(rr)
# 
# f <- colorRamp(c("lightblue", "blue"))
# my_dat$colors <- rgb(f(my_dat$range)/255)


my_dat <- group_split(my_dat, my_dat$sign)

my_dat_neg <- data.frame(my_dat[[1]])

my_dat_pos <- data.frame(my_dat[[2]])


my_dat_neg <- my_dat_neg[order(my_dat_neg$neg, decreasing = TRUE),]

my_dat_pos <- my_dat_pos[order(my_dat_pos$neg, decreasing = FALSE),]

library(ggplot2)
library(ggforce)


plot <- ggplot()



if (nrow(my_dat_neg) != 0){

  
  for(i in 1:nrow(my_dat_neg)){
  
    # setting up the bezier curve poitnts and the data points
    # x = c(xend1, xcontrol, xend2)
    # y = c(yend1, ycontrol, yend2)
    # type = rep*3 "cubic" or "quadratic"
    # point = c('end', 'control', 'end')
    # colours = c(end1_colour, control_colour, end2_colour )
    
    len <- nrow(my_dat_neg)
    
    j <- len - i + 1
    
    row <- my_dat_neg[j,]
    
    
    neg <- row$neg
    val <- row$extrapol
    sem <- row$sem
    col <- row$colors
    
    beziers <- data.frame(
      x = c(0, neg/2, neg),
      y = c(0, (j*0.2*abs(neg))/len, 0))
      #y = c(0, abs(neg), 0))
    
    
    plot <- plot +
      geom_bezier(aes(x = x, 
                      y = y),
                  data = beziers,
                  size = val,
                  #colour = col)
      )
      
  }  
}


if (nrow(my_dat_pos) != 0){
  
  
  for(i in 1:nrow(my_dat_pos)){
    
    # setting up the bezier curve poitnts and the data points
    # x = c(xend1, xcontrol, xend2)
    # y = c(yend1, ycontrol, yend2)
    # type = rep*3 "cubic" or "quadratic"
    # point = c('end', 'control', 'end')
    # colours = c(end1_colour, control_colour, end2_colour )
  
    
    len <- nrow(my_dat_pos)
    
    j <- len - i + 1
      
    row <- my_dat_pos[j,]
    

    
    neg <- row$neg
    val <- row$extrapol
    sem <- row$sem
    col <- row$colors
    
    
    #formule calcul point y de la courbe : 
    # (rang * |x|) / n_tot_curves
    # tests : (j*abs(neg))/len
    
    
    beziers <- data.frame(
      x = c(0, neg/2, neg),
      y = c(0, (j*0.2*abs(neg))/len, 0))
      #y = c(0, abs(neg), 0))
      
    
    
    plot <- plot +
      geom_bezier(aes(x = x, 
                      y = y),
                  data = beziers,
                  size = val,
                  #colour = col)
      )
      
    
  }  
}

plot <- plot +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank()) +
        xlab("Genomic region") +
    scale_color_identity(guide = "legend")

plot
