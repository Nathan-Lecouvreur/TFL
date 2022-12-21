gg_spider <- function(data, colour = TRUE, same_height = TRUE) {
  
   library(ggplot2)
   library(ggforce)
   library(colourvalues)
   library(dplyr)
   
   plot <- ggplot()
   
   data <- my_dat  
   
   colnames(data) <- c("position", "val", "sem")
  
   data$trans <- log(data$val + 1)
  
   data$colour <- color_values(data$trans, 
                              palette = "viridis")
  
   data$sign <- sign(data$position)
  
   data_split <- group_split(data, data$sign)
   
   data_neg <- data.frame(data_split[[1]])
   data_pos <- data.frame(data_split[[2]])
  
   data_neg <- data_neg[order(data_neg$position, decreasing = TRUE),]
   data_pos <- data_pos[order(data_pos$position, decreasing = FALSE),]
  
   data_neg$rank <- seq.int(nrow(data_neg))
   data_pos$rank <- seq.int(nrow(data_pos))
  
   data_neg$y <- ((data_neg$rank * abs(data_neg$position)) 
                      / (nrow(data_neg)))
   data_pos$y <- ((data_pos$rank * abs(data_pos$position)) 
                      / (nrow(data_pos)))
  
   if (same_height == TRUE) {
     
      if (max(data_pos$y) > max(data_neg$y)){
        
        max_neg <- max(data_neg$y)
        
        data_neg$y <- ((max(data_pos$y) 
                         * data_neg$y)
                        / max_neg)
      } else {
        
        max_pos <- max(data_pos$y)
        
        data_pos$y <- ((max(data_neg$y) 
                         * data_pos$y)
                        / max_pos)
      }
   }

   
   
   if (nrow(data_neg) != 0){
      
      
      for(i in 1:nrow(data_neg)){
         
         len <- nrow(data_neg)
         
         j <- len - i + 1
         
         row <- data_neg[j,]
         
         
         pos <- row$pos
         val <- row$trans
         col <- row$colour
         y   <- row$y
         
         beziers <- data.frame(x = c(0, pos/2, pos),
                               y = c(0, y, 0))
         
         if (colour == TRUE){
         
            plot <- plot +
               geom_bezier(aes(x = x, 
                               y = y),
                           data = beziers,
                           size = val,
                           colour = col)
         } else {
            plot <- plot +
               geom_bezier(aes(x = x, 
                               y = y),
                           data = beziers,
                           size = val)
         }         
      }  
   }

   
   if (nrow(data_pos) != 0){
      
      
      for(i in 1:nrow(data_pos)){
         
         len <- nrow(data_pos)
         
         j <- len - i + 1
         
         row <- data_pos[j,]
         
         
         pos <- row$pos
         val <- row$trans
         col <- row$colour
         y   <- row$y
         
         beziers <- data.frame(x = c(0, pos/2, pos),
                               y = c(0, y, 0))
         
         if (colour == TRUE){
            
            plot <- plot +
               geom_bezier(aes(x = x, 
                               y = y),
                           data = beziers,
                           size = val,
                           colour = col)
         } else {
            plot <- plot +
               geom_bezier(aes(x = x, 
                               y = y),
                           data = beziers,
                           size = val)
         }         
      }  
   }
   
   plot
   
   return(plot)  
}