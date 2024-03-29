   

################################################################################
#                            qPCR_import                                       #
################################################################################

qPCR_import <- function(folder_standard, 
                        path_values){


   # Imports

   require(dplyr)
   require(ggplot2)
   require(tidyverse)
   require(data.table)
   require(openxlsx)
   require(readxl)
   require(magrittr)
   require(plotrix)
   require(ie2misc)
   require(readr)
   
   # base variables (potentially transform into arguments)
   
   # HRS_tag <- "H"
   # 
   # loop_tag <- "B"
   # 
   # folder_standard <- "Z:/Nathan/Code/TFL/R/data/HRS/standard"
   # 
   # path_values <- "Z:/Nathan/Code/TFL/R/data/HRS/HRS"
   
   
   
   ################################################################################
   #                      Standard Curves processing                              #
   ################################################################################
   
   
   df_standard <- 
      list.files(path = folder_standard,
                 pattern = NULL) %>% 
      map_df(~read_excel(paste(folder_standard, 
                               .,
                               sep = "/")))
   
   df_standard$a <- as.numeric(df_standard$a)
   df_standard$b <- as.numeric(df_standard$b)
   
   df_standard <- na.omit(df_standard)
   
   colnames(df_standard) <- c("Target_standard", "Efficiency %", "a", "b", "R^2")
   
   
   ################################################################################
   #                          HRS data processing                                 #
   ################################################################################
   
   # HRS files, they have to be CSV since there is some rights issues while 
   # importing them as a xlsx file (_rel files not found)
   
   
   # Get List of Files in Directory
   files <- list.files(path_values, pattern = ".csv")
   
   # Import All Files
   for(i in files){
      assign(x = gsub(".csv","",i),
             value = read_delim(paste0(i)))
   }
   
   # Bind Tables into One Table
   df_HRS_tot <- do.call(rbind,mget(ls(pattern = "^[^files]*$")))
   
   
   df_HRS_tot <- 
      df_HRS_tot %>% 
      filter(Content %like% "Unkn") %>% 
      select(Well, Target, Sample, Cq) %>% 
      na.omit()
   
   
   df_HRS_tot$Cq <- as.numeric(df_HRS_tot$Cq)
   
   df_HRS_tot <- na.omit(df_HRS_tot)
   
   
   df_HRS_tot<- 
      df_HRS_tot%>%
      group_by(Target, Sample) %>%
      mutate(mean = mean(Cq), 
             sd = sd(Cq), 
             var = var(Cq),
             n = n())
   
   
   
   df_HRS_tot <- mutate(df_HRS_tot, 
                        low_lim = mean - (sd * 1.05),
                        high_lim = mean + (sd * 1.05))
   
   
   
   df_HRS_select <- filter(df_HRS_tot,
                           between(Cq,
                                   low_lim, 
                                   high_lim) |
                              (n < 3)) 
   
   df_HRS_select <- 
      df_HRS_select %>% 
      group_by(Target, Sample) %>% 
      mutate(mean = mean(Cq), 
             sd = sd(Cq), 
             var = var(Cq))
   
   
   
   df_value <-
      df_HRS_select %>%
      group_by(Target, Sample) %>%
      summarise(value = 10^((mean(mean) 
                             - with(df_standard,
                                    b[Target_standard == Target[1]]))
                            / -abs(with(df_standard,
                                        a[Target_standard == Target[1]]))))
   
   
   list_replicates <- split(df_value,
                            df_value$Sample)
   
   df_HRS_select <- data.table()
   
   for(replicate in list_replicates){
      replicate <- 
         replicate %>% 
         mutate(number = gsub("[^0-9]",
                              "", 
                              Sample))
      
      df_HRS_select <- bind_rows(replicate, df_HRS_select)
   }
   
   return(df_HRS_select)
}



################################################################################
#                            qPCR_HRS                                          #
################################################################################


qPCR_HRS <- function(df_values,
                     HRS_tag,
                     loop_tag){

   # Tags the HRS and loops
   
   df_loop <-
      df_values %>%
      filter(Sample %like% loop_tag) %>%
      mutate(fraction = loop_tag)
   
   
   df_HRS <-
      df_values %>%
      filter(Sample %like% HRS_tag) %>%
      mutate(fraction = HRS_tag)
   
   
   df_HRS_tagged <- bind_rows(df_HRS,
                              df_loop)
   
   
   
   df_ratio <- 
      df_HRS_tagged %>% 
      group_by(Target, number) %>% 
      summarise(ratio = mean(value[fraction == HRS_tag])
                / mean(value[fraction == loop_tag]),
                value_HRS = mean(value[fraction == HRS_tag]),
                value_loop = mean(value[fraction == loop_tag])) %>% 
      na.omit()
   
   

   # Defines the normalisation factor from the data
   
   df_ratio$Target_num <- as.numeric(df_ratio$Target) 
   
   df_ratio <- arrange(df_ratio, Target_num)
   
   write_excel_csv(df_ratio, "Z:/Nathan/Code/TFL/R/data/ratio_3.csv")
   
   filter_factor <- 0
   
   df_selected_1 <-
      df_ratio %>% 
      group_by(number) %>% 
      mutate(mean = mean(ratio),
             sd = sd(ratio))
   
   df_selected_2 <- 
      df_selected_1 %>% 
      group_by(number, Target) %>% 
      filter(ratio < mean[1] - (sd[1] * filter_factor))
   
   df_selected_2 <- 
      df_selected_2 %>% 
      group_by(number) %>% 
      mutate(mean = mean(ratio),
             sd = sd(ratio))
   
   df_selected_3 <- 
      df_selected_2 %>% 
      group_by(number, Target) %>% 
      filter(ratio < mean[1] - (sd[1] * filter_factor))
   
   df_norm <- 
      df_selected_3 %>%
      group_by(number) %>% 
      summarise(mean = mean(ratio),
                sd = std.error(ratio)) %>% 
      mutate(sd = sd/mean)
   
   colnames(df_norm) <- c("number_norm", "mean_norm", "sd_norm")
   
   
   df_normalised <- 
      df_ratio %>% 
      group_by(number) %>% 
      mutate(norm_ratio = (ratio 
                           / with(df_norm, 
                                  mean_norm[number_norm == number[1]])))
   
   
   df_final <- 
      df_normalised %>% 
      group_by(Target) %>% 
      summarise(mean = mean(norm_ratio),
                sd = std.error(norm_ratio))
   
   
   
   df_final <- arrange(df_final,
                       as.numeric(Target))
   
   # Classical plot generation
   
   barplot_HRS_norm <- ggplot(data = df_final,
                              mapping = aes(y = mean,
                                            x = factor(as.numeric(Target)),
                                            ymin = mean - sd,
                                            ymax = mean + sd)) +
      geom_col() +
      geom_errorbar() +
      geom_hline(yintercept = 1,
                 linetype = 2) +
      geom_hline(yintercept = 1 - mean(df_norm$sd_norm)) +
      geom_hline(yintercept = 1 + mean(df_norm$sd_norm)) +
      ylab("Relative Enrichment Level") +
      xlab("fragments")
   
   return(list(dataframe = df_final,
               plot = barplot_HRS_norm))
   
}


################################################################################
#                            qPCR_HRS_plot                                     #
################################################################################


qPCR_HRS_plot <- function(df_final){
   

}



HRS_tag <- "H"

loop_tag <- "B"

folder_standard <- "Z:/Nathan/Code/TFL/R/data/HRS/standard"

path_values <- "Z:/Nathan/Code/TFL/R/data/HRS/HRS"


df_test <- qPCR_import(folder_standard,
            path_values)


HRS_output <- qPCR_HRS(df_test,
                         HRS_tag,
                         loop_tag)

HRS_output[2]




