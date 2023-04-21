   

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
   require(purrr)

   # Standard curves data import
   
   df_standard <- 
      list.files(path = folder_standard,
                 pattern = NULL) %>% 
      map_df(~read_excel(paste(folder_standard, 
                               .,
                               sep = "/")))
   
   df_standard$a <- as.numeric(df_standard$a)
   df_standard$b <- as.numeric(df_standard$b)
   
   colnames(df_standard) <- c("Target_standard", "Efficiency %", "a", "b", "R^2")
   
   
   
   # HRS files have to be CSV since there is some rights issues while 
   # importing them as a xlsx file (_rel files not found)
   
   
   setwd(path_values)
   
   # Get List of Files in Directory
   files <- list.files(path = path_values, pattern = ".csv")
   
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
      select(Well, Target, Sample, Cq)
   
   
   df_HRS_tot$Cq <- as.numeric(df_HRS_tot$Cq)
   
   df_HRS_tot <- df_HRS_tot[!is.na(df_HRS_tot$Cq),]
   
   
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
   df_HRS_select <- 
      df_value %>% 
      separate(Sample,
               into = c("ID", "number"),
               sep = "(?<=[A-Za-z])(?=[0-9])",
               remove = FALSE)

   
   return(df_HRS_select)
}



################################################################################
#                            qPCR_HRS                                          #
################################################################################


qPCR_HRS <- function(df_values,
                     HRS_tag,
                     loop_tag,
                     path_output = NULL){

   # Tags the HRS and loops
   
   colnames(df_values) <- c("Target", 
                           "Sample", 
                           "fraction",
                           "number", 
                           "value")
   
   
   df_ratio <- 
      df_values %>% 
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
#                                qPCR RD HRS                                   #
################################################################################
qPCR_RD_HRS <- function(df_import
                        )

################################################################################
#                            qPCR 3C processing                                #
################################################################################

qPCR_3C <- function(df_import,
                    norm_target,
                    path_sizes,
                    normalisation_control_factor = 1,
                    moke_tag = "K",
                    test_tag = "T"){
   
   
   
   
   
   
   colnames(df_import) <- c("Target", 
                            "Sample", 
                            "condition",
                            "number", 
                            "value")
   
   
   
   df_3C_norm1 <- 
      df_import %>%
      group_by(Sample) %>% 
      mutate(value_norm = (value 
                           / value[Target == norm_target])) %>% 
      mutate(log_value = log(value_norm, 
                             base = 10))

   
   df_selection_test <- 
      df_3C_norm1 %>% 
      group_by(Target, condition) %>%
      filter(log_value < (mean(log_value) 
                          + (sd(log_value)
                             * normalisation_control_factor)) &
             log_value > (mean(log_value) 
                          - (sd(log_value) 
                             * normalisation_control_factor)))
   
   
   df_selection <- 
      df_selection_test %>%       
      group_by(Target, condition) %>%
      summarise(mean_pre = mean(value_norm),
                sd_pre = madstat(value_norm))

   
   df_positions <- 
      read_excel(path_positions) %>% 
      mutate(distance = (REFseq_pos 
                        - min(REFseq_pos)))
   
   colnames(df_positions) <- c("REFseq_pos", "Frag", "distance")
   
   df_positions$Frag <- as.character(df_positions$Frag)
   
   
   df_selection_pos <- 
      df_selection %>% 
      group_by(Target) %>% 
      mutate(distance = with(df_positions, distance[Frag == Target[1]]))

   
   # Selecting values under the mean then over the mean after selection
   
   df_background <- 
      df_selection_pos %>%
      group_by(condition) %>%
      mutate(selec_1 = mean(mean_pre)
             - mean(sd_pre)) %>% 
      filter(mean_pre < (mean(mean_pre)
                              - mean(sd_pre))) %>% 
      filter(mean_pre > (mean(mean_pre)
                         - madstat(mean_pre))) %>% 
      filter(distance <= min(df_selection_pos %>%
                                group_by(condition) %>% 
                                summarise(max_value = max(distance)) %>% 
                                pull(max_value) %>% 
                                min())) %>% 
      group_by(condition) %>% 
      summarise(norm_factor = mean(mean_pre),
                sd_baseline = madstat(mean_pre))

   colnames(df_background) <- c("condition_background", "norm_factor", "sd_pre")
   
   
   df_normalised <- 
      df_selection_pos %>% 
      mutate(norm_value = (mean_pre /
                           with(df_background, 
                                norm_factor[condition_background == condition])),
             sd_norm = (sd_pre /
                        with(df_background, 
                             norm_factor[condition_background == condition])))
   
   
   plot_3C <- ggplot(data = df_normalised,
                     mapping = aes(x = distance,
                                   y = norm_value,
                                   color = condition,
                                   ymin = norm_value - sd_norm,
                                   ymax = norm_value + sd_norm)) +
              geom_point() +
              geom_errorbar() +
              geom_hline(yintercept = 1)

   
   
   df_test <- 
      df_selection_test %>% 
      group_by(Target) %>% 
      mutate(pval = t.test(x = value_norm[condition == moke_tag],
                           y = value_norm[condition == test_tag])$p.value)
      filter(pval < 0.05)
   plot_3C_test
   
   out_3C_processing <- c(plot_3C, df_normalised)
   
   
   
   
   
   return(list(plot = plot_3C,
               dataframe = df_normalised))

}

HRS_tag <- "H"

loop_tag <- "L"

folder_standard <- "Z:/Nathan/Code/TFL/R/data/HRS/standard"

path_values <- "Z:/Nathan/Code/TFL/R/data/HRS/HRS"



df_import <- qPCR_import(folder_standard,
                         path_values)


HRS_output <- qPCR_HRS(df_import,
                       HRS_tag,
                       loop_tag)


HRS_output

# 
# folder_standard <- "Z:/Nathan/Code/TFL/R/data/3CqPCR/standard"
# 
# path_values <- "Z:/Nathan/Code/TFL/R/data/3CqPCR/values"


# 
# 
# 
# ggplot(data = filter(df_3C_norm1,
#                      condition == "K"),
#        mapping = aes(value_norm)) +
#    geom_histogram(bins = 50) 


# norm_target <- "K19"
# 
# moke_tag <- "K"
# 
# test_tag <- "T"
# 
# normalisation_control_factor <- 1
# 
# path_positions <- "Z:/Nathan/Code/TFL/R/data/3CqPCR/positions/positions.xlsx"
# 
# 
# 
# norm_3C <- qPCR_3C(df_import,
#                    norm_target,
#                    path_postions,
#                    normalisation_control_factor = 1)
# 
# norm_3C$plot
# 
# 
# df_3C <- norm_3C$dataframe
# 
# 
# shap <- shapiro.test(df_3C_norm1$value_norm)
# 
# df_3C_norm1 <- filter(df_3C_norm1, Target != "K19")
# 
# df_shap <- df_3C_norm1 %>% group_by(condition, Target) %>% summarise(pval = shapiro.test(value_norm)$p.value)
# 
# df_shap_normal <-
#    df_shap %>% 
#    filter(pval > 0.05)


# test <- 
#    norm_3C %>% 
#    separate(Sample,
#             into = c("ID", "number"),
#             sep = "(?<=[A-Za-z])(?=[0-9])")
#    
# 
# df_3C_norm1 <- 
#    df_import %>% 
#    mutate(value = value / value[Target == norm_target])

