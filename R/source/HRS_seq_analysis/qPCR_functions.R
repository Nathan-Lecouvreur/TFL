# Imports

require(dplyr)
require(ggplot2)
require(tidyverse)
require(data.table)
require(openxlsx)
library(readxl)
require(magrittr)
library(plotrix)
library(ie2misc)
library(readr)

# base variables (potentially transform into arguments)

HRS_tag <- "H"

loop_tag <- "B"

folder_standard <- "Z:/Nathan/Code/TFL/R/data/HRS/standard"

# Importing the datasets
# Put all the standard curves in a folder and the HRS results in another one

# standard files



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

# folder_HRS <- "Z:/Nathan/Code/TFL/R/data/HRS/HRS"
# 
# df_HRS_tot <-
#    list.files(path = folder_HRS, 
#               pattern = NULL) %>% 
#    map_df(~read.csv(paste(folder_HRS,
#                           ., 
#                           sep = "/"),
#                     colClasses = c("Target" = "character"),
#                     sep = ";",
#                     header = TRUE))


# Load Libraries


# Set Working Directory

path_values <- "Z:/Nathan/Code/TFL/R/data/HRS/HRS"


# Get List of Files in Directory
files <- list.files("Z:/Nathan/Code/TFL/R/data/HRS/HRS", pattern = ".csv")

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
