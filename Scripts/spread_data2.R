# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Data Input -------------------------------------------------------------

data.in <- read_excel("~/Documents/GitHub/MJ_Proficiency/Data/MJ Data.xlsx", 
                      sheet = "Raw Data (2)")

data.in <- data.in[,c(3:10)]
# Data Cleaning ----------------------------------------------------------

data.in$Group1 <- paste(data.in$ANALYSIS,data.in$REPORTED_NAME, data.in$Sample, sep="_")

data.in2 <- data.in %>% 
        group_by(Group1) %>% 
        mutate(Item = seq_along(Group1)) %>% 
        unite(Test, Set,REPL)

data.in3 <- as.data.frame(data.in2)

data.in3 <- data.in3[,c(8,9,4,1,6,7)]


wide_data <- spread(data.in3, Item, ENTRY)

#  write.csv(wide_data, "temppy.csv")

# wide_data
