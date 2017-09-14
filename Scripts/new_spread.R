# Clean Up environment ---------------------------------------------------
#rm(list=ls())

# Packages ---------------------------------------------------------------
library(tidyverse)
library(readxl)

# Data Input -------------------------------------------------------------

raw_data <- read_excel("~/Documents/GitHub/MJ_Proficiency/Data/MJ_4.xlsx")

# Data Cleaning ----------------------------------------------------------

trimmed_data <- raw_data[,c(1,3,4,5,6,7,8,14,17)]
trimmed_data$Sample <- substr(trimmed_data$DESCRIPTION, 16,17)


trimmed_data <- trimmed_data %>% 
        filter(STATUS != "X") %>% 
        filter(STATUS != "R")

# Visualising Data -------------------------------------------------------

trimmed_data$Group1 <- paste(trimmed_data$ANALYSIS,trimmed_data$REPORTED_NAME, trimmed_data$Sample, sep="_")

trimmed_data_1 <- trimmed_data %>% 
        filter(Sample == "S1")
Sets <- unique(trimmed_data_1$SAMPLE_NUMBER)
trimmed_data_1$set <- 2
n <- nrow(trimmed_data_1)
for (i in 1:n){
        if(trimmed_data_1$SAMPLE_NUMBER[i] == Sets[1]) {
        trimmed_data_1$set[i] = 1
        } else {
        trimmed_data_1$set[i] = 2
        }
}

trimmed_data_2 <- trimmed_data %>% 
        filter(Sample == "S2")
Sets <- unique(trimmed_data_2$SAMPLE_NUMBER)
trimmed_data_2$set <- 2
n <- nrow(trimmed_data_2)
for (i in 1:n){
        if(trimmed_data_2$SAMPLE_NUMBER[i] == Sets[1]) {
                trimmed_data_2$set[i] = 1
        } else {
                trimmed_data_2$set[i] = 2
        }
}

trimmed_data_3 <- trimmed_data %>% 
        filter(Sample == "S3")
Sets <- unique(trimmed_data_3$SAMPLE_NUMBER)
trimmed_data_3$set <- 2
n <- nrow(trimmed_data_3)
for (i in 1:n){
        if(trimmed_data_3$SAMPLE_NUMBER[i] == Sets[1]) {
                trimmed_data_3$set[i] = 1
        } else {
                trimmed_data_3$set[i] = 2
        }
}

new_trimmed_data <- rbind(trimmed_data_1, trimmed_data_2, trimmed_data_3)

trimmed_data2 <- new_trimmed_data %>% 
        group_by(Group1) %>% 
        mutate(Item = seq_along(Group1)) %>% 
        unite(Test, set,REPLICATE_COUNT)

trimmed_data3 <- as.data.frame(trimmed_data2)

trimmed_data3 <- trimmed_data3[,c(11,12,3,10,4,5,6)]


wide_data <- spread(trimmed_data3, Item, ENTRY)

colnames(wide_data)[6] <- "A1"
colnames(wide_data)[7] <- "A2"
colnames(wide_data)[8] <- "B1"
colnames(wide_data)[9] <- "B2"


wide_data$A1 = sapply(wide_data$A1, as.numeric)
wide_data$A2 = sapply(wide_data$A2, as.numeric)
wide_data$B1 = sapply(wide_data$B1, as.numeric)
wide_data$B2 = sapply(wide_data$B2, as.numeric)


wide_data <- select(wide_data, -1) 

wide_data 