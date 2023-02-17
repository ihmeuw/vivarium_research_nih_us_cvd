###### Setup ######
rm(list=ls())

#install.packages("nnet")
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("rpart.plot")
#install.packages("ggpubr")
suppressMessages(library(data.table))
library(ggplot2)
library(nnet)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggpubr)

###### Files and paths ######
file_path <- "H:/simulation_science/nhanes/"

###### Read in file ######
load(paste0(file_path, "nhanes_microdata.rdata"))
data$fpg <- data$lbxglu / 18.0182

##### Find FPG Standard Deviation by Age/Sex Group ##### 
std_fpg <- data.frame(matrix(ncol=5,nrow=0))
colnames(std_fpg) <- c('sex_id','age_start','age_end','fpg_std','count_nonmiss')
counter = 1

for (i in 1:13){
  for (j in 1:2){
    age_low = 25 + (5*(i-1))
    age_high = age_low + 5 
    data_current <- data[(data$age_year>age_low) & (data$age_year<age_high) & (data$sex_id==j)]
    fpg_std = sd(data_current$fpg, na.rm=TRUE)
    fpg_count = sum(!is.na(data_current$fpg))
    std_fpg[counter,1] = j
    std_fpg[counter,2] = age_low 
    std_fpg[counter,3] = age_high
    std_fpg[counter,4] = fpg_std
    std_fpg[counter,5] = fpg_count
    print(counter)
    counter = counter+1
    }
    
  }


std_fpg

write.csv(std_fpg, 'H:/fpg_std_nhanes.csv')



