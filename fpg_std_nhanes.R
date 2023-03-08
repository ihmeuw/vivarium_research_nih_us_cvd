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

##### Find FPG Standard Deviation by Age/Sex Group at Draw Level##### 
## Creating a dataframe with the age/sexes listed 
std_fpg <- data.frame(matrix(ncol=3,nrow=0))
colnames(std_fpg) <- c('sex_id','age_start','age_end')
counter = 1
for (i in 1:12){
  for (j in 1:2){
    age_low = 25 + (5*(i-1))
    age_high = age_low + 5 
    std_fpg[counter,1] = j
    std_fpg[counter,2] = age_low 
    std_fpg[counter,3] = age_high
    counter = counter+1
  }}
std_fpg

#Now making the draws 
#I use 75% of the length of the age/sex group as my sample rate 

std_fpg_temp_big <- data.frame(matrix(ncol=1000,nrow=0))

for (i in 1:12){
  for (j in 1:2){
    age_low = 25 + (5*(i-1))
    age_high = age_low + 5 
    data_current <- data[(data$age_year>age_low) & (data$age_year<age_high) & (data$sex_id==j) & (!is.na(data$fpg))]
    std_fpg_temp <- data.frame(matrix(ncol=1000,nrow=1))
    for (draw in 1:1000){
      data_boot = data_current[sample(nrow(data_current), (nrow(data_current)*0.75), replace=TRUE), ]
      fpg_std = sd(data_boot$fpg, na.rm=TRUE)
      std_fpg_temp[1,draw] = fpg_std
    }
    std_fpg_temp_big = rbind(std_fpg_temp_big, std_fpg_temp) 
  }
}

std_fpg = cbind(std_fpg, std_fpg_temp_big)

std_fpg

write.csv(std_fpg, 'H:/fpg_std_nhanes_draw_level.csv')

## Below this is an alternative approach 
## I originally subsetted from the whole dataset, but the distribution of 
## age/sex groups are not even. Therefore, I chose to subset to age/sex 
## Specific group first and sample second. 

std_fpg <- data.frame(matrix(ncol=3,nrow=0))
colnames(std_fpg) <- c('sex_id','age_start','age_end')
counter = 1
for (i in 1:12){
  for (j in 1:2){
    age_low = 25 + (5*(i-1))
    age_high = age_low + 5 
    std_fpg[counter,1] = j
    std_fpg[counter,2] = age_low 
    std_fpg[counter,3] = age_high
    counter = counter+1
  }}
std_fpg

for (draw in 1:10){
  data_boot = data[sample(nrow(data), 10000, replace=TRUE), ]
  std_fpg_temp <- data.frame(matrix(ncol=1,nrow=0))
  counter = 1
  for (i in 1:12){
    for (j in 1:2){
      colnames(std_fpg_temp) <- c(draw)
      age_low = 25 + (5*(i-1))
      age_high = age_low + 5 
      data_current <- data_boot[(data_boot$age_year>age_low) & (data_boot$age_year<age_high) & (data_boot$sex_id==j)]
      fpg_std = sd(data_current$fpg, na.rm=TRUE)
      std_fpg_temp[counter,1] = fpg_std
      counter = counter+1
    }
  }
  std_fpg = cbind(std_fpg, std_fpg_temp) 
}

std_fpg





