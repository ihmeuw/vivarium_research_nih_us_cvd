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

colnames(data)
#bpsys - SBP values 
#lbdldl - LDL-C values 
#bmi - based on measurements of height/weight 
#lbxglu - FPG values 

#This is testing for correlation for the first time by age and includes p-values
data_final_2 <- data.frame(matrix(ncol=14,nrow=0))
colnames(data_final_2) <- c('age_start','age_end','sbp_ldl','sbp_ldl_pval','sbp_bmi','sbp_bmi_pval','ldl_bmi','ldl_bmi_pval','sbp_fpg','sbp_fpg_pval','ldl_fpg','ldl_fpg_pval','bmi_fpg','bmi_fpg_pval')
counter = 1

for (i in 1:13){
  age_low = 25 + (5*(i-1))
  age_high = age_low + 5 
  data_current <- data[(data$age_year>age_low) & (data$age_year<age_high)]
  sbp_ldl_spea = cor.test(data_current$bpsys, data_current$lbdldl, method = c("spearman"))
  sbp_bmi_spea = cor.test(data_current$bpsys, data_current$bmi, method = c("spearman"))
  ldl_bmi_spea = cor.test(data_current$lbdldl, data_current$bmi, method = c("spearman"))
  sbp_fpg_spea = cor.test(data_current$bpsys, data_current$lbxglu, method = c("spearman"))
  ldl_fpg_spea = cor.test(data_current$lbdldl, data_current$lbxglu, method = c("spearman"))
  bmi_fpg_spea = cor.test(data_current$bmi, data_current$lbxglu, method = c("spearman"))
  data_final_2[counter,1] = age_low 
  data_final_2[counter,2] = age_high
  data_final_2[counter,3] = sbp_ldl_spea$estimate
  data_final_2[counter,4] = sbp_ldl_spea$p.value
  data_final_2[counter,5] = sbp_bmi_spea$estimate
  data_final_2[counter,6] = sbp_bmi_spea$p.value
  data_final_2[counter,7] = ldl_bmi_spea$estimate
  data_final_2[counter,8] = ldl_bmi_spea$p.value
  data_final_2[counter,9] = sbp_fpg_spea$estimate
  data_final_2[counter,10] = sbp_fpg_spea$p.value
  data_final_2[counter,11] = ldl_fpg_spea$estimate
  data_final_2[counter,12] = ldl_fpg_spea$p.value
  data_final_2[counter,13] = bmi_fpg_spea$estimate
  data_final_2[counter,14] = bmi_fpg_spea$p.value
  print(counter)
  counter = counter+1
}

data_final_2

#Plots for Greg 
par( mfrow= c(3,2) )

plot(data_final_2$age_start, data_final_2$sbp_bmi, main='SBP and BMI Correlation') 
loessMod10 <- loess(sbp_bmi ~ age_start, data=data_final_2)
j <- order(data_final_2$age_start)
lines(data_final_2$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_2$age_start, data_final_2$sbp_ldl, main='SBP and LDL-C Correlation') 
loessMod10 <- loess(sbp_ldl ~ age_start, data=data_final_2)
j <- order(data_final_2$age_start)
lines(data_final_2$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_2$age_start, data_final_2$ldl_bmi, main='LDL-C and BMI Correlation') 
loessMod10 <- loess(ldl_bmi ~ age_start, data=data_final_2)
j <- order(data_final_2$age_start)
lines(data_final_2$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_2$age_start, data_final_2$sbp_fpg, main='SBP and FPG Correlation') 
loessMod10 <- loess(sbp_fpg ~ age_start, data=data_final_2)
j <- order(data_final_2$age_start)
lines(data_final_2$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_2$age_start, data_final_2$ldl_fpg, main='LDL-C and FPG Correlation') 
loessMod10 <- loess(ldl_fpg ~ age_start, data=data_final_2)
j <- order(data_final_2$age_start)
lines(data_final_2$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_2$age_start, data_final_2$bmi_fpg, main='FPG and BMI Correlation') 
loessMod10 <- loess(bmi_fpg ~ age_start, data=data_final_2)
j <- order(data_final_2$age_start)
lines(data_final_2$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

#Next, we remove folks on treatment. This is since those on treatment 
#might have a low SBP or LDL-C and throw off correlation 

data[,sbptx:=ifelse(highbp==0 & is.na(bpmeds), 0, bpmeds)]
data[,choltx:=ifelse(highchol==0 & is.na(cholmeds), 0, cholmeds)]
data[,tx:=ifelse(sbptx==0 & choltx==0, "none", ifelse(sbptx==1 & choltx==0, "bponly",
                                                      ifelse(sbptx==0 & choltx==1, "cholonly", ifelse(sbptx==1 & choltx==1, "both", NA))))]
data[,tx2:=factor(tx, levels=c("none", "bponly", "cholonly", "both"))]
data2 <- data[data$tx2 == 'none']

data_final_3 <- data.frame(matrix(ncol=14,nrow=0))
colnames(data_final_3) <- c('age_start','age_end','sbp_ldl','sbp_ldl_pval','sbp_bmi','sbp_bmi_pval','ldl_bmi','ldl_bmi_pval','sbp_fpg','sbp_fpg_pval','ldl_fpg','ldl_fpg_pval','bmi_fpg','bmi_fpg_pval')
counter = 1

for (i in 1:13){
  age_low = 25 + (5*(i-1))
  age_high = age_low + 5 
  data_current <- data2[(data2$age_year>age_low) & (data2$age_year<age_high)]
  sbp_ldl_spea = cor.test(data_current$bpsys, data_current$lbdldl, method = c("spearman"))
  sbp_bmi_spea = cor.test(data_current$bpsys, data_current$bmi, method = c("spearman"))
  ldl_bmi_spea = cor.test(data_current$lbdldl, data_current$bmi, method = c("spearman"))
  sbp_fpg_spea = cor.test(data_current$bpsys, data_current$lbxglu, method = c("spearman"))
  ldl_fpg_spea = cor.test(data_current$lbdldl, data_current$lbxglu, method = c("spearman"))
  bmi_fpg_spea = cor.test(data_current$bmi, data_current$lbxglu, method = c("spearman"))
  data_final_3[counter,1] = age_low 
  data_final_3[counter,2] = age_high
  data_final_3[counter,3] = sbp_ldl_spea$estimate
  data_final_3[counter,4] = sbp_ldl_spea$p.value
  data_final_3[counter,5] = sbp_bmi_spea$estimate
  data_final_3[counter,6] = sbp_bmi_spea$p.value
  data_final_3[counter,7] = ldl_bmi_spea$estimate
  data_final_3[counter,8] = ldl_bmi_spea$p.value
  data_final_3[counter,9] = sbp_fpg_spea$estimate
  data_final_3[counter,10] = sbp_fpg_spea$p.value
  data_final_3[counter,11] = ldl_fpg_spea$estimate
  data_final_3[counter,12] = ldl_fpg_spea$p.value
  data_final_3[counter,13] = bmi_fpg_spea$estimate
  data_final_3[counter,14] = bmi_fpg_spea$p.value
  print(counter)
  counter = counter+1
}

data_final_3

plot(data_final_3$age_start, data_final_3$sbp_ldl) #All over the show 
plot(data_final_3$age_start, data_final_3$sbp_bmi) #High for young people, 0 for 70+
plot(data_final_3$age_start, data_final_3$ldl_bmi) #High for young, low by 50 
plot(data_final_3$age_start, data_final_3$sbp_fpg) #Slower decrease 
plot(data_final_3$age_start, data_final_3$ldl_fpg) #High in young people, 0 by 70
plot(data_final_3$age_start, data_final_3$bmi_fpg) #Consistent for all ages 

#Plots for Greg 
par( mfrow= c(3,2) )

plot(data_final_3$age_start, data_final_3$sbp_bmi, main='SBP and BMI Correlation') 
loessMod10 <- loess(sbp_bmi ~ age_start, data=data_final_3)
j <- order(data_final_3$age_start)
lines(data_final_3$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_3$age_start, data_final_3$sbp_ldl, main='SBP and LDL-C Correlation') 
loessMod10 <- loess(sbp_ldl ~ age_start, data=data_final_3)
j <- order(data_final_3$age_start)
lines(data_final_3$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_3$age_start, data_final_3$ldl_bmi, main='LDL-C and BMI Correlation') 
loessMod10 <- loess(ldl_bmi ~ age_start, data=data_final_3)
j <- order(data_final_3$age_start)
lines(data_final_3$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_3$age_start, data_final_3$sbp_fpg, main='SBP and FPG Correlation') 
loessMod10 <- loess(sbp_fpg ~ age_start, data=data_final_3)
j <- order(data_final_3$age_start)
lines(data_final_3$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_3$age_start, data_final_3$ldl_fpg, main='LDL-C and FPG Correlation') 
loessMod10 <- loess(ldl_fpg ~ age_start, data=data_final_3)
j <- order(data_final_3$age_start)
lines(data_final_3$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

plot(data_final_3$age_start, data_final_3$bmi_fpg, main='FPG and BMI Correlation') 
loessMod10 <- loess(bmi_fpg ~ age_start, data=data_final_3)
j <- order(data_final_3$age_start)
lines(data_final_3$age_start[j],loessMod10$fitted[j],col="red",lwd=3)
abline(h=0)

#Based on a conversation with Greg, we agreed on the following: 
#SBP and BMI correlation will be done to include age specific correlation 
#SBP/LDL-C, SBP/FPG, LDL-C/BMI, and FPG/BMI will NOT include age specific 
#correlation and instead will just be an overall value for everyone 
#LDL-C/FPG correlation will be set to 1 

#The below loop is adjusted for that new decision 

data_final_5 <- data.frame(matrix(ncol=14,nrow=0))
colnames(data_final_5) <- c('age_start','age_end','sbp_ldl','sbp_ldl_pval','sbp_bmi','sbp_bmi_pval','ldl_bmi','ldl_bmi_pval','sbp_fpg','sbp_fpg_pval','ldl_fpg','ldl_fpg_pval','bmi_fpg','bmi_fpg_pval')
counter = 1

for (i in 1:13){
  age_low = 25 + (5*(i-1))
  age_high = age_low + 5 
  data_current <- data2[(data2$age_year>age_low) & (data2$age_year<age_high)]
  sbp_ldl_spea = cor.test(data2$bpsys, data2$lbdldl, method = c("spearman"))
  sbp_bmi_spea = cor.test(data_current$bpsys, data_current$bmi, method = c("spearman"))
  ldl_bmi_spea = cor.test(data2$lbdldl, data2$bmi, method = c("spearman"))
  sbp_fpg_spea = cor.test(data2$bpsys, data2$lbxglu, method = c("spearman"))
  ldl_fpg_spea = 0
  bmi_fpg_spea = cor.test(data2$bmi, data2$lbxglu, method = c("spearman"))
  data_final_5[counter,1] = age_low 
  data_final_5[counter,2] = age_high
  data_final_5[counter,3] = sbp_ldl_spea$estimate
  data_final_5[counter,4] = sbp_ldl_spea$p.value
  data_final_5[counter,5] = sbp_bmi_spea$estimate
  data_final_5[counter,6] = sbp_bmi_spea$p.value
  data_final_5[counter,7] = ldl_bmi_spea$estimate
  data_final_5[counter,8] = ldl_bmi_spea$p.value
  data_final_5[counter,9] = sbp_fpg_spea$estimate
  data_final_5[counter,10] = sbp_fpg_spea$p.value
  data_final_5[counter,11] = 0
  data_final_5[counter,12] = 'N/A'
  data_final_5[counter,13] = bmi_fpg_spea$estimate
  data_final_5[counter,14] = bmi_fpg_spea$p.value
  print(counter)
  counter = counter+1
}

data_final_5

#Looks like the loop worked as expected! Now the last step is to reset 
#the negatives in SBP/BMI to 0 as these are not statistically significant 
#and the negatives are likely just due to noisy data 

data_final_5[data_final_5<0] <- 0

write.csv(data_final_5, 'H:/correlation.csv')

#The below code is for a by year analysis that did not show any significant changes 
#for different years 

data <- data[,year2:=factor(svyyear, labels=c(1,2,3,4,5,6,7,8,9,10,11,12))]
data2 <- data[(data$tx2 == 'none') | (data$tx2 == 'bponly')]
data2 <- data[(data$tx2 == 'none')]

data_final_4 <- data.frame(matrix(ncol=16,nrow=0))
colnames(data_final_4) <- c('year','sex_id','age_start','age_end','sbp_ldl','sbp_ldl_pval','sbp_bmi','sbp_bmi_pval','ldl_bmi','ldl_bmi_pval','sbp_fpg','sbp_fpg_pval','ldl_fpg','ldl_fpg_pval','bmi_fpg','bmi_fpg_pval')
counter = 1

for (n in 1:12){
  for (j in 1:2){
    for (i in 1:10){
      age_low = 25 + (5*(i-1))
      age_high = age_low + 5 
      data_current <- data2[(data2$age_year>age_low) & (data2$age_year<age_high) & (data2$sex_id==j) & (data2$year2==n)]
      sbp_ldl_spea = cor.test(data_current$bpsys, data_current$lbdldl, method = c("spearman"))
      sbp_bmi_spea = cor.test(data_current$bpsys, data_current$bmi, method = c("spearman"))
      ldl_bmi_spea = cor.test(data_current$lbdldl, data_current$bmi, method = c("spearman"))
      sbp_fpg_spea = cor.test(data_current$bpsys, data_current$lbxglu, method = c("spearman"))
      ldl_fpg_spea = cor.test(data_current$lbdldl, data_current$lbxglu, method = c("spearman"))
      bmi_fpg_spea = cor.test(data_current$bmi, data_current$lbxglu, method = c("spearman"))
      data_final_4[counter,1] = n
      data_final_4[counter,2] = j
      data_final_4[counter,3] = age_low 
      data_final_4[counter,4] = age_high
      data_final_4[counter,5] = sbp_ldl_spea$estimate
      data_final_4[counter,6] = sbp_ldl_spea$p.value
      data_final_4[counter,7] = sbp_bmi_spea$estimate
      data_final_4[counter,8] = sbp_bmi_spea$p.value
      data_final_4[counter,9] = ldl_bmi_spea$estimate
      data_final_4[counter,10] = ldl_bmi_spea$p.value
      data_final_4[counter,11] = sbp_fpg_spea$estimate
      data_final_4[counter,12] = sbp_fpg_spea$p.value
      data_final_4[counter,13] = ldl_fpg_spea$estimate
      data_final_4[counter,14] = ldl_fpg_spea$p.value
      data_final_4[counter,15] = bmi_fpg_spea$estimate
      data_final_4[counter,16] = bmi_fpg_spea$p.value
      print(counter)
      counter = counter+1
    }
    
  }
  
}


data_final_4

data_final_4_old <- data_final_4[data_final_4$year==1,]
data_final_4_new <- data_final_4[data_final_4$year==12,]

plot(data_final_4_old$age_start, data_final_4_old$sbp_bmi, main='Survey 1988-1991') 
loessMod10 <- loess(sbp_bmi ~ age_start, data=data_final_4_old)
j <- order(data_final_4_old$age_start)
lines(data_final_4_old$age_start[j],loessMod10$fitted[j],col="red",lwd=3)

plot(data_final_4_new$age_start, data_final_4_new$sbp_bmi, main='Survey 2017-2018') 
loessMod10 <- loess(sbp_bmi ~ age_start, data=data_final_4_new)
j <- order(data_final_4_new$age_start)
lines(data_final_4_new$age_start[j],loessMod10$fitted[j],col="red",lwd=3)

plot(data_final_4_old$age_start, data_final_4_old$sbp_ldl, main='Survey 1988-1991') 
loessMod10 <- loess(sbp_ldl ~ age_start, data=data_final_4_old)
j <- order(data_final_4_old$age_start)
lines(data_final_4_old$age_start[j],loessMod10$fitted[j],col="red",lwd=3)

plot(data_final_4_new$age_start, data_final_4_new$sbp_ldl, main='Survey 2017-2018') 
loessMod10 <- loess(sbp_ldl ~ age_start, data=data_final_4_new)
j <- order(data_final_4_new$age_start)
lines(data_final_4_new$age_start[j],loessMod10$fitted[j],col="red",lwd=3)

plot(data_final_4$year, data_final_4$sbp_ldl) #All over the show 
plot(data_final_4$year, data_final_4$sbp_bmi) #High for young people, 0 for 70+
plot(data_final_4$year, data_final_4$ldl_bmi) #High for young, low by 50 
plot(data_final_4$year, data_final_4$sbp_fpg) #Slower decrease 
plot(data_final_4$year, data_final_4$ldl_fpg) #High in young people, 0 by 70
plot(data_final_4$year, data_final_4$bmi_fpg) #Consistent for all ages 




