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

sbp_ldl_ken = cor(data$bpsys, data$lbdldl, method = c("kendall"), use = "complete.obs")
sbp_ldl_pear = cor(data$bpsys, data$lbdldl, method = c("pearson"), use = "complete.obs")
sbp_ldl_spea = cor(data$bpsys, data$lbdldl, method = c("spearman"), use = "complete.obs")
sbp_ldl_ken 
sbp_ldl_pear
sbp_ldl_spea
ggscatter(data, x='bpsys', y='lbdldl', add = 'reg.line')

sbp_bmi_ken = cor(data$bpsys, data$bmi, method = c("kendall"), use = "complete.obs")
sbp_bmi_pear = cor(data$bpsys, data$bmi, method = c("pearson"), use = "complete.obs")
sbp_bmi_spea = cor(data$bpsys, data$bmi, method = c("spearman"), use = "complete.obs")
sbp_bmi_ken 
sbp_bmi_pear
sbp_bmi_spea
ggscatter(data, x='bpsys', y='bmi', add = 'reg.line')

ldl_bmi_ken = cor(data$lbdldl, data$bmi, method = c("kendall"), use = "complete.obs")
ldl_bmi_pear = cor(data$lbdldl, data$bmi, method = c("pearson"), use = "complete.obs")
ldl_bmi_spea = cor(data$lbdldl, data$bmi, method = c("spearman"), use = "complete.obs")
ldl_bmi_ken 
ldl_bmi_pear
ldl_bmi_spea
ggscatter(data, x='lbdldl', y='bmi', add = 'reg.line')

sbp_fpg_spea = cor(data$bpsys, data$lbxglu, method = c("spearman"), use = "complete.obs")
ldl_fpg_spea = cor(data$lbdldl, data$lbxglu, method = c("spearman"), use = "complete.obs")
bmi_fpg_spea = cor(data$bmi, data$lbxglu, method = c("spearman"), use = "complete.obs")
sbp_fpg_spea 
ldl_fpg_spea
bmi_fpg_spea
ggscatter(data, x='bpsys', y='lbxglu', add = 'reg.line')

data_20_25 = data[(data$age_year>20) & (data$age_year<25)]
sbp_ldl_spea = cor(data_20_25$bpsys, data_20_25$lbdldl, method = c("spearman"), use = "complete.obs")
sbp_ldl_spea
sbp_bmi_spea = cor(data_20_25$bpsys, data_20_25$bmi, method = c("spearman"), use = "complete.obs")
sbp_bmi_spea
ldl_bmi_spea = cor(data_20_25$lbdldl, data_20_25$bmi, method = c("spearman"), use = "complete.obs")
ldl_bmi_spea

data_final <- data.frame(matrix(ncol=8,nrow=0))
colnames(data_final) <- c('age_start','age_end','sbp_ldl','sbp_bmi','ldl_bmi','sbp_fpg','ldl_fpg','bmi_fpg')

for (i in 1:15){
  age_low = 25 + (5*(i-1))
  age_high = age_low + 5 
  data_current = data[(data$age_year>age_low) & (data$age_year<age_high)]
  sbp_ldl_spea = cor(data_current$bpsys, data_current$lbdldl, method = c("spearman"), use = "complete.obs")
  sbp_bmi_spea = cor(data_current$bpsys, data_current$bmi, method = c("spearman"), use = "complete.obs")
  ldl_bmi_spea = cor(data_current$lbdldl, data_current$bmi, method = c("spearman"), use = "complete.obs")
  sbp_fpg_spea = cor(data_current$bpsys, data_current$lbxglu, method = c("spearman"), use = "complete.obs")
  ldl_fpg_spea = cor(data_current$lbdldl, data_current$lbxglu, method = c("spearman"), use = "complete.obs")
  bmi_fpg_spea = cor(data_current$bmi, data_current$lbxglu, method = c("spearman"), use = "complete.obs")
  data_final[i,1] = age_low 
  data_final[i,2] = age_high
  data_final[i,3] = sbp_ldl_spea
  data_final[i,4] = sbp_bmi_spea
  data_final[i,5] = ldl_bmi_spea
  data_final[i,6] = sbp_fpg_spea
  data_final[i,7] = ldl_fpg_spea
  data_final[i,8] = bmi_fpg_spea
}

data_final

write.csv(data_final, 'H:/correlation.csv')
