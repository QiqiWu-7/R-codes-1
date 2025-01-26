rm(list=ls())
library(tidyverse)
library(Rmisc)
library(sjmisc)
library(lubridate)
library(survival)
library(survminer)
library(haven)
library(rms)
library(ggplot2)
library(latex2exp)
library(openxlsx)
library(survMisc)
library(CoxR2)

data <- read.csv("D:/UKB_data/task/SI_outcome_wide/data.impu.done.csv") 
proteomics <- read.csv("D:/UKB_data/Protein_58082.csv")
Protein <- proteomics %>% dplyr::select("match_ID","X62_ADM.0","X1418_IL6.0","X2730_TNFRSF4.0","X1355_IGSF3.0","X505_CEACAM1.0")
colnames(Protein)
metabolomics <- read.csv("D:/UKB_data/metabolic_58082.csv")
metabolic <- metabolomics %>% dplyr::select("match_ID","X23453.0.0","X23618.0.0","X23469.0.0","X23615.0.0","X23550.0.0")
colnames(metabolic)

str(data)
data$loneliness_group<-factor(data$loneliness_group)
data$live_alone<-factor(data$live_alone)
data$visit<-factor(data$visit)
data$leisure<-factor(data$leisure)
data$si_group<-factor(data$si_group)
data$contact_group<-factor(data$contact_group)                    
data$sex<-factor(data$sex)
data$Ethnicity<-factor(data$Ethnicity)
data$region<-factor(data$region)
data$Employment<-factor(data$Employment)
data$education_level<-factor(data$education_level)
data$Smoking<-factor(data$Smoking)
data$alcohol_intake<-factor(data$alcohol_intake)
data$cancer<-factor(data$cancer)
data$chd_history<-factor(data$chd_history)
data$hypoglycemic0<-factor(data$hypoglycemic0)
data$anti_hypertension_medicine.0<-factor(data$anti_hypertension_medicine.0)
data$cholestetol_lowering_mediaction.0<-factor(data$cholestetol_lowering_mediaction.0)
data$age<-as.numeric(data$age)
data$MVPA_self<-as.numeric(data$MVPA_self)
str(data)
names(data)

folder_path <- "D:/UKB_data/mutiple outcome/Digestive system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_digestive <- data_list %>%
  reduce(full_join, by = "eid")
combined_digestive
colnames(combined_digestive)

data_PERM <- left_join(data,combined_digestive,by="eid")
data_PERM <-  merge(data_PERM,Protein,by.x = "eid", by.y = "match_ID", all.x = TRUE) 
data_PERM <-  merge(data_PERM,metabolic,by.x = "eid", by.y = "match_ID", all.x = TRUE) 
colnames(data_PERM)

disease_types<-c("Chronicliverdisease.HDC.ICD10")
length(disease_types)  

model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox0 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region, data=dataname)
  cox1 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region+TDI+education_level+Employment, data=dataname) 
  cox2 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region+Final_Healthy_diet_score+Smoking+alcohol_intake+
                  sleep_duration+MVPA_self, data=dataname) 
  cox3 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region+depression_score, data=dataname)
  cox4 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region+leukocytes+platelets+Mean_platelet+Lymphocyte+
                  Monocyte+Neutrophill+Eosinophill+Basophill+CRP, data=dataname)
  cox5 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region+infirmity+
                  anti_hypertension_medicine.0+cholestetol_lowering_mediaction.0, data=dataname)
  cox6 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region+
                  X62_ADM.0+X1418_IL6.0+X2730_TNFRSF4.0+X1355_IGSF3.0+X505_CEACAM1.0, data=dataname)
  cox7 <- coxph(Surv(ti,out) ~x+  age+sex+Ethnicity+region+
                  X23453.0.0+X23618.0.0+X23469.0.0+X23615.0.0+X23550.0.0, data=dataname)
  
  a <- cbind("PERM1" = (summary(cox0)$conf.int[1:z,c(1)]-summary(cox1)$conf.int[1:z,c(1)])/(summary(cox0)$conf.int[1:z,c(1)]-1)*100,
             "PERM2" = (summary(cox0)$conf.int[1:z,c(1)]-summary(cox2)$conf.int[1:z,c(1)])/(summary(cox0)$conf.int[1:z,c(1)]-1)*100,
             "PERM3" = (summary(cox0)$conf.int[1:z,c(1)]-summary(cox3)$conf.int[1:z,c(1)])/(summary(cox0)$conf.int[1:z,c(1)]-1)*100,
             "PERM4" = (summary(cox0)$conf.int[1:z,c(1)]-summary(cox4)$conf.int[1:z,c(1)])/(summary(cox0)$conf.int[1:z,c(1)]-1)*100,
             "PERM5" = (summary(cox0)$conf.int[1:z,c(1)]-summary(cox5)$conf.int[1:z,c(1)])/(summary(cox0)$conf.int[1:z,c(1)]-1)*100,
             "PERM6" = (summary(cox0)$conf.int[1:z,c(1)]-summary(cox6)$conf.int[1:z,c(1)])/(summary(cox0)$conf.int[1:z,c(1)]-1)*100,
             "PERM7" = (summary(cox0)$conf.int[1:z,c(1)]-summary(cox7)$conf.int[1:z,c(1)])/(summary(cox0)$conf.int[1:z,c(1)]-1)*100
  )
  a <- cbind(a, "exposure" = rep(exposure))
  result<-as.data.frame(a)
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

result_list_cox <- list()
colnames(data_PERM)

for (disease_type in disease_types) {

  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")
  
  df <- filter(data_PERM, !is.na(get(incidental_var)))
  
  result_cox <- model(df$si_group, paste("Social isolation~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  
  result_list_cox[[disease_type]] <- result_cox
}

Disease_cox<- do.call(rbind, result_list_cox)
Disease_cox

write.csv(Disease_cox,"D:/UKB_data/task/SI_outcome_wide/SI_liver_PERM.csv")
