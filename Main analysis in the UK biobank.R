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
library("MatchIt")


#PSM----------------------------------------------------------------------------
data<-read.csv("D:/UKB_data/task/SI_outcome_wide/data.impu.done.csv")
colnames(data)
frq(data$si_group)
data<-drop_na(data)
data1<-matchit(data=data,
               formula=si_group ~ age + sex+Ethnicity+region+TDI+education_level+
                 Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group,
               method = "nearest", distance = "logit", replace = FALSE,caliper = 0.05)

summary(data1)
matched_data1 <- match.data(data1)
colnames(matched_data1)
write.csv(matched_data1,"D:/UKB_data/task/SI_outcome_wide/matched_data2.csv")


#-------------------------------------------------------------------------------
folder_path <- "D:/UKB_data/mutiple outcome/cancer/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_cancers <- data_list %>%
  reduce(full_join, by = "eid")
combined_cancers
colnames(combined_cancers)

folder_path <- "D:/UKB_data/mutiple outcome/Circulation system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_circulation <- data_list %>%
  reduce(full_join, by = "eid")
combined_circulation
colnames(combined_circulation)

folder_path <- "D:/UKB_data/mutiple outcome/Digestive system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_digestive <- data_list %>%
  reduce(full_join, by = "eid")
combined_digestive
colnames(combined_digestive)

folder_path <- "D:/UKB_data/mutiple outcome/Endocrine, metabolic disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_metabolic <- data_list %>%
  reduce(full_join, by = "eid")
combined_metabolic
colnames(combined_metabolic)

folder_path <- "D:/UKB_data/mutiple outcome/Genitourinary system/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_genitourinary <- data_list %>%
  reduce(full_join, by = "eid")
combined_genitourinary
colnames(combined_genitourinary)


folder_path <- "D:/UKB_data/mutiple outcome/Hematopoietic system/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Hematopoietic <- data_list %>%
  reduce(full_join, by = "eid")
combined_Hematopoietic
colnames(combined_Hematopoietic)

folder_path <- "D:/UKB_data/mutiple outcome/Musculoskeletal system and connective tissue/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Musculoskeletal<- data_list %>%
  reduce(full_join, by = "eid")
combined_Musculoskeletal
colnames(combined_Musculoskeletal)


folder_path <- "D:/UKB_data/mutiple outcome/Nervous system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Nervous<- data_list %>%
  reduce(full_join, by = "eid")
combined_Nervous
colnames(combined_Nervous)

folder_path <- "D:/UKB_data/mutiple outcome/Respiratory system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Respiratory<- data_list %>%
  reduce(full_join, by = "eid")
combined_Respiratory
colnames(combined_Respiratory)

folder_path <- "D:/UKB_data/mutiple outcome/Mental and behavioural disorder/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_mental<- data_list %>%
  reduce(full_join, by = "eid")
combined_mental
colnames(combined_mental)


folder_path <- "D:/UKB_data/mutiple outcome/Eye/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Eye<- data_list %>%
  reduce(full_join, by = "eid")
combined_Eye
colnames(combined_Eye)


folder_path <- "D:/UKB_data/mutiple outcome/Ear/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Ear<- data_list %>%
  reduce(full_join, by = "eid")
combined_Ear
colnames(combined_Ear)

folder_path <- "D:/UKB_data/mutiple outcome/Skin/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Skin<- data_list %>%
  reduce(full_join, by = "eid")
combined_Skin
colnames(combined_Skin)

folder_path <- "D:/UKB_data/mutiple outcome/Infectious/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Infectious<- data_list %>%
  reduce(full_join, by = "eid")
combined_Infectious
colnames(combined_Infectious)

folder_path <- "D:/UKB_data/mutiple outcome/Mortality/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Mortality<- data_list %>%
  reduce(full_join, by = "eid")
combined_Mortality
colnames(combined_Mortality)


data1<-read.csv("D:/UKB_data/task/SI_outcome_wide/matched_data2.csv")
colnames(data1)
str(data1)
data1$loneliness_group<-factor(data1$loneliness_group)
data1$live_alone<-factor(data1$live_alone)
data1$visit<-factor(data1$visit)
data1$leisure<-factor(data1$leisure)
data1$si_group<-factor(data1$si_group)
data1$contact_group<-factor(data1$contact_group)                    
data1$sex<-factor(data1$sex)
data1$Ethnicity<-factor(data1$Ethnicity)
data1$region<-factor(data1$region)
data1$Employment<-factor(data1$Employment)
data1$education_level<-factor(data1$education_level)
data1$Smoking<-factor(data1$Smoking)
data1$alcohol_intake<-factor(data1$alcohol_intake)
data1$cancer<-factor(data1$cancer)
data1$chd_history<-factor(data1$chd_history)
data1$hypoglycemic0<-factor(data1$hypoglycemic0)
data1$anti_hypertension_medicine.0<-factor(data1$anti_hypertension_medicine.0)
data1$cholestetol_lowering_mediaction.0<-factor(data1$cholestetol_lowering_mediaction.0)
data1$age<-as.numeric(data1$age)
data1$MVPA_self<-as.numeric(data1$MVPA_self)
data1<-left_join(data1,combined_cancers,by="eid")
data1<-left_join(data1,combined_circulation,by="eid")
data1<-left_join(data1,combined_digestive,by="eid")
data1<-left_join(data1,combined_metabolic,by="eid")
data1<-left_join(data1,combined_genitourinary,by="eid")
data1<-left_join(data1,combined_Hematopoietic,by="eid")
data1<-left_join(data1,combined_Musculoskeletal,by="eid")
data1<-left_join(data1,combined_Nervous,by="eid")
data1<-left_join(data1,combined_Respiratory,by="eid")
data1<-left_join(data1,combined_mental,by="eid")
data1<-left_join(data1,combined_Eye,by="eid")
data1<-left_join(data1,combined_Ear,by="eid")
data1<-left_join(data1,combined_Skin,by="eid")
data1<-left_join(data1,combined_Infectious,by="eid")
data1<-left_join(data1,combined_Mortality,by="eid")
disease_types<-c("cancers.HDC.ICD10","lungcancer.HDC.ICD10",
                 "breastcancer.HDC.ICD10","bladdercancer.HDC.ICD10",
                 "colorectalcancer.HDC.ICD10","non_Hodgkin_lymphoma.HDC.ICD10",
                 "prostatecancer.HDC.ICD10",
                 "CVD.HDC.ICD10","IHD.HDC.ICD10","stroke.HDC.ICD10","AF.HDC.ICD10","PAD.HDC.ICD10","hypertension.HDC.ICD10",
                 "Venous_thromboembolism.HDC.ICD10","Haemorrhoids.HDC.ICD10",
                 "Digestive.HDC.ICD10","Chronicliverdisease.HDC.ICD10","gallbladder_biliary_tract_pancreas.HDC.ICD10","IBD.HDC.ICD10",
                 "esophagus_stomach_duodenum.HDC.ICD10","Diverticular_disease.HDC.ICD10",
                 "metabolic_disease.HDC.ICD10","T2DM.HDC.ICD10","hyperthyroidism.HDC.ICD10","hypothyroidism.HDC.ICD10",
                 "obesity.HDC.ICD10",
                 "Genitourinary.HDC.ICD10","Urolithiasis.HDC.ICD10","Bladder_disease.HDC.ICD10","Hyperplasia_prostate.HDC.ICD10",
                 "Hematopoietic.HDC.ICD10","anaemias.HDC.ICD10","haemorrhagic.HDC.ICD10",
                 "Musculoskeletal_connectivetissue.HDC.ICD10","gout.HDC.ICD10","Rheumatoidarthritis.HDC.ICD10",
                 "Spondylosis.HDC.ICD10",
                 "Nervous.HDC.ICD10","AD.HDC.ICD10","PD.HDC.ICD10","epilepsy.HDC.ICD10","migraine.HDC.ICD10",
                 "Respiratory.HDC.ICD10","Asthma.HDC.ICD10","COPD.HDC.ICD10","Other_upper_respiratory.HDC.ICD10",
                 "Mental.HDC.ICD10","depression.HDC.ICD10","anxiety.HDC.ICD10","bipolar.HDC.ICD10",
                 "OSA.HDC.ICD10","substance_abuse.HDC.ICD10","Suicide.HDC.ICD10",
                 "eyes.HDC.ICD10","glaucoma.HDC.ICD10","cataract.HDC.ICD10",
                 "ears.HDC.ICD10","hear_loss.HDC.ICD10","disorders_of_vestibular_function.HDC.ICD10",
                 "skins.HDC.ICD10","atopic_dermatitis.HDC.ICD10","psoriasis.HDC.ICD10",
                 "infectious.HDC.ICD10","Gastrointestinal_infections.HDC.ICD10","Influenza.HDC.ICD10","Pneumonia.HDC.ICD10",
                 "Urinary_tract_infection.HDC.ICD10",
                 "AllCause.Mortality.ICD10","Cancer.Mortality.ICD10","CVD.Mortality.ICD10","Digestive.Mortality.ICD10",
                 "Infections.Mortality.ICD10","Respiratory.Mortality.ICD10")
length(disease_types)
colnames(data1)

#SI Cox--------------------------------------------------------------------------------------------
colnames(data1)
model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox1 <- coxph(Surv(ti,out) ~x+  age + sex , data=dataname)
  cox2 <- coxph(Surv(ti,out) ~x+ age + sex+Ethnicity+region+TDI+education_level+
                  Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group, data=dataname)
  level <- c("Not isolated","Socially isolated")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- rbind(a,b)
  result<-as.data.frame(d)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.9f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

case<-function(x,x1,model,time,outcome,data){
  group_vars1 <- syms(as.character(x))
  group_vars2 <- syms(as.character(outcome))
  group_vars3 <- syms(as.character(time)) 
  
  a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
  b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3)/365)
  d<-merge(a,b,by=x,all.x = T)
  d
  d$personyear<-d[,2]/d[,3]*1000
  d
  c<-table(x1)
  c
  c<-data.frame(c) 
  names(c)
  c
  
  z<-cbind( "N" =c[,c(2)],
            "case"= d[,2],
            "follow up"= d[,3],
            "person years"=d[,4],
            "model" =rep(model)
  )
  z<-data.frame(z)
  names(z)
  z[,3]<-sprintf("%0.0f", as.numeric(z[,3]))
  z[,4]<-sprintf("%0.2f", as.numeric(z[,4]))
  z1<-z%>% unite(case_p,'case',"follow.up",sep="/")
  result<-z1
  y<- length(table(x1))-1
  level <- c("Not isolated","Socially isolated")
  result<-cbind(level,result)
  return(result)
}


result_list_cox <- list()
result_list_case<-list()
colnames(data1)
for (disease_type in disease_types) {
  
  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")

  df <- filter(data1, !is.na(get(incidental_var)))
  
  result_cox <- model(df$si_group, paste("Social isolation~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  result_case <- case(c("si_group"), df$si_group,
                      paste("Social isolation~", disease_type),
                      c(fuduration_var),
                      c(incidental_var),
                      df)
  
  result_list_cox[[disease_type]] <- result_cox
  result_list_case[[disease_type]] <- result_case
}

Disease_cox<- do.call(rbind, result_list_cox)
Disease_cox
Disease_case<- do.call(rbind, result_list_case)
Disease_case

write.csv(Disease_cox,"D:/UKB_data/task/SI_outcome_wide/Cox_si_PSM.csv")
write.csv(Disease_case,"D:/UKB_data/task/SI_outcome_wide/Case_si_PSM.csv")

data<-read.csv("D:/UKB_data/task/SI_outcome_wide/Cox_si_PSM.csv")
names(data)
data$Bonferroni=p.adjust(data$pvalue,method = "bonferroni")
write.csv(data,"D:/UKB_data/task/SI_outcome_wide/Cox_si_PSM.csv",row.names = F)



#LIVE ALONE---------------------------------------------------------------------------------------
frq(data1$live_alone)
model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox1 <- coxph(Surv(ti,out) ~x+  age + sex , data=dataname)
  cox2 <- coxph(Surv(ti,out) ~x+ age + sex+Ethnicity+region+TDI+education_level+
                  Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group, data=dataname)
  level <- c("Not live alone","Live alone")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- rbind(a,b)
  result<-as.data.frame(d)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.9f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

case<-function(x,x1,model,time,outcome,data){
  group_vars1 <- syms(as.character(x))
  group_vars2 <- syms(as.character(outcome))
  group_vars3 <- syms(as.character(time)) 
  
  a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
  b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3)/365)
  d<-merge(a,b,by=x,all.x = T)
  d
  d$personyear<-d[,2]/d[,3]*1000
  d
  c<-table(x1)
  c
  c<-data.frame(c) 
  names(c)
  c
  
  z<-cbind( "N" =c[,c(2)],
            "case"= d[,2],
            "follow up"= d[,3],
            "person years"=d[,4],
            "model" =rep(model)
  )
  z<-data.frame(z)
  names(z)
  z[,3]<-sprintf("%0.0f", as.numeric(z[,3]))
  z[,4]<-sprintf("%0.2f", as.numeric(z[,4]))
  z1<-z%>% unite(case_p,'case',"follow.up",sep="/")
  result<-z1
  y<- length(table(x1))-1
  level <- c("Not live alone","Live alone")
  result<-cbind(level,result)
  return(result)
}


result_list_cox <- list()
result_list_case<-list()
for (disease_type in disease_types) {

  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")

  df <- filter(data1, !is.na(get(incidental_var)))
  
  result_cox <- model(df$live_alone, paste("live_alone~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  result_case <- case(c("live_alone"), df$live_alone,
                      paste("live_alone~", disease_type),
                      c(fuduration_var),
                      c(incidental_var),
                      df)
  
  result_list_cox[[disease_type]] <- result_cox
  result_list_case[[disease_type]] <- result_case
}

Disease_cox_live_alone<- do.call(rbind, result_list_cox)
Disease_cox_live_alone
Disease_case_live_alone<- do.call(rbind, result_list_case)
Disease_case_live_alone
write.csv(Disease_cox_live_alone,"D:/UKB_data/task/SI_outcome_wide/Cox_live_alone_PSM.csv")
write.csv(Disease_case_live_alone,"D:/UKB_data/task/SI_outcome_wide/Case_live_alone_PSM.csv")


data<-read.csv("D:/UKB_data/task/SI_outcome_wide/Cox_live_alone_PSM.csv")
names(data)
data$Bonferroni=p.adjust(data$pvalue,method = "bonferroni")
write.csv(data,"D:/UKB_data/task/SI_outcome_wide/Cox_live_alone_PSM.csv",row.names = F)



# Little contact with family or friends-----------------------------------------------
frq(data1$visit)
model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox1 <- coxph(Surv(ti,out) ~x+  age + sex , data=dataname)
  cox2 <- coxph(Surv(ti,out) ~x+ age + sex+Ethnicity+region+TDI+education_level+
                  Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group, data=dataname)
  level <- c("More visits","Little visits")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- rbind(a,b)
  result<-as.data.frame(d)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.9f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

case<-function(x,x1,model,time,outcome,data){
  group_vars1 <- syms(as.character(x))
  group_vars2 <- syms(as.character(outcome))
  group_vars3 <- syms(as.character(time)) 
  
  a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
  b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3)/365)
  d<-merge(a,b,by=x,all.x = T)
  d
  d$personyear<-d[,2]/d[,3]*1000
  d
  c<-table(x1)
  c
  c<-data.frame(c) 
  names(c)
  c
  
  z<-cbind( "N" =c[,c(2)],
            "case"= d[,2],
            "follow up"= d[,3],
            "person years"=d[,4],
            "model" =rep(model)
  )
  z<-data.frame(z)
  names(z)
  z[,3]<-sprintf("%0.0f", as.numeric(z[,3]))
  z[,4]<-sprintf("%0.2f", as.numeric(z[,4]))
  z1<-z%>% unite(case_p,'case',"follow.up",sep="/")
  result<-z1
  y<- length(table(x1))-1
  level <- c("More visits","Little visits")
  result<-cbind(level,result)
  return(result)
}


result_list_cox <- list()
result_list_case<-list()
for (disease_type in disease_types) {
  
  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")
  
  df <- filter(data1, !is.na(get(incidental_var)))
  
  result_cox <- model(df$visit, paste("little_visits~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  result_case <- case(c("visit"), df$visit,
                      paste("little_visits~", disease_type),
                      c(fuduration_var),
                      c(incidental_var),
                      df)
  
  result_list_cox[[disease_type]] <- result_cox
  result_list_case[[disease_type]] <- result_case
}

Disease_cox_visit<- do.call(rbind, result_list_cox)
Disease_cox_visit
Disease_case_visit<- do.call(rbind, result_list_case)
Disease_case_visit
write.csv(Disease_cox_visit,"D:/UKB_data/task/SI_outcome_wide/Cox_little_visit_PSM.csv")
write.csv(Disease_case_visit,"D:/UKB_data/task/SI_outcome_wide/Case_little_visit_PSM.csv")


data<-read.csv("D:/UKB_data/task/SI_outcome_wide/Cox_little_visit_PSM.csv")
names(data)
data$Bonferroni=p.adjust(data$pvalue,method = "bonferroni")
write.csv(data,"D:/UKB_data/task/SI_outcome_wide/Cox_little_visit_PSM.csv",row.names = F)





# Fewer leisure/social activities----------------------------------------------------
frq(data1$leisure)
model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox1 <- coxph(Surv(ti,out) ~x+  age + sex , data=dataname)
  cox2 <- coxph(Surv(ti,out) ~x+ age + sex+Ethnicity+region+TDI+education_level+
                  Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group, data=dataname)
  level <- c("More activities","Fewer activities")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- rbind(a,b)
  result<-as.data.frame(d)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.9f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

case<-function(x,x1,model,time,outcome,data){
  group_vars1 <- syms(as.character(x))
  group_vars2 <- syms(as.character(outcome))
  group_vars3 <- syms(as.character(time)) 
  
  a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
  b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3)/365)
  d<-merge(a,b,by=x,all.x = T)
  d
  d$personyear<-d[,2]/d[,3]*1000
  d
  c<-table(x1)
  c
  c<-data.frame(c) 
  names(c)
  c
  
  z<-cbind( "N" =c[,c(2)],
            "case"= d[,2],
            "follow up"= d[,3],
            "person years"=d[,4],
            "model" =rep(model)
  )
  z<-data.frame(z)
  names(z)
  z[,3]<-sprintf("%0.0f", as.numeric(z[,3]))
  z[,4]<-sprintf("%0.2f", as.numeric(z[,4]))
  z1<-z%>% unite(case_p,'case',"follow.up",sep="/")
  result<-z1
  y<- length(table(x1))-1
  level <- c("More activities","Fewer activities")
  result<-cbind(level,result)
  return(result)
}

result_list_cox <- list()
result_list_case<-list()

for (disease_type in disease_types) {

  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")
  
  df <- filter(data1, !is.na(get(incidental_var)))

  result_cox <- model(df$leisure, paste("fewer_activities~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  result_case <- case(c("leisure"), df$leisure,
                      paste("fewer_activities~", disease_type),
                      c(fuduration_var),
                      c(incidental_var),
                      df)
  
  result_list_cox[[disease_type]] <- result_cox
  result_list_case[[disease_type]] <- result_case
}

Disease_cox_leisure<- do.call(rbind, result_list_cox)
Disease_cox_leisure
Disease_case_leisure<- do.call(rbind, result_list_case)
Disease_case_leisure
write.csv(Disease_cox_leisure,"D:/UKB_data/task/SI_outcome_wide/Cox_fewer_activities_PSM.csv")
write.csv(Disease_case_leisure,"D:/UKB_data/task/SI_outcome_wide/Case_fewer_activities_PSM.csv")


data<-read.csv("D:/UKB_data/task/SI_outcome_wide/Cox_fewer_activities_PSM.csv")
names(data)
data$Bonferroni=p.adjust(data$pvalue,method = "bonferroni")
write.csv(data,"D:/UKB_data/task/SI_outcome_wide/Cox_fewer_activities_PSM.csv",row.names = F)








