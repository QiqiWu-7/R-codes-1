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
library(matrixStats)
library(data.table)

#--------------------------------------------------------------------------------
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

data <- read.csv("D:/UKB_data/task/SI_outcome_wide/matched_data2.csv")  
data_exposure_GRS <- fread("D:/UKB_data/task/SI_outcome_wide/GRS_SI.csv") 
gene <- fread("D:/UKB_data/UKB_HD/gene.csv")  
names(gene)
gene <- gene %>% dplyr::select(eid,"22027-0.0","22001-0.0","22019-0.0","22021-0.0",
                               "22009-0.1","22009-0.2","22009-0.3","22009-0.4","22009-0.5","22009-0.6",
                               "22009-0.7","22009-0.8","22009-0.9","22009-0.10","22000-0.0")
gene <- gene %>% dplyr::rename(X22027.0.0 = "22027-0.0",
                               X22001.0.0 = "22001-0.0",
                               X22019.0.0 = "22019-0.0",
                               X22021.0.0 = "22021-0.0",
                               X22009.0.1 = "22009-0.1",
                               X22009.0.2 = "22009-0.2",
                               X22009.0.3 = "22009-0.3",
                               X22009.0.4 = "22009-0.4",
                               X22009.0.5 = "22009-0.5",
                               X22009.0.6 = "22009-0.6",
                               X22009.0.7 = "22009-0.7",
                               X22009.0.8 = "22009-0.8",
                               X22009.0.9 = "22009-0.9",
                               X22009.0.10 = "22009-0.10",
                               X22000.0.0 = "22000-0.0")
data <- left_join(data,gene, by="eid")
length(data$eid)
data <- left_join(data,data_exposure_GRS, by="eid")
length(data$eid)
data<-left_join(data,combined_cancers,by="eid")
data<-left_join(data,combined_circulation,by="eid")
data<-left_join(data,combined_digestive,by="eid")
data<-left_join(data,combined_metabolic,by="eid")
data<-left_join(data,combined_genitourinary,by="eid")
data<-left_join(data,combined_Hematopoietic,by="eid")
data<-left_join(data,combined_Musculoskeletal,by="eid")
data<-left_join(data,combined_Nervous,by="eid")
data<-left_join(data,combined_Respiratory,by="eid")
data<-left_join(data,combined_mental,by="eid")
data<-left_join(data,combined_Eye,by="eid")
data<-left_join(data,combined_Ear,by="eid")
data<-left_join(data,combined_Skin,by="eid")
data<-left_join(data,combined_Infectious,by="eid")
data<-left_join(data,combined_Mortality,by="eid")
##quality control for genetic analysis
dim(data)
names(data)
length(data$eid)    
##genetic quality control (individuals that are outliers in heterozygosity and missing rates: Filed ID 22027)
data1<-filter(data,is.na(data$X22027.0.0)==T)
dim(data1)  
data1<-as.data.frame(data1)
length(data1$eid)  
t1<-length(data$eid)-length(data1$eid)       
t1
## sex mismatch (Genetic sex:22001;Sex:X31.0.0)  
data2<-filter(data1,data1$X22001.0.0==data1$sex)  
length(data2$eid)  
t2<-length(data1$eid)-length(data2$eid)
t2
## individuals with sex chromosome aneuploidy (ID22019)
data3<-filter(data2,is.na(data2$X22019.0.0)==TRUE) 
length(data3$eid)
t3<-length(data2$eid)-length(data3$eid)
t3
## excessive genetic relatedness(more than 10 putative third-degree relatives in the kinship table Field ID 22021) 
data4<-filter(data3,data3$X22021.0.0==1|data3$X22021.0.0==0) 
length(data4$eid)
t4<-length(data3$eid)-length(data4$eid)
t4
##White British
PRS_data<-filter(data4,data4$Ethnicity==1)  
length(PRS_data$eid)  
t5<-length(data4$eid)-length(PRS_data$eid)
t5
##IMPUTATION
names(PRS_data)
length(PRS_data$eid)
pa_list<-data%>%summarize_at(vars(58:70),median,na.rm=TRUE)  
pa_list
pa_list<-t(pa_list)
pa_list<-data.frame(pa_list)
pa_list
write.csv(pa_list,"D:/UKB_data/task/SI_outcome_wide/pa_Imputation.csv",row.names = TRUE)
data_imputation=read.csv("D:/UKB_data/task/SI_outcome_wide/pa_Imputation.csv")
colnames(data_imputation)<-c("rsid","value")
l<-as.character(data_imputation$rsid)
v<-as.vector(data_imputation$value)
length(data_imputation$rsid)
for(i in 1:length(data_imputation$rsid)){
  if(l[i] %in% colnames(data)){
    data[l[i]][is.na(data[l[i]])==TRUE]<-v[i]
  }
}
###Weighted allele socre
data_exposure=read.csv('D:/UKB_data/task/SI_snp final.csv')
wgrs=rep(0,dim(data)[1])
m<-as.vector(data_exposure$rsid)
for(i in 1:length(data_exposure$rsid)){
  beta=abs(data_exposure$effect[i])   
  if(m[i] %in% colnames(data)){
    wgrs=wgrs+beta*data[,m[i]]
  }
}
data$weightgrs<-wgrs

#----------------------------------------------------------------------------------------------
#Chronicliverdisease
df <- data %>% mutate(Incidental.Chronicliverdisease.HDC.ICD10 = ifelse(is.na(Incidental.Chronicliverdisease.HDC.ICD10), 1, Incidental.Chronicliverdisease.HDC.ICD10))
logfit1<-glm(Incidental.Chronicliverdisease.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
# T2DM
df <- data %>% mutate(Incidental.T2DM.HDC.ICD10 = ifelse(is.na(Incidental.T2DM.HDC.ICD10), 1, Incidental.T2DM.HDC.ICD10))
logfit1<-glm(Incidental.T2DM.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Hematopoietic
df <- data %>% mutate(Incidental.Hematopoietic.HDC.ICD10 = ifelse(is.na(Incidental.Hematopoietic.HDC.ICD10), 1, Incidental.Hematopoietic.HDC.ICD10))
logfit1<-glm(Incidental.Hematopoietic.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#anaemias
df <- data %>% mutate(Incidental.anaemias.HDC.ICD10 = ifelse(is.na(Incidental.anaemias.HDC.ICD10), 1, Incidental.anaemias.HDC.ICD10))
logfit1<-glm(Incidental.anaemias.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#COPD
df <- data %>% mutate(Incidental.COPD.HDC.ICD10 = ifelse(is.na(Incidental.COPD.HDC.ICD10), 1, Incidental.COPD.HDC.ICD10))
logfit1<-glm(Incidental.COPD.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Mental
df <- data %>% mutate(Incidental.Mental.HDC.ICD10 = ifelse(is.na(Incidental.Mental.HDC.ICD10), 1, Incidental.Mental.HDC.ICD10))
logfit1<-glm(Incidental.Mental.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#depression
df <- data %>% mutate(Incidental.depression.HDC.ICD10 = ifelse(is.na(Incidental.depression.HDC.ICD10), 1, Incidental.depression.HDC.ICD10))
logfit1<-glm(Incidental.depression.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#anxiety
df <- data %>% mutate(Incidental.anxiety.HDC.ICD10 = ifelse(is.na(Incidental.anxiety.HDC.ICD10), 1, Incidental.anxiety.HDC.ICD10))
logfit1<-glm(Incidental.anxiety.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#substance_abuse
df <- data %>% mutate(Incidental.substance_abuse.HDC.ICD10 = ifelse(is.na(Incidental.substance_abuse.HDC.ICD10), 1, Incidental.substance_abuse.HDC.ICD10))
logfit1<-glm(Incidental.substance_abuse.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Pneumonia
df <- data %>% mutate(Incidental.Pneumonia.HDC.ICD10 = ifelse(is.na(Incidental.Pneumonia.HDC.ICD10), 1, Incidental.Pneumonia.HDC.ICD10))
logfit1<-glm(Incidental.Pneumonia.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Urinary_tract_infection
df <- data %>% mutate(Incidental.Urinary_tract_infection.HDC.ICD10 = ifelse(is.na(Incidental.Urinary_tract_infection.HDC.ICD10), 1, Incidental.Urinary_tract_infection.HDC.ICD10))
logfit1<-glm(Incidental.Urinary_tract_infection.HDC.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#AllCause.Mortality
df <- data %>% mutate(Incidental.AllCause.Mortality.ICD10 = ifelse(is.na(Incidental.AllCause.Mortality.ICD10), 1, Incidental.AllCause.Mortality.ICD10))
logfit1<-glm(Incidental.AllCause.Mortality.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Cancer.Mortality
df <- data %>% mutate(Incidental.Cancer.Mortality.ICD10 = ifelse(is.na(Incidental.Cancer.Mortality.ICD10), 1, Incidental.Cancer.Mortality.ICD10))
logfit1<-glm(Incidental.Cancer.Mortality.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#CVD.Mortality
df <- data %>% mutate(Incidental.CVD.Mortality.ICD10 = ifelse(is.na(Incidental.CVD.Mortality.ICD10), 1, Incidental.CVD.Mortality.ICD10))
logfit1<-glm(Incidental.CVD.Mortality.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Digestive.Mortality
df <- data %>% mutate(Incidental.Digestive.Mortality.ICD10 = ifelse(is.na(Incidental.Digestive.Mortality.ICD10), 1, Incidental.Digestive.Mortality.ICD10))
logfit1<-glm(Incidental.Digestive.Mortality.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Infections.Mortality
df <- data %>% mutate(Incidental.Infections.Mortality.ICD10 = ifelse(is.na(Incidental.Infections.Mortality.ICD10), 1, Incidental.Infections.Mortality.ICD10))
logfit1<-glm(Incidental.Infections.Mortality.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
#Respiratory.Mortality
df <- data %>% mutate(Incidental.Respiratory.Mortality.ICD10 = ifelse(is.na(Incidental.Respiratory.Mortality.ICD10), 1, Incidental.Respiratory.Mortality.ICD10))
logfit1<-glm(Incidental.Respiratory.Mortality.ICD10 ~weightgrs  +age+sex+Ethnicity+region+TDI+education_level+Smoking+alcohol_intake+BMI+MVPA_self+loneliness_group+X22009.0.1+X22009.0.2+X22009.0.3+X22009.0.4+X22009.0.5+X22009.0.6+X22009.0.7+X22009.0.8+X22009.0.9+X22009.0.10+X22000.0.0, data=df,family=binomial(link="logit"))
summary(logfit1)
exp(coef(logfit1))
exp(confint.default(logfit1))
