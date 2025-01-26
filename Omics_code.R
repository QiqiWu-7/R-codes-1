rm(list=ls())
library(mediation)
library(dplyr)
library(survival)
library(tidyverse)
library(Rmisc)
library(sjmisc)
library(lubridate)
library(survminer)
library(haven)
library(rms)
library(ggplot2)
library(latex2exp)
library(openxlsx)
library(survMisc)
library(CoxR2)

data <- read.csv("D:/UKB_data/task/SI_outcome_wide/data.impu.done.csv")  
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

folder_path <- "D:/UKB_data/mutiple outcome/Digestive system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_digestive <- data_list %>%
  reduce(full_join, by = "eid")
combined_digestive
colnames(combined_digestive)
data<-left_join(data,combined_digestive,by="eid")
colnames(data)
disease_types<-c("Chronicliverdisease.HDC.ICD10")
length(disease_types)
colnames(data)

Protein <- read.csv("D:/UKB_data/Protein_58082.csv")
names(data)
names(Protein)
data_Protein <- merge(data,Protein,by.x = "eid", by.y = "match_ID", all.x = TRUE) 
#--------------------------------------------------------------
colnames(data_Protein)
Protein_names <- colnames(data_Protein)[147:5995]  
length(Protein_names) 
exposures <- Protein_names 
final_result_list <- list()  
for (disease_type in disease_types) {
  result_list_cox <- list() 
  for (exposure in exposures) {
    incidental_var <- paste("Incidental", disease_type, sep = ".")
    fuduration_var <- paste("FUduration", disease_type, sep = ".")
    df <- filter(data_Protein, !is.na(get(incidental_var)))
    formula <- as.formula(paste("Surv(", fuduration_var, ",", incidental_var, ") ~ ", exposure, 
                                "+ age + sex + Ethnicity + region + TDI + education_level + 
                                  Smoking + alcohol_intake + BMI + MVPA_self + loneliness_group"))
    cox_model <- coxph(formula, data = df)  
    a <- cbind("HR" = summary(cox_model)$conf.int[1, 1],
               "low" = summary(cox_model)$conf.int[1, 3],
               "up" = summary(cox_model)$conf.int[1, 4],
               "pvalue" = summary(cox_model)$coefficients[1, 5])
    result <- as.data.frame(a)
    result$exposure <- exposure
    result$model <- "Cox"
    result$Outcomes <- disease_type
    result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
    result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
    result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
    result[,4]<-sprintf("%0.9f", as.numeric(result[,4]))
    result <- rownames_to_column(result, var = "rowname")
    result_list_cox[[exposure]] <- result
    Disease_cox <- do.call(rbind, result_list_cox)
    Disease_cox$Bonferroni=p.adjust(Disease_cox$pvalue,method = "bonferroni")
    final_result_list[[disease_type]] <- Disease_cox
  }
}
all_results <- do.call(rbind, final_result_list)
all_results
write.csv(all_results,"D:/UKB_data/task/SI_outcome_wide/Protein_Chronicliverdisease.csv")
#------------------
Protein_outcomes <- read.csv("D:/UKB_data/task/SI_outcome_wide/Protein_Chronicliverdisease.csv")
names(Protein_outcomes)
final_result_list <- list() 
for (disease_type in disease_types)  {
  df <- filter(Protein_outcomes, Protein_outcomes$Outcomes == disease_type)
  df <- filter(df, df$Bonferroni < 0.05)
  exposures <- df$exposure
  result_list_logit <- list() 
  for (exposure in exposures) {
    formula <- as.formula(paste("si_group ~",exposure,"+ age + sex + Ethnicity + region + TDI + education_level + 
                                  Smoking + alcohol_intake + BMI + MVPA_self + loneliness_group" ))
    model <- glm(formula, data = data_Protein, family = binomial)
    summary_model <- summary(model)
    lower <- confint.default(model)[2, "2.5 %"]
    upper <- confint.default(model)[2, "97.5 %"]
    result <- data.frame(
      "exposure" = exposure,
      "outcome" = disease_type,
      "pvalue" = summary_model$coefficients[2, 4],
      "beta" = summary_model$coefficients[2, 1],
      "Se" = summary_model$coefficients[2, 2],
      "lower" <- lower,
      "upper" <- upper
    ) 
    result[,3]<-sprintf("%0.3f", as.numeric(result[,3]))      
    result$blank<-rep("")
    result <- result %>% unite(CI,`beta`,`X.lower.....lower`, sep = "(",remove = F)  
    result <- result %>% unite(CI,`CI`,`X.upper.....upper`, sep = "-",remove = F)  
    result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
    result <- rownames_to_column(result, var = "rowname")
    result
    result_list_logit[[exposure]] <- result
    Disease_logit <- do.call(rbind, result_list_logit)
    Disease_logit$Bonferroni=p.adjust(Disease_logit$pvalue,method = "bonferroni")
    final_result_list[[disease_type]] <- Disease_logit
  }
} 
all_results <- do.call(rbind, final_result_list)  
all_results
write.csv(all_results,"D:/UKB_data/task/SI_outcome_wide/SI_Protein(Chronicliverdisease).csv")
#-----------------------------------------
SI_Protein <- read.csv("D:/UKB_data/task/SI_outcome_wide/SI_Protein(Chronicliverdisease).csv")
names(SI_Protein)
final_result_list <- list() 
for (disease_type in disease_types)  {
  df <- filter(SI_Protein, SI_Protein$outcome == disease_type)
  df <- filter(df, df$Bonferroni < 0.05)
  mediators <- df$exposure  
  result_list_mediate <- list()  
  for (mediator in mediators) {
    incidental_var <- paste("Incidental", disease_type, sep = ".")
    fuduration_var <- paste("FUduration", disease_type, sep = ".")
    df <- filter(data_Protein, !is.na(get(incidental_var)))
    mediator_formula <- as.formula(paste(mediator, "~", "si_group","+ age + sex + Ethnicity + region + TDI + 
                                  education_level + Smoking + alcohol_intake + BMI + MVPA_self + loneliness_group"))
    outcome_formula <- as.formula(paste("Surv(", fuduration_var, ",", incidental_var, ") ~", "si_group", "+", mediator,
                                        "+ age + sex + Ethnicity + region + TDI + education_level + Smoking + alcohol_intake + BMI + MVPA_self + loneliness_group"))
    mediator_model <- lm(mediator_formula, data = df)     
    outcome_model <- survreg(outcome_formula, data = df)
    med <- mediate(mediator_model, outcome_model, treat = "si_group", mediator = mediator, sims = 500)
    summary_info <- summary(med)
    ACME <- summary_info$d.avg
    p_ACME <- summary_info$d.avg.p
    ADE <- summary_info$z.avg
    p_ADE <- summary_info$z.avg.p
    Prop <- summary_info$n.avg
    p_prop <- summary_info$n.avg.p
    total <- summary_info$tau.coef
    p_total <- summary_info$tau.p
    result <- data.frame(Exposure_Mediator_Outcome = paste("si_group", mediator, disease_type, sep = "_"),
                         ACME = ACME,
                         p_ACME = p_ACME,
                         ADE = ADE,
                         p_ADE = p_ADE,
                         Prop = Prop,
                         p_prop = p_prop,
                         total = total,
                         p_total = p_total,
                         "exposure" = "si_group",
                         "mediator" = mediator,
                         "outcome" = disease_type)
    result_list_mediate[[mediator]] <- result
    Disease_mediate <- do.call(rbind, result_list_mediate)
    final_result_list[[disease_type]] <- Disease_mediate
  }
}
all_results <- do.call(rbind, final_result_list)  
all_results
write.csv(all_results,"D:/UKB_data/task/SI_outcome_wide/SI_Protein_Chronicliverdisease.csv")
#------------------------------
result_med <- read.csv("D:/UKB_data/task/SI_outcome_wide/SI_Protein_Chronicliverdisease.csv")
names(result_med)
length(result_med$mediator) 
result_sig<-filter(result_med, result_med$p_ACME<0.05)
result_sig<-filter(result_sig, result_sig$p_ADE<0.05)
result_sig<-filter(result_sig, result_sig$p_total<0.05)
length(result_sig$mediator)
Protein_names <- c("match_ID",result_sig$mediator)
length(Protein_names)  
data1 <- Protein[,names(Protein) %in% Protein_names]
length(colnames(data1))  
data_Protein <- merge(data,data1,by.x = "eid", by.y = "match_ID", all.x = F) 
length(data_Protein$eid) 
set.seed(123)
data2 <- filter(data_Protein,is.na(data_Protein$Incidental.Chronicliverdisease.HDC.ICD10)==F)
label_names <- read.csv("D:/UKB_data/name_Protein.csv")
name_mapping <- setNames(label_names$lable, label_names$field_id)
colnames(data2) <- ifelse(colnames(data2) %in% names(name_mapping),
                          name_mapping[colnames(data2)],
                          colnames(data2))
inTrain <- createDataPartition(y=data2[,"Incidental.Chronicliverdisease.HDC.ICD10"],p=0.7,list = F)
traindata <- data2[inTrain,]
testdata <- data2[-inTrain,]
Protein_names <- c(result_sig$mediator)
Protein_names <- recode(Protein_names, !!!name_mapping)
length(Protein_names)  
model_xgboost = xgboost(
  data = as.matrix(traindata[,Protein_names]), 
  label = traindata$Incidental.Chronicliverdisease.HDC.ICD10,
  max_depth = 3,    
  eta = 0.3,        
  nthread = 2,      
  nrounds = 100,    
  early_stopping_rounds = 10,    
  objective = "binary:logistic"  
)
X_pred <- as.matrix(testdata[, Protein_names])
shp <- shapviz(model_xgboost, X_pred = X_pred)
pdf("D:/UKB_data/task/SI_outcome_wide/Protein_liver_shapplot.pdf")
sv_importance(shp, kind = "beeswarm") + theme_bw()
dev.off()



