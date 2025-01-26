rm(list=ls())
library(stringr)
library(TwoSampleMR)
library(MendelianRandomization)
library(cause)
require(GenomicSEM)
library(ggpubr)
library(ieugwasr)
library(MRPRESSO)
library(qqman)
library(VariantAnnotation)  
library(gwasglue)  
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
library(stats)
library(MungeSumstats)   
library(dplyr)
library(data.table)
library(gwasvcf)

#This codes will produce multivariable GWAS sumstats for latent phenotype called SOCIAL ISOLATION made of:
#Living alone + Little contact with family or friends + Fewer leisure/social activities
#Ref to the method: https://github.com/GenomicSEM/GenomicSEM/wiki/4.-Common-Factor-GWAS; to the paper: 10.1038/s41562-019-0566-x

setwd("D:/UKB_data/task/commonfactorGWAS/")
getwd()

#Step 1: Munge the summary statistics------------------------------------------------
Not_living_alone <- fread("ukb-b-5445.vcf.gz")
More_contact <- fread("ukb-b-5379.vcf.gz")
Fewer_activities <- read.csv("ukb-b-5076.vcf.gz")

#Not_living_alone
Not_living_alone_vcf=readVcf("ukb-b-5445.vcf.gz")
names(Not_living_alone_vcf) 
Not_living_alone_snp=gwasvcf_to_TwoSampleMR(Not_living_alone_vcf,type="exposure")
head(Not_living_alone_snp)
str(Not_living_alone_snp)
Not_living_alone_snp <- Not_living_alone_snp %>% dplyr::rename(A2 = other_allele.exposure,
                                                               A1 = effect_allele.exposure,
                                                               effect = beta.exposure,
                                                               P = pval.exposure,
                                                               SNP = SNP,
                                                               N = samplesize.exposure,
                                                               SE = se.exposure)
colnames(Not_living_alone_snp)
write.table(Not_living_alone_snp,"D:/UKB_data/task/commonfactorGWAS/Not_living_alone.txt")

#More_contact
More_contact_vcf=readVcf("D:/UKB_data/SI SNP/ukb-b-5379.vcf.gz")
names(More_contact_vcf)
More_contact_snp=gwasvcf_to_TwoSampleMR(More_contact_vcf,type="exposure")
head(More_contact_snp)
str(More_contact_snp)
More_contact_snp <- More_contact_snp %>% dplyr::rename(A2 = other_allele.exposure,
                                                       A1 = effect_allele.exposure,
                                                       effect = beta.exposure,
                                                       P = pval.exposure,
                                                       SNP = SNP,
                                                       N = samplesize.exposure,
                                                       SE = se.exposure)
colnames(More_contact_snp)
write.table(More_contact_snp,"D:/UKB_data/task/commonfactorGWAS/More_contact.txt")

#Fewer_activities
Fewer_activities_vcf=readVcf("D:/UKB_data/SI SNP/ukb-b-5076.vcf.gz")
names(Fewer_activities_vcf)   
Fewer_activities_snp=gwasvcf_to_TwoSampleMR(Fewer_activities_vcf,type="exposure")
head(Fewer_activities_snp)
str(Fewer_activities_snp)
Fewer_activities_snp <- Fewer_activities_snp %>% dplyr::rename(A2 = other_allele.exposure,
                                                               A1 = effect_allele.exposure,
                                                               effect = beta.exposure,
                                                               P = pval.exposure,
                                                               SNP = SNP,
                                                               N = samplesize.exposure,
                                                               SE = se.exposure)
colnames(Fewer_activities_snp)
write.table(Fewer_activities_snp,"D:/UKB_data/task/commonfactorGWAS/Fewer_activities.txt")

##SNP、A1、A2、effect、P、N、SE
Not_living_alone <- fread("Not_living_alone.txt")
More_contact <- fread("More_contact.txt")
Fewer_activities <- fread("Fewer_activities.txt")
#create vector of the summary statistics files
files <- c("Not_living_alone.txt","More_contact.txt","Fewer_activities.txt")
#using hapmap3
hm3 <- "eur_w_ld_chr/w_hm3.snplist"
#name the traits 
trait.names <- c("Not_living_alone","More_contact","Fewer_activities")
#list the sample sizes which associated with the traits
N = c(459988, 459830, 461369)
#define the imputation quality filter (default is to filter to SNPs with INFO > 0.9)
info.filter = 0.9
#define the MAF filter (default is to filter to SNPs with MAF > 0.01)
maf.filter = 0.01
#run munge
munge(files = files, hm3 = hm3, trait.names = trait.names, N = N, info.filter = info.filter, maf.filter = maf.filter)


#Step 2: Run multivariable LDSC-----------------------------------------------------------------------------
#run LDSC
traits <- c("Not_living_alone.sumstats.gz", "More_contact.sumstats.gz", "Fewer_activities.sumstats.gz")
sample.prev <- c(0.5, 0.5, 0.3)
population.prev <- c(0.2, 0.2, 0.2)
ld <- "eur_w_ld_chr/"
wld <- "eur_w_ld_chr/"
NMF_LDSCoutput <- ldsc(traits, sample.prev, population.prev, ld, wld, trait.names)
NMF_LDSCoutput$V #is the sampling covariance matrix in the format expected by lavaan
NMF_LDSCoutput$S #is the covariance matrix (on the liability scale for case/control designs)
NMF_LDSCoutput$m #number of SNPs used to construct the LD score
#Standard Errors
k <- nrow(NMF_LDSCoutput$S)
SE <- matrix(0, k, k)
SE[lower.tri(SE,diag=TRUE)] <- sqrt(diag(NMF_LDSCoutput$V))


##Step 3: Prepare the summary statistics for GWAS-------------------------------------------------------------
files <- c("Not_living_alone.txt","More_contact.txt","Fewer_activities.txt")
ref <- "reference.1000G.maf.0.005.txt/reference.1000G.maf.0.005.txt"
trait.names <- c("Not_living_alone","More_contact","Fewer_activities")
se.logit <- c(F,F,F)
OLS <- c(F,F,F)
info.filter <- 0.6
maf.filter <- 0.01
SI_sumstats <- sumstats(files=files, ref=ref, trait.names=trait.names, se.logit=se.logit, OLS=OLS, linprob=NULL,
                             N=NULL, betas=NULL, info.filter=info.filter, maf.filter=maf.filter, keep.indel=FALSE,
                             parallel=FALSE,cores=NULL)

##Step 4: Combine the summary statistics and LDSC output and run the common factor GWAS----------------------
NMF_factor <- commonfactorGWAS(covstruc = NMF_LDSCoutput, SNPs = SI_sumstats,
                               estimation = "DWLS", cores = NULL, toler = 1e-30, SNPSE = FALSE,
                               parallel = TRUE, GC="standard", MPI=FALSE)


