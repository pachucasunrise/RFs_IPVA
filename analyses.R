################################################################################
## Project: Risk factors for IPVA
## Script purpose: Taking final dataset (that includes IPVA and potential risk/
## protective factors) and creating figures and tables for Wellcome Open 
## Research paper
## Note: Figure 1 derived separately in Excel
## Date: 26th September 2019
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

################################################################################
# 0. Locations, packages, functions
cd "C:/Program Files/R/R-3.5.3/bin"
R

packages<-c('readstata13','data.table','tidyr','formattable','tidyverse',
	'dplyr','gdata','foreign','readxl','matrixStats','tableone','Rcmdr','mice',
	'magrittr','varhandle','zoo','mice','backports','tableone')

source("http://bioconductor.org/biocLite.R")

for(pkg in packages){
  if(!require(pkg,character.only=TRUE)){
    BiocInstaller::biocLite(pkg,suppressUpdates=TRUE)
    library(pkg,character.only=TRUE)
    }
  }

rm(pkg,packages)


################################################################################

## ---- Clear global R environment----------------------------------------------
rm(list=ls())


## ---- Location files----------------------------------------------------------
#The following filepaths have been removed for data security purposes
#loc_inp<-[input location]
#loc_out<-[output location]
#loc_func<-[functions location]

setwd(loc_out)


################################################################################
# 1. Get and prep data  
################################################################################
# Load data
load(paste0(loc_inp,'cohort/imp_boys_RFs.RData'))
load(paste0(loc_inp,'cohort/imp_girls_RFs.RData'))

# Declare which risk factors will be studied throughout, and in what order
rfs <- c("imd_2010","ethnicity","sex_min","patmon_score",
	#MFQ depression scores
	"mfq_total_ccs","mfq_total_cct",
	"anxiety_tf3","anxiety_tf4","sh_ccs","sh_tf4","asb_ccq","asb_cct",
	"smok_ccs","smok_cct","alc_score","cann_ccs","cann_cct",
	"drug_ccs","drug_cct","rsb","hosp",
	#Education scores
	"score_13","gcse_16",
	"neet_cct","neet_ccu")

ace <- c("emotional_abuse","physical_abuse","sexual_abuse","emotional_neglect","bullying",
	"violence_between_parents","substance_household","mental_health_problems_or_suicide",
	"parent_convicted_offence","parental_separation")
#ace_c = any classic ACE, ace_no = no. of ACEs
aces <- c("ace_c",ace,"ace_no")

# Relevel ethnicity
imp_boys$data$ethnicity <- relevel(factor(imp_boys$data$ethnicity),ref="white")
imp_girls$data$ethnicity <- relevel(factor(imp_girls$data$ethnicity),ref="white")


## ---- Create long form of imputed data ---------------------------------------
imp_boys.long <- mice::complete(imp_boys, action = 'long', inc = TRUE)
imp_girls.long <- mice::complete(imp_girls, action = 'long', inc = TRUE)
dim(imp_boys.long[imp_boys.long$.imp==1,]);dim(imp_girls.long[imp_girls.long$.imp==1,])

## ---- Generate binary variables from scores ----------------------------------------
#Extreme Parental monitoring
imp_boys$data$patmon_score[imp_boys$data$patmon_score<=13] <- 0
imp_boys$data$patmon_score[imp_boys$data$patmon_score>13] <- 1
imp_boys$imp$patmon_score[imp_boys$imp$patmon_score<=13] <- 0
imp_boys$imp$patmon_score[imp_boys$imp$patmon_score>13] <- 1
imp_girls$data$patmon_score[imp_girls$data$patmon_score<=13] <- 0
imp_girls$data$patmon_score[imp_girls$data$patmon_score>13] <- 1
imp_girls$imp$patmon_score[imp_girls$imp$patmon_score<=13] <- 0
imp_girls$imp$patmon_score[imp_girls$imp$patmon_score>13] <- 1

imp_boys.long$patmon_score[imp_boys.long$patmon_score<=13] <- 0
imp_boys.long$patmon_score[imp_boys.long$patmon_score>13] <- 1
imp_boys.long$patmon_score[is.na(imp_boys.long$patmon_score)==TRUE] <- "NA"
imp_girls.long$patmon_score[imp_girls.long$patmon_score<=13] <- 0
imp_girls.long$patmon_score[imp_girls.long$patmon_score>13] <- 1
imp_girls.long$patmon_score[is.na(imp_girls.long$patmon_score)==TRUE] <- "NA"

#Depress CCS
imp_boys$data$mfq_total_ccs[imp_boys$data$mfq_total_ccs<=11] <- 0
imp_boys$data$mfq_total_ccs[imp_boys$data$mfq_total_ccs>11] <- 1
imp_boys$imp$mfq_total_ccs[imp_boys$imp$mfq_total_ccs<=11] <- 0
imp_boys$imp$mfq_total_ccs[imp_boys$imp$mfq_total_ccs>11] <- 1
imp_girls$data$mfq_total_ccs[imp_girls$data$mfq_total_ccs<=11] <- 0
imp_girls$data$mfq_total_ccs[imp_girls$data$mfq_total_ccs>11] <- 1
imp_girls$imp$mfq_total_ccs[imp_girls$imp$mfq_total_ccs<=11] <- 0
imp_girls$imp$mfq_total_ccs[imp_girls$imp$mfq_total_ccs>11] <- 1

imp_boys.long$mfq_total_ccs[imp_boys.long$mfq_total_ccs<=11] <- 0
imp_boys.long$mfq_total_ccs[imp_boys.long$mfq_total_ccs>11] <- 1
imp_boys.long$mfq_total_ccs[is.na(imp_boys.long$mfq_total_ccs)==TRUE] <- "NA"
imp_girls.long$mfq_total_ccs[imp_girls.long$mfq_total_ccs<=11] <- 0
imp_girls.long$mfq_total_ccs[imp_girls.long$mfq_total_ccs>11] <- 1
imp_girls.long$mfq_total_ccs[is.na(imp_girls.long$mfq_total_ccs)==TRUE] <- "NA"

#Depress CCT
imp_boys$data$mfq_total_cct[imp_boys$data$mfq_total_cct<=11] <- 0
imp_boys$data$mfq_total_cct[imp_boys$data$mfq_total_cct>11] <- 1
imp_boys$imp$mfq_total_cct[imp_boys$imp$mfq_total_cct<=11] <- 0
imp_boys$imp$mfq_total_cct[imp_boys$imp$mfq_total_cct>11] <- 1
imp_girls$data$mfq_total_cct[imp_girls$data$mfq_total_cct<=11] <- 0
imp_girls$data$mfq_total_cct[imp_girls$data$mfq_total_cct>11] <- 1
imp_girls$imp$mfq_total_cct[imp_girls$imp$mfq_total_cct<=11] <- 0
imp_girls$imp$mfq_total_cct[imp_girls$imp$mfq_total_cct>11] <- 1

imp_boys.long$mfq_total_cct[imp_boys.long$mfq_total_cct<=11] <- 0
imp_boys.long$mfq_total_cct[imp_boys.long$mfq_total_cct>11] <- 1
imp_boys.long$mfq_total_cct[is.na(imp_boys.long$mfq_total_cct)==TRUE] <- "NA"
imp_girls.long$mfq_total_cct[imp_girls.long$mfq_total_cct<=11] <- 0
imp_girls.long$mfq_total_cct[imp_girls.long$mfq_total_cct>11] <- 1
imp_girls.long$mfq_total_cct[is.na(imp_girls.long$mfq_total_cct)==TRUE] <- "NA"

#Alc score
imp_boys$data$alc_score[imp_boys$data$alc_score<=7] <- 0
imp_boys$data$alc_score[imp_boys$data$alc_score>7] <- 1
imp_boys$imp$alc_score[imp_boys$imp$alc_score<=7] <- 0
imp_boys$imp$alc_score[imp_boys$imp$alc_score>7] <- 1
imp_girls$data$alc_score[imp_girls$data$alc_score<=7] <- 0
imp_girls$data$alc_score[imp_girls$data$alc_score>7] <- 1
imp_girls$imp$alc_score[imp_girls$imp$alc_score<=7] <- 0
imp_girls$imp$alc_score[imp_girls$imp$alc_score>7] <- 1

imp_boys.long$alc_score[imp_boys.long$alc_score<=7] <- 0
imp_boys.long$alc_score[imp_boys.long$alc_score>7] <- 1
imp_boys.long$alc_score[is.na(imp_boys.long$alc_score)==TRUE] <- "NA"
imp_girls.long$alc_score[imp_girls.long$alc_score<=7] <- 0
imp_girls.long$alc_score[imp_girls.long$alc_score>7] <- 1
imp_girls.long$alc_score[is.na(imp_girls.long$alc_score)==TRUE] <- "NA"

#Educ score
imp_boys$data$score_13_fi[imp_boys$data$score_13_fi<=116] <- 0
imp_boys$data$score_13_fi[imp_boys$data$score_13_fi>116] <- 1
imp_boys$imp$score_13_fi[imp_boys$imp$score_13_fi<=116] <- 0
imp_boys$imp$score_13_fi[imp_boys$imp$score_13_fi>116] <- 1
imp_girls$data$score_13_fi[imp_girls$data$score_13_fi<=116] <- 0
imp_girls$data$score_13_fi[imp_girls$data$score_13_fi>116] <- 1
imp_girls$imp$score_13_fi[imp_girls$imp$score_13_fi<=116] <- 0
imp_girls$imp$score_13_fi[imp_girls$imp$score_13_fi>116] <- 1

imp_boys.long$score_13_fi[imp_boys.long$score_13_fi<=116] <- 0
imp_boys.long$score_13_fi[imp_boys.long$score_13_fi>116] <- 1
imp_boys.long$score_13_fi[is.na(imp_boys.long$score_13_fi)==TRUE] <- "NA"
imp_girls.long$score_13_fi[imp_girls.long$score_13_fi<=116] <- 0
imp_girls.long$score_13_fi[imp_girls.long$score_13_fi>116] <- 1
imp_girls.long$score_13_fi[is.na(imp_girls.long$score_13_fi)==TRUE] <- "NA"


# Now rel_ind_021, rsb_fi and hosp_fi which are currently 1 or NA
imp_boys$data$rel_ind_021[is.na(imp_boys$data$rel_ind_021)==TRUE] <- "0"
imp_boys$imp$rel_ind_021[is.na(imp_boys$data$rel_ind_021)==TRUE] <- "0"
table(imp_boys.long$rel_ind_021, exclude=NULL)
levels(imp_boys.long$rel_ind_021) <- c("0","1")
imp_boys.long$rel_ind_021[is.na(imp_boys.long$rel_ind_021)==TRUE] <- "0"
table(imp_boys.long$rel_ind_021, exclude=NULL)

imp_boys$data$rel_def_017[is.na(imp_boys$data$rel_def_017)==TRUE] <- "0"
imp_boys$imp$rel_def_017[is.na(imp_boys$data$rel_def_017)==TRUE] <- "0"
levels(imp_boys.long$rel_def_017) <- c("0","1")
imp_boys.long$rel_def_017[is.na(imp_boys.long$rel_def_017)==TRUE] <- "0"
levels(imp_boys.long$rel_ind_017) <- c("0","1")
imp_boys.long$rel_ind_017[is.na(imp_boys.long$rel_ind_017)==TRUE] <- "0"

imp_girls$data$rel_ind_021[is.na(imp_girls$data$rel_ind_021)==TRUE] <- "0"
imp_girls$imp$rel_ind_021[is.na(imp_girls$data$rel_ind_021)==TRUE] <- "0"
levels(imp_girls.long$rel_ind_021) <- c("0","1")
imp_girls.long$rel_ind_021[is.na(imp_girls.long$rel_ind_021)==TRUE] <- "0"

imp_girls$data$rel_def_017[is.na(imp_girls$data$rel_def_017)==TRUE] <- "0"
imp_girls$imp$rel_def_017[is.na(imp_girls$data$rel_def_017)==TRUE] <- "0"
levels(imp_girls.long$rel_def_017) <- c("0","1")
imp_girls.long$rel_def_017[is.na(imp_girls.long$rel_def_017)==TRUE] <- "0"
levels(imp_girls.long$rel_ind_017) <- c("0","1")
imp_girls.long$rel_ind_017[is.na(imp_girls.long$rel_ind_017)==TRUE] <- "0"

table(imp_boys.long$rsb_fi, exclude=NULL)
table(imp_boys.long$hosp_fi, exclude=NULL)

#Also have a version where boys and girls together
imp_all<-mice::rbind(imp_boys,imp_girls)
imp_all.long <- mice::complete(imp_all, action = 'long', inc = TRUE)


################################################################################
# 2. Results to describe within paper text
################################################################################

## How many definitely (or indicate) in a relationship by 17, 
##and 21, respectively? --
num_boys <- table(imp_boys$data$rel_def_017, exclude=NULL)[2]
denom_boys <- table(imp_boys$data$rel_def_017, exclude=NULL)[1]+num_boys
num_girls <- table(imp_girls$data$rel_def_017, exclude=NULL)[2]
denom_girls <- table(imp_girls$data$rel_def_017, exclude=NULL)[1]+num_girls
num_boys/denom_boys
num_girls/denom_girls
(num_boys+num_girls)/(denom_boys+denom_girls)

num_boys <- table(imp_boys$data$rel_ind_017, exclude=NULL)[2]
denom_boys <- table(imp_boys$data$rel_ind_017, exclude=NULL)[1]+num_boys
num_girls <- table(imp_girls$data$rel_ind_017, exclude=NULL)[2]
denom_girls <- table(imp_girls$data$rel_ind_017, exclude=NULL)[1]+num_girls
num_boys/denom_boys
num_girls/denom_girls
(num_boys+num_girls)/(denom_boys+denom_girls)

num_boys <- table(imp_boys$data$rel_ind_021, exclude=NULL)[2]
denom_boys <- table(imp_boys$data$rel_ind_021, exclude=NULL)[1]+num_boys
num_girls <- table(imp_girls$data$rel_ind_021, exclude=NULL)[2]
denom_girls <- table(imp_girls$data$rel_ind_021, exclude=NULL)[1]+num_girls
num_boys/denom_boys
num_girls/denom_girls
(num_boys+num_girls)/(denom_boys+denom_girls)

#Keep denominators for later
rm(num_girls,num_boys)


## ---- For in text: Numbers in a relationship by all potential risk factors ------
num_row <- sum(sapply(c(rfs,aces
			), function(x) length(unique(imp_boys.long[imp_all.long$.imp==0,x],
				exclude=NULL))))+2

sex_stats <- print(
	           CreateCatTable(vars = c(rfs #,aces
	           	), data = imp_all.long[imp_all.long$.imp==0,], 
	           	 strata = c("kz021"), includeNA = TRUE),
	             showAllLevels = TRUE,
	             includeNA = TRUE,
	             #quote = TRUE,
	             test = FALSE,
	             format = "f") 

rel_stats <- print(
	           CreateCatTable(vars = c(rfs #,aces
	           	), data = imp_all.long[imp_all.long$.imp==0,], 
		           	strata = c("kz021", "rel_ind_021"), 
		           	includeNA = TRUE),
	            showAllLevels = TRUE,
	            #quote = TRUE,
	            test = FALSE,
	            format = "f")

out <- data.frame(matrix(NA, nrow = num_row, ncol = 8))
colnames(out) <- c(rep("",2),"Males",rep("",2),"Females",rep("",2))
out[1, ] <- c("Variable","Level",rep(c("Total","Relationship",""),2))
out[2:num_row, 1] <- dimnames(sex_stats)[[1]]
out[2:num_row, 2] <- rel_stats[, 1]
out[2:num_row, 3] <- sex_stats[, 2]
out[2:num_row, 4] <- rel_stats[, 4]
out[2:num_row, 5] <- round(as.numeric(out[2:num_row, 4])/
	as.numeric(out[2:num_row, 3])*100, digits = 1)
out[2:num_row, 6] <- sex_stats[, 3]
out[2:num_row, 7] <- rel_stats[, 5]
out[2:num_row, 8] <- round(as.numeric(out[2:num_row, 7])/
	as.numeric(out[2:num_row, 6])*100, digits = 1)

write.csv(out, "RFs_relationship_table.csv", row.names = TRUE)
rm(num_row, rel_stats, sex_stats, out)


#What proportions of men and women reported exclusively emotional vic(/perp), 
#respectively?
num <- table(imp_boys.long[imp_boys.long$.imp==0 & imp_boys.long$vic_phys_021==0 
	& imp_boys.long$vic_sex_021==0,"vic_emo_021"],exclude=NULL)[2]
denom_boys <- table(imp_boys.long[imp_boys.long$.imp==0,"vic_emo_021"],
	exclude=NULL)[1]+table(imp_boys.long[imp_boys.long$.imp==0,"vic_emo_021"],
	exclude=NULL)[2]
num/denom_boys

num <- table(imp_girls.long[imp_girls.long$.imp==0 & imp_girls.long$vic_phys_021==0 
	& imp_girls.long$vic_sex_021==0,"vic_emo_021"],exclude=NULL)[2]
denom_girls <- table(imp_girls.long[imp_girls.long$.imp==0,"vic_emo_021"],
	exclude=NULL)[1]+table(imp_girls.long[imp_girls.long$.imp==0,"vic_emo_021"],exclude=NULL)[2]
num/denom_girls

num <- table(imp_boys.long[imp_boys.long$.imp==0 & imp_boys.long$per_phys_021==0 
	& imp_boys.long$per_sex_021==0,"per_emo_021"],exclude=NULL)[2]
num/denom_boys

num <- table(imp_girls.long[imp_girls.long$.imp==0 & imp_girls.long$per_phys_021==0 
	& imp_girls.long$per_sex_021==0,"per_emo_021"],exclude=NULL)[2]
num/denom_girls

#What numbers and proportions reported being a victim(/perp) both before and after turning 18 years old?
num_boys <- table(imp_boys.long[imp_boys.long$.imp==0 & imp_boys.long$vic_017==1,
	"vic_1821"],exclude=NULL)[2]
num_boys/denom_boys

num_girls <- table(imp_girls.long[imp_girls.long$.imp==0 & imp_girls.long$vic_017==1,
	"vic_1821"],exclude=NULL)[2]
num_girls/denom_girls

num_boys+num_girls
(num_boys+num_girls)/(denom_boys+denom_girls)

num_boys <- table(imp_boys.long[imp_boys.long$.imp==0 & imp_boys.long$per_017==1,
	"per_1821"],exclude=NULL)[2]
num_boys/denom_boys

num_girls <- table(imp_girls.long[imp_girls.long$.imp==0 & imp_girls.long$per_017==1,
	"per_1821"],exclude=NULL)[2]
num_girls/denom_girls

num_boys+num_girls
(num_boys+num_girls)/(denom_boys+denom_girls)

#What proportions reported being exposed to both vic and perp at 0-21 years old?
num_boys <- table(imp_boys.long[imp_boys.long$.imp==0 & imp_boys.long$vic_021==1,
	"per_021"],exclude=NULL)[2]
num_boys/denom_boys

num_girls <- table(imp_girls.long[imp_girls.long$.imp==0 & imp_girls.long$vic_021==1,
	"per_021"],exclude=NULL)[2]
num_girls/denom_girls

num_boys+num_girls
(num_boys+num_girls)/(denom_boys+denom_girls)


################################################################################
# 3. Tables
################################################################################

## ---- Table 1: Prevalence of victimisation and perpetration sub-types by sex 
## and age at when it was reported ---------------------------------------------
subtypes<- c("V: Any","V: Emotional","V: Physical","V: Sexual",
			 "P: Any","P: Emotional","P: Physical","P: Sexual")
num_row <- length(subtypes)+1
out <- data.frame(matrix(NA, nrow = num_row, ncol = 13))
colnames(out) <- c("","Males",rep("",5),"Females",rep("",5))
out[1, ] <- c("",rep(c("0-17y","","18-21y","","0-21y",""),2))
out[2:num_row, 1] <- subtypes

age <- c("017","1821","021")

count <- 2
count2 <- 2
for(a in age){
	type <- c(paste0("vic_",a),paste0("vic_emo_",a),paste0("vic_phys_",a),
				paste0("vic_sex_",a),paste0("per_",a),paste0("per_emo_",a),
				paste0("per_phys_",a),paste0("per_sex_",a))
	for(t in type){
		out[count, c(count2,count2+6)] <- table(imp_all.long[
			imp_all.long$.imp==0,t], 
			imp_all.long[imp_all.long$.imp==0,"kz021"])[2,1:2]
		count <- count+1
		}
		count <- 2
		count2 <- count2+2
	}
out[2:num_row, c(3,5,7)] <- round(as.numeric(unlist(
	out[2:num_row, c(2,4,6)]))/denom_boys*100, digits = 1)
out[2:num_row, c(9,11,13)] <- round(as.numeric(unlist(
	out[2:num_row, c(8,10,12)]))/denom_girls*100, digits = 1)

write.csv(out, "vic_per_table.csv", row.names = TRUE)
rm(subtypes, num_row, count, count2, a, age, t, type, out)


## ---- Table 2: Relative risks of interpersonal-violence and abuse (IPVA) by 21 
#years old by socio-demographic/clinical variables and sex. ---------------------
#Function to make RR tables
source(paste0(loc_func,'rrTable.R'))

out_rfs_mi <- rrTable(imp_boys,imp_boys.long,imp_girls,imp_girls.long,"vic_021",
	"per_021",rfs,2)
write.csv(out_rfs_mi, "rfs_vic_perp_mi_021.csv", row.names = FALSE, na = "")
#Replace object with numbers to 10 decimal places so can calculate change in 
#coefficients later
out_rfs_mi <- rrTable(imp_boys,imp_boys.long,imp_girls,imp_girls.long,"vic_021",
	"per_021",rfs,2)


## ---- Table 3: Relative risks of interpersonal-violence and abuse (IPVA) by 21 
#years old by Adverse Childhood Experiences (ACEs)and sex -----------------------
out_aces_mi <- rrTable(imp_boys,imp_boys.long,imp_girls,imp_girls.long,"vic_021",
	"per_021",aces,2)
write.csv(out_aces_mi, "aces_vic_perp_mi_021.csv", row.names = FALSE, na = "")
out_aces_mi <- rrTable(imp_boys,imp_boys.long,imp_girls,imp_girls.long,"vic_021",
	"per_021",aces,10)

## ---- Extended Data Table A: ALSPAC study questions/responses used to capture 
#romantic relationships ----------


## ---- Extended Data Table B: Details about study variables of interest -------


## ---- Extended Data Box A: Notes on imputation -------------------------------


## ---- Extended Data Table C: Cohort Characteristics --------------------------
sex_stats <- print(
	           CreateCatTable(vars = c(rfs, aces
	           	), data = imp_all.long[imp_all.long$.imp==0,], strata = c("kz021"), includeNA = TRUE),
	             showAllLevels = TRUE,
	             includeNA = TRUE,
	             #quote = TRUE,
	             test = FALSE)

write.csv(sex_stats, "baseline_characs.csv", row.names = TRUE)
rm(sex_stats)


## ---- Extended Data Table D: Prevalence of Interpersonal Violence and Abuse 
#(IPVA) victimisation and perpetration by socio-demographic/ clinical variables 
#and sex -------------------------------------------------------------------
source(paste0(loc_func,'prevTable.R'))
out <- prevTable(imp_boys.long,imp_girls.long,rfs)
write.csv(out, "rfs_vic_perp_mi_props.csv", row.names = FALSE, na = "")


## ---- Extended Data Table E: Prevalence of Interpersonal Violence and Abuse (IPVA) 
#victimisation and perpetration by Adverse Childhood Experiences (ACEs), age at when 
#IPVA occurred, and sex
out <- prevTable(imp_boys.long,imp_girls.long,aces)
write.csv(out, "aces_vic_perp_mi_props.csv", row.names = FALSE, na = "")


## ---- Extended Data Table F: Relative risks of interpersonal-violence and abuse 
#(IPVA) by 21 years old by socio-demographic/clinical variables and sex (missing 
#risk factor data not imputed)
source(paste0(loc_func,'rrTable_c.R'))
orig_boys <- imp_boys.long[imp_boys.long$.imp == 0,]
orig_girls <- imp_girls.long[imp_girls.long$.imp == 0,]
out_rfs_cca <- rrTable_c(orig_boys,orig_girls,"vic_021","per_021",rfs,2)
write.csv(out_rfs_cca, "rfs_vic_perp_cca_021.csv", row.names = FALSE, na = "")


################################################################################
# 4. Additional checks
################################################################################

## ---- % change in coefficients for RFs between mi and cca --------------------

#Note that dimensions are diff between mi and cca tables
out_rfs_mi <- rrTable(imp_boys,imp_boys.long,imp_girls,imp_girls.long,"vic_021",
	"per_021",rfs,10)
out_rfs_cca <- rrTable_c(imp_boys.long,imp_girls.long,"vic_021","per_021",rfs,10)

source("O:/Training/R/change_coef.R")
out_rfs_change <- change_coef(out_rfs_mi,out_rfs_cca,2)
write.csv(out_rfs_change, "rfs_change_coefs.csv", row.names = FALSE, na = "")

change_prop <- c(out_rfs_change[,11],out_rfs_change[,12],out_rfs_change[,13],
	out_rfs_change[,14])
summary(change_prop)


## ---- Check distribution of observed and imputed variables -------------------------------------------------------------------
sex_stats_cca <- print(
	           CreateCatTable(vars = rfs, data = imp_all.long[imp_all.long$.imp==0,], 
	           	 strata = c("kz021"), includeNA = FALSE),
	             showAllLevels = TRUE,
	             #quote = TRUE,
	             test = FALSE,
	             format = "p")

boys_cca <- data.frame(matrix(NA, nrow = dim(sex_stats_cca)[1], ncol = 3))
girls_cca <- data.frame(matrix(NA, nrow = dim(sex_stats_cca)[1], ncol = 3))

boys_cca[,1] <- dimnames(sex_stats_cca)[[1]]
boys_cca[,2:3] <- sex_stats_cca[,1:2]
colnames(boys_cca) <- c("Var","Level","obs_prop")
girls_cca[,1] <- dimnames(sex_stats_cca)[[1]]
girls_cca[,2:3] <- sex_stats_cca[,c(1,3)]
colnames(girls_cca) <- c("Var","Level","obs_prop")

no_imp <- 45
count <- c(1:no_imp)
for (c in count){
	    	assign(paste0("sex_stats_mi",c), 
			value = print(CreateCatTable(vars = rfs, data = imp_all.long[imp_all.long$.imp==c,], 
	       	 strata = c("kz021"), includeNA = FALSE),
	         showAllLevels = TRUE,
	         #quote = TRUE,
	         test = FALSE,
	         format = "p"))
	}

boys_mi <- data.frame(matrix(NA, nrow = dim(sex_stats_mi1)[1], ncol = (no_imp+3)))
girls_mi <- data.frame(matrix(NA, nrow = dim(sex_stats_mi1)[1], ncol = (no_imp+3)))

boys_mi[,1] <- dimnames(sex_stats_mi1)[[1]]
boys_mi[,2:3] <- sex_stats_mi1[,1:2]
girls_mi[,1] <- dimnames(sex_stats_mi1)[[1]]
girls_mi[,2:3] <- sex_stats_mi1[,c(1,3)]

names <- NULL
for (c in count){
	table <- paste0("sex_stats_mi",c)
	boys_mi[,c+2] <- as.numeric(get(table)[,2])
	girls_mi[,c+2] <- as.numeric(get(table)[,3])
	names <- c(names,paste0("mi",c))
	}

colnames(boys_mi) <- c("Var","Level",names,"ave_mi_prop")
colnames(girls_mi) <- c("Var","Level",names,"ave_mi_prop")

for (n in names){
	boys_mi[,n] < as.double(boys_mi[,n])
	girls_mi[,n] < as.double(girls_mi[,n])
	}

boys_mi[,"ave_mi_prop"] <- round(rowMeans(boys_mi[,names]),digits=1)
girls_mi[,"ave_mi_prop"] <- round(rowMeans(girls_mi[,names]),digits=1)

#Now merge observed and completed
boys_mi[,1] <- ifelse(as.character(boys_mi[,1])!="", boys_mi[,1], NA)
boys_mi[,1] <- na.locf(boys_mi[,1])
boys_cca[,1] <- ifelse(as.character(boys_cca[,1])!="", boys_cca[,1], NA)
boys_cca[,1] <- na.locf(boys_cca[,1])
boys_merge <- merge(boys_mi, boys_cca, by=c("Var","Level"))
boys_merge <- boys_merge[,c("Var","Level","ave_mi_prop","obs_prop")]
write.csv(boys_merge, "props_obs_vs_mi_boys.csv", row.names = FALSE, na = "")

girls_mi[,1] <- ifelse(as.character(girls_mi[,1])!="", girls_mi[,1], NA)
girls_mi[,1] <- na.locf(girls_mi[,1])
girls_cca[,1] <- ifelse(as.character(girls_cca[,1])!="", girls_cca[,1], NA)
girls_cca[,1] <- na.locf(girls_cca[,1])
girls_merge <- merge(girls_mi, girls_cca, by=c("Var","Level"))
girls_merge <- girls_merge[,c("Var","Level","ave_mi_prop","obs_prop")]
write.csv(girls_merge, "props_obs_vs_mi_girls.csv", row.names = FALSE, na = "")


## ---- end -------------------------------------------------------------------