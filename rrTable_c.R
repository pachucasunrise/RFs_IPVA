################################################################################
## Project: Risk factors for IPVA
## Script purpose: Function for creating tables of RRs using the ACE complete case data 
## (i.e. only one dataset, not the first tranche of imputed datasets), split by sex
## Date: 07.01.20
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

require(labelled)
require(dplyr)
require(mice)
require(magrittr)
require(tableone)

#source("./useful-code-r/code/functions/match-lab.r")

rrTable_cc <- function(orig_boys, orig_girls, outcome1, outcome2, vars, digits){

	#To help with calculating prevalence later
	orig_boys[,c("one")] <- rep(1, dim(orig_boys)[1])
	orig_girls[,c("one")] <- rep(1, dim(orig_girls)[1])

	stats <- print(
	           CreateCatTable(vars = vars, data = orig_boys, 
	           	 includeNA = TRUE),
	             showAllLevels = TRUE,
	             #quote = TRUE,
	             test = FALSE,
	             format = "f")

	# Number of variables in rfs that have an 'NA' value (because no imputation)
	num_row <- sum(sapply(vars, function(x) length(unique(orig_boys[,x]#,exclude=NULL
		))))+1

	out <- data.frame(matrix(NA, nrow = num_row, ncol = 14))
	colnames(out) <- c(rep("",2),"Males",rep("",5),"Females",rep("",5))
	out[1, ] <- c("Variable","Level",rep(c("Vic RR","LCI","UCI","Perp RR","CI","UCI"),2))

	# Don't want to bother with 'n' row
	length <- length(dimnames(stats)[[1]])
	out[2:num_row, 1] <- dimnames(stats)[[1]][2:length]
	out[2:num_row, 2] <- stats[, 1][2:length]

	#To determine row number for output
	level <- 2

	for (r in vars){
		length <- length(levels(droplevels(addNA(orig_boys[,r]))))
		baseline <- min(levels(as.factor(orig_boys[,r])))
		for (l in 1:(length-2)){
			model <- paste0(outcome1," ~ addNA(",r,")")

			#Boys vic
			fit <- glm(as.formula(model), family=binomial(link = "logit"), data = orig_boys)
			out[level+l, 3] <- exp(summary(fit)$coefficient[1+l,1])
			out[level+l, 4] <- exp(summary(fit)$coefficient[1+l,1]-1.96*summary(fit)$coefficient[1+l,2])
			out[level+l, 5] <- exp(summary(fit)$coefficient[1+l,1]+1.96*summary(fit)$coefficient[1+l,2])

			#06.01.20 - want to use baseline prevalence to approximate RR
			#As per Zhang et al: https://jamanetwork.com/journals/jama/fullarticle/188182
			num <- aggregate(orig_boys[,outcome1], by=list(orig_boys[,r]), FUN=sum)
			denom <- aggregate(orig_boys$one, by=list(orig_boys[,r]), FUN=sum)[,2]
			num[,2] <- num[,2]/denom
			p0 <- num[num[,1]==baseline,2]
			out[level+l, 3] <- round(as.numeric(out[level+l, 3])/(1-p0+(p0*as.numeric(out[level+l, 3]))), digits=digits)
			out[level+l, 4] <- round(as.numeric(out[level+l, 4])/(1-p0+(p0*as.numeric(out[level+l, 4]))), digits=digits)
			out[level+l, 5] <- round(as.numeric(out[level+l, 5])/(1-p0+(p0*as.numeric(out[level+l, 5]))), digits=digits)

			#Girls vic
			fit <- glm(as.formula(model), family=binomial(link = "logit"), data = orig_girls)
			out[level+l, 9] <- exp(summary(fit)$coefficient[1+l,1])
			out[level+l, 10] <- exp(summary(fit)$coefficient[1+l,1]-1.96*summary(fit)$coefficient[1+l,2])
			out[level+l, 11] <- exp(summary(fit)$coefficient[1+l,1]+1.96*summary(fit)$coefficient[1+l,2])
			num <- aggregate(orig_girls[,outcome1], by=list(orig_girls[,r]), FUN=sum)
			denom <- aggregate(orig_girls$one, by=list(orig_girls[,r]), FUN=sum)[,2]
			num[,2] <- num[,2]/denom
			p0 <- num[num[,1]==baseline,2]
			out[level+l, 9] <- round(as.numeric(out[level+l, 9])/(1-p0+(p0*as.numeric(out[level+l, 9]))), digits=digits)
			out[level+l, 10] <- round(as.numeric(out[level+l, 10])/(1-p0+(p0*as.numeric(out[level+l, 10]))), digits=digits)
			out[level+l, 11] <- round(as.numeric(out[level+l, 11])/(1-p0+(p0*as.numeric(out[level+l, 11]))), digits=digits)

			#Perp
			model <- paste0(outcome2," ~ addNA(",r,")")
			#Boys perp
			fit <- glm(as.formula(model), family=binomial(link = "logit"), data = orig_boys)
			out[level+l, 6] <- exp(summary(fit)$coefficient[1+l,1])
			out[level+l, 7] <- exp(summary(fit)$coefficient[1+l,1]-1.96*summary(fit)$coefficient[1+l,2])
			out[level+l, 8] <- exp(summary(fit)$coefficient[1+l,1]+1.96*summary(fit)$coefficient[1+l,2])
			num <- aggregate(orig_boys[,outcome1], by=list(orig_boys[,r]), FUN=sum)
			denom <- aggregate(orig_boys$one, by=list(orig_boys[,r]), FUN=sum)[,2]
			num[,2] <- num[,2]/denom
			p0 <- num[num[,1]==baseline,2]
			out[level+l, 6] <- round(as.numeric(out[level+l, 6])/(1-p0+(p0*as.numeric(out[level+l, 6]))), digits=digits)
			out[level+l, 7] <- round(as.numeric(out[level+l, 7])/(1-p0+(p0*as.numeric(out[level+l, 7]))), digits=digits)
			out[level+l, 8] <- round(as.numeric(out[level+l, 8])/(1-p0+(p0*as.numeric(out[level+l, 8]))), digits=digits)

			#Girls perp
			fit <- glm(as.formula(model), family=binomial(link = "logit"), data = orig_girls)
			out[level+l, 12] <- exp(summary(fit)$coefficient[1+l,1])
			out[level+l, 13] <- exp(summary(fit)$coefficient[1+l,1]-1.96*summary(fit)$coefficient[1+l,2])
			out[level+l, 14] <- exp(summary(fit)$coefficient[1+l,1]+1.96*summary(fit)$coefficient[1+l,2])
			num <- aggregate(orig_girls[,outcome1], by=list(orig_girls[,r]), FUN=sum)
			denom <- aggregate(orig_girls$one, by=list(orig_girls[,r]), FUN=sum)[,2]
			num[,2] <- num[,2]/denom
			p0 <- num[num[,1]==baseline,2]
			out[level+l, 12] <- round(as.numeric(out[level+l, 12])/(1-p0+(p0*as.numeric(out[level+l, 12]))), digits=digits)
			out[level+l, 13] <- round(as.numeric(out[level+l, 13])/(1-p0+(p0*as.numeric(out[level+l, 13]))), digits=digits)
			out[level+l, 14] <- round(as.numeric(out[level+l, 14])/(1-p0+(p0*as.numeric(out[level+l, 14]))), digits=digits)
			}
		out[(out[,1] == as.character(r) & out[,2] == levels(droplevels(addNA(orig_boys[,r])))[1]), 3] <- "ref"

		level <- level+length
		}

	out[is.na(out[,2])==TRUE,2] <- "Not reported"
	out[out[,2]=="0",2] <- "No"
	out[out[,2]=="1",2] <- "Yes"

	#Drop those levels that have no rows
	out <- out[is.na(out[,3])==FALSE,]

	return(out)
	}