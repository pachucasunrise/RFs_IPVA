################################################################################
## Project: Risk factors for IPVA
## Script purpose: Function for creating tables of RRs using complete case data, 
##split by sex
## Date: 6th June 2019
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

require(labelled)
require(dplyr)
require(mice)
require(magrittr)
require(tableone)

#source("./useful-code-r/code/functions/match-lab.r")

rrTable_cc <- function(imp_boys.long, imp_girls.long, outcome1, outcome2, vars, 
	digits){

	orig_boys <- imp_boys.long[imp_boys.long$.imp == 0,]
	orig_girls <- imp_girls.long[imp_girls.long$.imp == 0,]


	stats <- print(
	           CreateCatTable(vars = vars, data = orig_boys, 
	           	 includeNA = TRUE),
	             showAllLevels = TRUE,
	             #quote = TRUE,
	             test = FALSE,
	             format = "f")

	num_row <- sum(sapply(vars, function(x) length(unique(orig_boys[,x]
		))))+1

	out <- data.frame(matrix(NA, nrow = num_row, ncol = 14))
	colnames(out) <- c(rep("",2),"Males",rep("",5),"Females",rep("",5))
	out[1, ] <- c("Variable","Level",rep(c("Vic RR","LCI","UCI","Perp RR","CI","UCI"),
		2))

	length <- length(dimnames(stats)[[1]])
	out[2:num_row, 1] <- dimnames(stats)[[1]][2:length]
	out[2:num_row, 2] <- stats[, 1][2:length]

	#To determine row number for output
	level <- 2

	for (r in vars){
		length <- length(levels(droplevels(addNA(orig_boys[,r]))))
		for (l in 1:(length-1)){
			model <- paste0(outcome1," ~ addNA(",r,")")

			#Boys vic
			fit <- glm(as.formula(model), family=binomial(link = "logit"), 
				data = orig_boys)
			out[level+l, 3] <- round(exp(summary(fit)$coefficient[l+1,1]), 
				digits = digits)
			out[level+l, 4] <- round(exp(summary(fit)$coefficient[l+1,1]
				-1.96*summary(fit)$coefficient[l+1,2]), digits = digits)
			out[level+l, 5] <- round(exp(summary(fit)$coefficient[l+1,1]
				+1.96*summary(fit)$coefficient[l+1,2]), digits = digits)

			#Girls vic
			fit <- glm(as.formula(model), family=binomial(link = "logit"), 
				data = orig_girls)
			out[level+l, 9] <- round(exp(summary(fit)$coefficient[l+1,1]), 
				digits = digits)
			out[level+l, 10] <- round(exp(summary(fit)$coefficient[l+1,1]
				-1.96*summary(fit)$coefficient[l+1,2]), digits = digits)
			out[level+l, 11] <- round(exp(summary(fit)$coefficient[l+1,1]
				+1.96*summary(fit)$coefficient[l+1,2]), digits = digits)

			model <- paste0(outcome2," ~ addNA(",r,")")
			#Boys perp
			fit <- glm(as.formula(model), family=binomial(link = "logit"), 
				data = orig_boys)
			out[level+l, 6] <- round(exp(summary(fit)$coefficient[l+1,1]), 
				digits = digits)
			out[level+l, 7] <- round(exp(summary(fit)$coefficient[l+1,1]
				-1.96*summary(fit)$coefficient[l+1,2]), digits = digits)
			out[level+l, 8] <- round(exp(summary(fit)$coefficient[l+1,1]
				+1.96*summary(fit)$coefficient[l+1,2]), digits = digits)

			#Girls perp
			fit <- glm(as.formula(model), family=binomial(link = "logit"), 
				data = orig_girls)
			out[level+l, 12] <- round(exp(summary(fit)$coefficient[l+1,1]), 
				digits = digits)
			out[level+l, 13] <- round(exp(summary(fit)$coefficient[l+1,1]
				-1.96*summary(fit)$coefficient[l+1,2]), digits = digits)
			out[level+l, 14] <- round(exp(summary(fit)$coefficient[l+1,1]
				+1.96*summary(fit)$coefficient[l+1,2]), digits = digits)
			}
		out[(out[,1] == as.character(r) & out[,2] == levels(droplevels(addNA(
			orig_boys[,r])))[1]), 3] <- "ref"

		level <- level+length
		}

	out[is.na(out[,2])==TRUE,2] <- "Not reported"
	out[out[,2]=="0",2] <- "No"
	out[out[,2]=="1",2] <- "Yes"

	#Drop levels that have no rows
	out <- out[is.na(out[,3])==FALSE,]

	return(out)
	}