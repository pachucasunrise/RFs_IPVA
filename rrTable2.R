################################################################################
## Project: Risk factors for IPVA
## Script purpose: Function for creating tables of RRs using complete case data, 
## split by sex
## Date: 6th June 2019
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

require(labelled)
require(dplyr)
require(mice)
require(magrittr)
require(tableone)

rrTable2 <- function(data_boys,data_girls,exposure,outcomes,digits){
	
	for (o in outcomes){
		data_boys[,o] <- factor(data_boys[,o], levels = c(0,1))
		data_girls[,o] <- factor(data_girls[,o], levels = c(0,1))
		}

	stats <- print(
		           CreateCatTable(vars = outcomes, data = data_boys, 
		           	 includeNA = TRUE),
		             showAllLevels = TRUE,
		             #quote = TRUE,
		             test = FALSE,
		             format = "f")

	# Number of variables in rfs that have an 'NA' value
	num_row <- sum(sapply(outcomes, function(x) length(levels(addNA(data_boys[,x]
				)))))+1

	out <- data.frame(matrix(NA, nrow = num_row, ncol = 8))
	colnames(out) <- c(rep("",2),"Males",rep("",2),"Females",rep("",2))
	out[1, ] <- c("Outcome","Level",rep(c("RR","LCI","UCI"),2))

	length <- length(dimnames(stats)[[1]])
	out[2:num_row, 1] <- dimnames(stats)[[1]][2:length]
	out[2:num_row, 2] <- stats[, 1][2:length]

	#To determine row number for output
	level <- 2

	#Make sure taking highest level of factor, vs. level 1
	max <- length(levels(data_boys[,exposure]))

	for (o in outcomes){
		length <- length(levels(droplevels(addNA(data_boys[,o]))))
		for (l in 1:(length-1)){
			model <- paste0(o," ~ addNA(",exposure,")")

			#Boys vic
			fit <- glm(as.formula(model), family=binomial(link = "logit"), 
				data = data_boys)
			out[level+l, 3] <- round(exp(summary(fit)$coefficient[max,1]), 
				digits = digits)
			out[level+l, 4] <- round(exp(summary(fit)$coefficient[max,1]
				-1.96*summary(fit)$coefficient[max,2]), digits = digits)
			out[level+l, 5] <- round(exp(summary(fit)$coefficient[max,1]
				+1.96*summary(fit)$coefficient[max,2]), digits = digits)

			#Girls vic
			fit <- glm(as.formula(model), family=binomial(link = "logit"), 
				data = data_girls)
			out[level+l, 6] <- round(exp(summary(fit)$coefficient[max,1]), 
				digits = digits)
			out[level+l, 7] <- round(exp(summary(fit)$coefficient[max,1]
				-1.96*summary(fit)$coefficient[max,2]), digits = digits)
			out[level+l, 8] <- round(exp(summary(fit)$coefficient[max,1]
				+1.96*summary(fit)$coefficient[max,2]), digits = digits)
			}

		level <- level+length
		}
	out[,1] <- ifelse(as.character(out[,1])!="", out[,1], NA)
	out[,1] <- na.locf(out[,1])	

	out <- out[(out[,2]=="1" | out[,2]=="Level"),]
	#Drop levels that have no rows
	out <- out[is.na(out[,3])==FALSE,]
	#Drop levels column
	out <- out[,-2]
	colnames(out) <- c("","Males",rep("",2),"Females",rep("",2))

	return(out)
	}