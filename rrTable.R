################################################################################
## Project: Risk factors for IPVA
## Script purpose: Function for creating tables of RRs using MI data, split by sex
## Date: 06.11.19
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

require(labelled)
require(dplyr)
require(mice)
require(magrittr)
require(tableone)

rrTable <- function(imp_boys, imp_boys.long, imp_girls, imp_girls.long, outcome1, outcome2, vars, digits){

	## ---- Rename variable indicating original dataset ----------------------------
	mi_boys <- imp_boys.long[imp_boys.long$.imp == 1,]
	mi_girls <- imp_girls.long[imp_girls.long$.imp == 1,]

	#To help with calculating prevalence later
	imp_boys.long[,c("one")] <- rep(1, dim(imp_boys.long)[1])
	imp_girls.long[,c("one")] <- rep(1, dim(imp_girls.long)[1])

	# imp_boys <- as.mids(imp_boys.long)
	# imp_girls <- as.mids(imp_girls.long)

	stats <- print(
		           CreateCatTable(vars = vars, data = mi_boys, 
		           	 includeNA = TRUE),
		             showAllLevels = TRUE,
		             #quote = TRUE,
		             test = FALSE,
		             format = "f")

	# Number of variables in rfs that have an 'NA' value (because no imputation)
	num_row <- sum(sapply(vars, function(x) length(unique(addNA(mi_boys[,x]#,exclude=NULL
		)))))+1

	out <- data.frame(matrix(NA, nrow = num_row, ncol = 14))
	colnames(out) <- c(rep("",2),"Males",rep("",5),"Females",rep("",5))
	out[1, ] <- c("Variable","Level",rep(c("Vic RR","LCI","UCI","Perp RR","CI","UCI"),2))

	# Don't want to bother with 'n' row
	length <- length(dimnames(stats)[[1]])
	out[2:num_row, 1] <- dimnames(stats)[[1]][2:length]
	out[2:num_row, 2] <- stats[, 1][2:length]

	#To determine row number for output
	level <- 2

	#If we just used imp_boys$data, this would be equivalent to CCA
	for (r in vars){
		length <- length(levels(droplevels(addNA(mi_boys[,r]))))
		baseline <- min(levels(as.factor(mi_boys[,r])))
		for (l in 1:(length-1)){
			model <- paste0(outcome1," ~ addNA(",r,")")

			#Boys vic
			fit <- with(data = imp_boys, exp = glm(as.formula(model), family=binomial(link = "logit")))
			out[level+l, 3] <- exp(summary(pool(fit))$estimate[1+l])
			out[level+l, 4] <- exp(summary(pool(fit))$estimate[1+l]-1.96*summary(pool(fit))$std.error[1+l])
			out[level+l, 5] <- exp(summary(pool(fit))$estimate[1+l]+1.96*summary(pool(fit))$std.error[1+l])

			#06.01.20 - want to use baseline prevalence to approximate RR
			#as per Zhang et al: https://jamanetwork.com/journals/jama/fullarticle/188182
			group <- aggregate(imp_boys.long[,outcome1], by=list(imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,2]
			num <- aggregate(imp_boys.long[,outcome1], by=list(imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,3]
			denom <- aggregate(imp_boys.long$one, by=list(imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,3]
			prop <- num/denom
			assign(paste0("mean_prop_boys_",outcome1), value = aggregate(prop, by=list(group), FUN=mean))
			p0 <- get(paste0("mean_prop_boys_",outcome1))
			p0 <- p0[p0[,1]==baseline,2]
			out[level+l, 3] <- round(as.numeric(out[level+l, 3])/(1-p0+(p0*as.numeric(out[level+l, 3]))), digits=digits)
			out[level+l, 4] <- round(as.numeric(out[level+l, 4])/(1-p0+(p0*as.numeric(out[level+l, 4]))), digits=digits)
			out[level+l, 5] <- round(as.numeric(out[level+l, 5])/(1-p0+(p0*as.numeric(out[level+l, 5]))), digits=digits)

			#Girls vic
			fit <- with(data = imp_girls, exp = glm(as.formula(model), family=binomial(link = "logit")))
			out[level+l, 9] <- exp(summary(pool(fit))$estimate[1+l])
			out[level+l, 10] <- exp(summary(pool(fit))$estimate[1+l]-1.96*summary(pool(fit))$std.error[1+l])
			out[level+l, 11] <- exp(summary(pool(fit))$estimate[1+l]+1.96*summary(pool(fit))$std.error[1+l])
			group <- aggregate(imp_girls.long[,outcome1], by=list(imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,2]
			num <- aggregate(imp_girls.long[,outcome1], by=list(imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,3]
			denom <- aggregate(imp_girls.long$one, by=list(imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,3]
			prop <- num/denom*100
			assign(paste0("mean_prop_girls_",outcome1), value = aggregate(prop, by=list(group), FUN=mean))
			p0 <- get(paste0("mean_prop_girls_",outcome1))
			p0 <- p0[p0[,1]==baseline,2]/100
			out[level+l, 9] <- round(as.numeric(out[level+l, 9])/(1-p0+(p0*as.numeric(out[level+l, 9]))), digits=digits)
			out[level+l, 10] <- round(as.numeric(out[level+l, 10])/(1-p0+(p0*as.numeric(out[level+l, 10]))), digits=digits)
			out[level+l, 11] <- round(as.numeric(out[level+l, 11])/(1-p0+(p0*as.numeric(out[level+l, 11]))), digits=digits)

			model <- paste0(outcome2," ~ addNA(",r,")")
			#Boys perp
			fit <- with(data = imp_boys, exp = glm(as.formula(model), family=binomial(link = "logit")))
			out[level+l, 6] <- exp(summary(pool(fit))$estimate[1+l])
			out[level+l, 7] <- exp(summary(pool(fit))$estimate[1+l]-1.96*summary(pool(fit))$std.error[1+l])
			out[level+l, 8] <- exp(summary(pool(fit))$estimate[1+l]+1.96*summary(pool(fit))$std.error[1+l])
			group <- aggregate(imp_boys.long[,outcome2], by=list(imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,2]
			num <- aggregate(imp_boys.long[,outcome2], by=list(imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,3]
			denom <- aggregate(imp_boys.long$one, by=list(imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,3]
			prop <- num/denom*100
			assign(paste0("mean_prop_boys_",outcome2), value = aggregate(prop, by=list(group), FUN=mean))
			p0 <- get(paste0("mean_prop_boys_",outcome2))
			p0 <- p0[p0[,1]==baseline,2]/100
			out[level+l, 6] <- round(as.numeric(out[level+l, 6])/(1-p0+(p0*as.numeric(out[level+l, 6]))), digits=digits)
			out[level+l, 7] <- round(as.numeric(out[level+l, 7])/(1-p0+(p0*as.numeric(out[level+l, 7]))), digits=digits)
			out[level+l, 8] <- round(as.numeric(out[level+l, 8])/(1-p0+(p0*as.numeric(out[level+l, 8]))), digits=digits)

			#Girls perp
			fit <- with(data = imp_girls, exp = glm(as.formula(model), family=binomial(link = "logit")))
			out[level+l, 12] <- exp(summary(pool(fit))$estimate[1+l])
			out[level+l, 13] <- exp(summary(pool(fit))$estimate[1+l]-1.96*summary(pool(fit))$std.error[1+l])
			out[level+l, 14] <- exp(summary(pool(fit))$estimate[1+l]+1.96*summary(pool(fit))$std.error[1+l])
				group <- aggregate(imp_girls.long[,outcome2], by=list(imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,2]
			num <- aggregate(imp_girls.long[,outcome2], by=list(imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,3]
			denom <- aggregate(imp_girls.long$one, by=list(imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,3]
			prop <- num/denom*100
			assign(paste0("mean_prop_girls_",outcome2), value = aggregate(prop, by=list(group), FUN=mean))
			p0 <- get(paste0("mean_prop_girls_",outcome2))
			p0 <- p0[p0[,1]==baseline,2]/100
			out[level+l, 12] <- round(as.numeric(out[level+l, 12])/(1-p0+(p0*as.numeric(out[level+l, 12]))), digits=digits)
			out[level+l, 13] <- round(as.numeric(out[level+l, 13])/(1-p0+(p0*as.numeric(out[level+l, 13]))), digits=digits)
			out[level+l, 14] <- round(as.numeric(out[level+l, 14])/(1-p0+(p0*as.numeric(out[level+l, 14]))), digits=digits)
			}
		out[(out[,1] == as.character(r) & out[,2] == levels(droplevels(addNA(imp_boys$data[,r])))[1]), 3] <- "ref"

		level <- level+length
		}

	out[is.na(out[,2])==TRUE,2] <- "Not reported"
	out[out[,2]=="0",2] <- "No"
	out[out[,2]=="1",2] <- "Yes"

	#Drop those levels that have no rows
	out <- out[is.na(out[,3])==FALSE,]

	return(out)
	}