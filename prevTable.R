################################################################################
## Project: Risk factors for IPVA
## Script purpose: Function for creating tables of proportions using MI data, 
#split by sex
## Date: 6th June 2019
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

require(labelled)
require(dplyr)
require(mice)
require(magrittr)
require(tableone)

prevTable <- function(imp_boys.long, imp_girls.long, vars){

	#Rename variable indicating original dataset
	mi_boys <- imp_boys.long[imp_boys.long$.imp == 1,]
	
	imp_boys.long[,c("one")] <- rep(1, dim(imp_boys.long)[1])
	imp_girls.long[,c("one")] <- rep(1, dim(imp_girls.long)[1])

	stats <- print(
		           CreateCatTable(vars = vars, data = mi_boys, includeNA = TRUE),
		             showAllLevels = TRUE,
		             #quote = TRUE,
		             test = FALSE,
		             format = "f")

	num_row <- sum(sapply(vars, function(x) length(unique(mi_boys[,x]#,exclude=NULL
		))))+1

	out <- data.frame(matrix(NA, nrow = num_row, ncol = 14))
	colnames(out) <- c(rep("",2),"Males",rep("",5),"Females",rep("",5))
	out[1, ] <- c("Variable","Level",rep(c("V 0-17y","V 18-21y","V 0-21y","P 0-17y",
		"P 18-21y","P 0-21y"),2))

	length <- length(dimnames(stats)[[1]])
	out[2:num_row, 1] <- dimnames(stats)[[1]][2:length]
	out[2:num_row, 2] <- stats[, 1][2:length]

	level <- 1
	outcomes <- c("vic_017","vic_1821","vic_021","per_017","per_1821","per_021")
	for(r in vars){
		for(o in outcomes){
			imp <- aggregate(imp_boys.long[,o], by=list(
				imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,2]
			num <- aggregate(imp_boys.long[,o], by=list(
				imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,3]
			denom <- aggregate(imp_boys.long$one, by=list(
				imp_boys.long$.imp,imp_boys.long[,r]), FUN=sum)[,3]
			prop <- num/denom*100
			assign(paste0("mean_prop_boys_",o), value = aggregate(prop, by=list(imp), 
				FUN=mean))

			imp <- aggregate(imp_girls.long[,o], by=list(
				imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,2]
			num <- aggregate(imp_girls.long[,o], by=list(
				imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,3]
			denom <- aggregate(imp_girls.long$one, by=list(
				imp_girls.long$.imp,imp_girls.long[,r]), FUN=sum)[,3]
			prop <- num/denom*100
			assign(paste0("mean_prop_girls_",o), value = aggregate(prop, by=list(imp), 
				FUN=mean))
			}
		length <- length(levels(droplevels(addNA(mi_boys[,r]))))
		for (l in 1:length){
			out[level+l, 3] <- round(mean_prop_boys_vic_017[l,2], digits = 1)
			out[level+l, 4] <- round(mean_prop_boys_vic_1821[l,2], digits = 1)
			out[level+l, 5] <- round(mean_prop_boys_vic_021[l,2], digits = 1)
			
			out[level+l, 6] <- round(mean_prop_boys_per_017[l,2], digits = 1)
			out[level+l, 7] <- round(mean_prop_boys_per_1821[l,2], digits = 1)
			out[level+l, 8] <- round(mean_prop_boys_per_021[l,2], digits = 1)

			out[level+l, 9] <- round(mean_prop_girls_vic_017[l,2], digits = 1)
			out[level+l, 10] <- round(mean_prop_girls_vic_1821[l,2], digits = 1)
			out[level+l, 11] <- round(mean_prop_girls_vic_021[l,2], digits = 1)
			
			out[level+l, 12] <- round(mean_prop_girls_per_017[l,2], digits = 1)
			out[level+l, 13] <- round(mean_prop_girls_per_1821[l,2], digits = 1)
			out[level+l, 14] <- round(mean_prop_girls_per_021[l,2], digits = 1)
			}
		level <- level+length
		}

	out[is.na(out[,2])==TRUE,2] <- "Not reported"
	out[out[,2]=="0",2] <- "No"
	out[out[,2]=="1",2] <- "Yes"

	#Drop levels that have no rows
	out <- out[is.na(out[,3])==FALSE,]

	return(out)
	}