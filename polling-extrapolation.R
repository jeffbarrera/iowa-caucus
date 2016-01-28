##############
# CONSTANTS
##############
FIRST_POLLING_COL <- 12
VOTE_SHARE_COL <- 3


# Jeff's working directory -- comment out and override
setwd("~//Documents/School/Stanford/Classes/Poli Sci 355B/Challenge 1/iowa-caucus")


##############
# LOAD AND CONVERT DATASETS
##############


# load results datasets
results2008 <- read.csv('results_2008.csv', stringsAsFactors=FALSE)

# load polling datasets
polls2008 <- read.csv('pollster/data/cleaned_2008_iowa.csv', stringsAsFactors=FALSE)




# clean polling data
cleanData <- function(df) {
	# replace '-' with NA
	df[df == '-'] <- NA

	# convert polling cols from char to numeric
	df[FIRST_POLLING_COL:ncol(df)] <- sapply(df[FIRST_POLLING_COL:ncol(df)], as.numeric)

	# convert pollster cols to factor
	df$POLLSTER <- as.factor(df$POLLSTER)

	# subset to polling conducted before the caucus
	df <- df[df$days_to_caucus > 0,]

	return(df)
}

polls2008 <- cleanData(polls2008)

head(polls2008)




##############
# play with data
##############

linear_extrapolate_candidate <- function(candidate, polling_df, results_df) {

	# find col index for that candidate's name
	cand_indx <- which(colnames(polling_df) == candidate)

	# simple linear model
	lm_model <- lm(polling_df[,cand_indx] ~ days_to_caucus, data=polling_df)

	# plot data
	plot(x=polling_df$days_to_caucus,
		y=polling_df[,cand_indx],
		ylim=c(0, results_df[results_df$Candidate==candidate, VOTE_SHARE_COL] + 10),
		xlim=c(max(na.omit(polling_df$days_to_caucus)), min(na.omit(polling_df$days_to_caucus))),
		xlab="Days to Caucus",
		ylab="Vote Share",
		main=candidate)

	# add regression line
	abline(lm_model,
		col="red",
		lwd="2")

	# add actual results line
	abline(h=results_df[results_df$Candidate==candidate, VOTE_SHARE_COL],
		lwd=2)
}

# try each candidate
for (candidate in results2008$Candidate) {
	linear_extrapolate_candidate(candidate, polls2008, results2008)
}



linear_extrapolate_candidate("ROMNEY", polls2008, results2008)
linear_extrapolate_candidate("HUCKABEE", polls2008, results2008)

linear_extrapolate_candidate("MCCAIN", polls2008, results2008)



which(colnames(polls2008) == "HUCKABEE")








