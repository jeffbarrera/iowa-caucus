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



# weightPollingBy538Rankings
# --------------------------
# Given a df of polling data and a df (or some other object) of 538's pollster rankings, duplicate polling observations to weight by 538's rankings.
# Return a df with weighted polling data




# predictFromPollTrends
# ---------------------
# Predict election day vote share by extrapolating from weighted polls regressed against time. 
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)



linear_extrapolate_candidate <- function(candidate, polling_df, results_df) {

	# find col index for that candidate's name
	cand_indx <- which(colnames(polling_df) == candidate)

	# check if there are any rows that aren't NA
	if(nrow(na.omit(polling_df[,cand_indx])) == 0) {
		print("All observations were NA")
		return(0)
	} else {

		# simple linear model
		lm_model <- lm(polling_df[,cand_indx] ~ days_to_caucus, data=polling_df)

		# plot data
		plot(x=polling_df$days_to_caucus,
			y=polling_df[,cand_indx],
			# ylim=c(0, results_df[results_df$Candidate==candidate, VOTE_SHARE_COL] + 10),
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

		# predict vote share on election day
		election_day_df <- data.frame(c(candidate),c("0"))
		colnames(election_day_df) <- c("Candidate", "days_to_caucus")
		election_day_df$days_to_caucus <- as.numeric(as.character(election_day_df$days_to_caucus))

		pred_vote_share <- predict(lm_model, newdata=election_day_df)
		
		return(pred_vote_share)
	}
}

###### try each candidate

# init df to store predictions
pred_votes <- data.frame(results2008$Candidate, c(NA))
colnames(pred_votes) <- c('candidate', 'vote_share')

for (cand in results2008$Candidate) {
	print(cand)
	pred_votes$vote_share[pred_votes$candidate == cand] <- linear_extrapolate_candidate(cand, polls2008, results2008)
}

pred_votes


which(colnames(polls2008) == "THOMPSON")


na.omit(polls2008[, 17]) ### FIGURE OUT HOW THIS WORKS

testModelRMSE <- function(pred_votes, results_df) {


}


linear_extrapolate_candidate("MCCAIN", polls2008, results2008)

results2008[results2008$Candidate == "MCCAIN", VOTE_SHARE_COL]


election_day_df <- data.frame(c("MCCAIN"),c("0"))
colnames(election_day_df) <- c("Candidate", "days_to_caucus")


class(election_day_df$days_to_caucus)

head(polls2008)


lm_model <- lm(polls2008$)

# predictFromLatestPolling
# ------------------------
# Take the average of the weighted most recent polls for a candidate -- maybe last week or few days (could try Monte Carlo to see what time interval works best).
# Return df/matrix of candidate names (rows) and predicted vote shares

# conbinePredictorsIntoModel
# --------------------------
# Using the outputs from predictFromPollTrends and predictFromLatestPolling outputs for both Iowa and national polling, build a model applying these predictors to 2008 and 2012 data. See which combinations work best, maybe with LOOCV? If have time, could try additional predictors -- contributions, prediction markets, etc.
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)

# applyModelTo2016
# ----------------
# Use the model/submodels (predictFromPollTrends, etc) to forecast vote shares for the 2016 candidates.
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)

# scaleModelPredictionsIntoVoteShares
# ------------------------------------
# Assuming the predictions from our model don't add up to 100, scale them proportionally: voteshare[candidate] / sum(voteshare) * 100