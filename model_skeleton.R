##############
# IMPORT LIBRARIES
##############
library(glmnet)
library(Metrics)



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
results2012 <- read.csv('results_2012.csv', stringsAsFactors=FALSE)

# df of 2016 candidates to predict
candidates_2016 <- data.frame(c('trump', 'cruz', 'rubio','bush','carson','christie','paul','huckabee','kasich'))
colnames(candidates_2016) <- c('candidate')


# load polling datasets
polls2008_iowa <- read.csv('pollster/data/cleaned_2008_iowa.csv', stringsAsFactors=FALSE)
polls2012_iowa <- read.csv('pollster/data/cleaned_2012_iowa.csv', stringsAsFactors=FALSE)
polls2016_iowa <- read.csv('pollster/data/cleaned_2016_iowa.csv', stringsAsFactors=FALSE)




# clean polling data
cleanPolling <- function(df) {
	# replace '-' with NA
	df[df == '-'] <- NA

	# convert polling cols from char to numeric
	df[FIRST_POLLING_COL:ncol(df)] <- sapply(df[FIRST_POLLING_COL:ncol(df)], as.numeric)

	# convert pollster cols to factor
	df$POLLSTER <- as.factor(df$POLLSTER)

	# convert all colnames to lower case
	colnames(df) <- tolower(colnames(df))

	# subset to polling conducted before the caucus
	df <- df[df$days_to_caucus > 0,]

	return(df)
}

polls2008_iowa <- cleanPolling(polls2008_iowa)
polls2012_iowa <- cleanPolling(polls2012_iowa)
polls2016_iowa <- cleanPolling(polls2016_iowa)

cleanResults <- function(df) {

	# convert all colnames to lower case
	colnames(df) <- tolower(colnames(df))

	# convert candidate names to lower case
	df$candidate <- tolower(df$candidate)

	return(df)
}

results2008 <- cleanResults(results2008)
results2012 <- cleanResults(results2012)



# weightPollingBy538Rankings
# --------------------------
# Given a df of polling data and a df (or some other object) of 538's pollster rankings, duplicate polling observations to weight by 538's rankings.
# Return a df with weighted polling data










# predictFromPollTrends
# ---------------------
# Predict election day vote share by extrapolating from weighted polls regressed against time. 
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)



##############
# PREDICT POLL TRENDS
##############


# linearExtrapolateCandidate
# --------------------------
# Given a candidate, polling_df, and results_df, generate a simple linear model regressing vote shares over time
# return predicted vote share on election day


## add plot boolean var

linearExtrapolateCandidate <- function(candidate, polling_df, results_df) {

	# find col index for that candidate's name
	cand_indx <- which(colnames(polling_df) == candidate)

	######## consider killing early models

	# check if there are any rows that aren't NA
	if(all(is.na(polling_df[,cand_indx]))) {
		print(candidate)
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
		abline(h=results_df[results_df$candidate==candidate, VOTE_SHARE_COL],
			lwd=2)

		# predict vote share on election day
		election_day_df <- data.frame(c(candidate),c("0"))
		colnames(election_day_df) <- c("candidate", "days_to_caucus")
		election_day_df$days_to_caucus <- as.numeric(as.character(election_day_df$days_to_caucus))

		pred_vote_share <- predict(lm_model, newdata=election_day_df)
		
		return(pred_vote_share)
	}
}



complexLinearExtrapolateCandidate <- function(candidate, polling_df, results_df) {

	# find col index for that candidate's name
	cand_indx <- which(colnames(polling_df) == candidate)

	# check if there are any rows that aren't NA
	if(all(is.na(polling_df[,cand_indx]))) {
		print(candidate)
		print("All observations were NA")
		return(0)
	} else {

		# simple linear model
		lm_model <- lm(polling_df[,cand_indx] ~ days_to_caucus + I(days_to_caucus^2), data=polling_df)

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
		abline(h=results_df[results_df$candidate==candidate, VOTE_SHARE_COL],
			lwd=2)

		# predict vote share on election day
		election_day_df <- data.frame(c(candidate),c("0"))
		colnames(election_day_df) <- c("candidate", "days_to_caucus")
		election_day_df$days_to_caucus <- as.numeric(as.character(election_day_df$days_to_caucus))

		pred_vote_share <- predict(lm_model, newdata=election_day_df)
		
		return(pred_vote_share)
	}
}

linearExtrapolateCandidate('mccain', polls2008_iowa, results2008)

complexLinearExtrapolateCandidate('mccain', polls2008_iowa, results2008)


# predCandidates
# --------------
# Given a list of candidates, polling data, and a model, generate predictions for each candidate
# returns df with candidate names and predictions
predCandidates <- function(polling_df, results_df, model) {

	# init df to store predictions
	pred_votes <- data.frame(results_df$candidate, c(NA))
	colnames(pred_votes) <- c('candidate', 'vote_share')

	for (cand in results_df$candidate) {
		pred_votes$vote_share[pred_votes$candidate == cand] <- model(cand, polling_df, results_df)
	}
	return(pred_votes)
}


# calcRMSE
# --------
# Given a df of prediction and a df with actual results, compute the rmse for the predictions
calcRMSE <- function(pred_votes, results_df) {

	# order both dfs by candidate name, just to be safe
	pred_votes <- pred_votes[order(pred_votes$candidate),]
	results_df <- results_df[order(results_df$candidate),]

	mse <- sum((results_df$percentage - pred_votes$vote_share)^2) / nrow(pred_votes)
	rmse <- sqrt(mse)
	return(rmse)
}


# testPollTrendsModel
# -------------------
# Given polling and results for an election, see how well a specified model extrapolates from polling data
testPollTrendsModel <- function(polling_df, results_df, model) {

	pred_votes <- predCandidates(polling_df, results_df, model)
	rmse <- calcRMSE(pred_votes, results_df)
	return(rmse)
}


testPollTrendsModel(polls2008_iowa, results2008, linearExtrapolateCandidate)
testPollTrendsModel(polls2012_iowa, results2012, linearExtrapolateCandidate)





# predictFromLatestPolling
# ------------------------
# Take the average of the weighted most recent polls for a candidate -- maybe last week or few days (could try Monte Carlo to see what time interval works best).
# Return df/matrix of candidate names (rows) and predicted vote shares




##############
# BUILD & TEST COMBINED MODEL OPTIONS
##############


# scaleToVoteShares
# ------------------------------------
# Assuming the predictions from our model don't add up to 100, scale them proportionally: voteshare[candidate] / sum(voteshare) * 100
scaleToVoteShares <- function(raw_preds) {
	scaled_preds <- (raw_preds / sum(raw_preds)) * 100
	return(scaled_preds)
}


# combinePredictorsIntoModel
# --------------------------
# Using the outputs from predictFromPollTrends and predictFromLatestPolling outputs for both Iowa and national polling, build a model applying these predictors to 2008 and 2012 data. See which combinations work best, maybe with LOOCV? If have time, could try additional predictors -- contributions, prediction markets, etc.
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)

combinePredictorsIntoModel <- function(polling_df, results_df) {

	# build matrix of predictors
	preds_train <- matrix(nrow=nrow(results_df), ncol=2)
	rownames(preds_train) <- results_df$candidate

	# add polling model predictions
	preds_train[,1] <- predCandidates(polling_df, results_df, linearExtrapolateCandidate)$vote_share
	preds_train[,2] <- predCandidates(polling_df, results_df, complexLinearExtrapolateCandidate)$vote_share

	# add predictor names to matrix
	colnames(preds_train) <- c('basic_linear', 'complex_linear')


	# build matrix of 2016 predictors
	preds_2016 <- matrix(nrow=nrow(candidates_2016), ncol=2)
	rownames(preds_2016) <- candidates_2016$candidate

	# add polling model predictions
	preds_2016[,1] <- predCandidates(polls2016_iowa, candidates_2016, linearExtrapolateCandidate)$vote_share
	preds_2016[,2] <- predCandidates(polls2016_iowa, candidates_2016, complexLinearExtrapolateCandidate)$vote_share

	# add predictor names to matrix
	colnames(preds_2016) <- c('basic_linear', 'complex_linear')


	

	# run simple linear regression 
	linear_model <- lm(results_df$percentage ~ ., data=as.data.frame(preds_train))
	summary(linear_model)
	lm_preds <- predict(linear_model, newdata=as.data.frame(preds_train))

	# check RMSE
	rmse(results_df$percentage, lm_preds)

	# try predicting 2016 using linear model
	lm_preds_2016 <- predict(linear_model, newdata=as.data.frame(preds_2016))




	# run LASSO on predictors
	lasso <- glmnet(x = preds_train, y = results_df$percentage)
	summary(lasso)

	# cross validate lasso
	lasso_cv<- cv.glmnet(x = preds_train, y = results_df$percentage)
	plot(lasso_cv)

	# predict 2016 using lasso
	lasso_preds <- predict(lasso, newx=preds_2016, s = lasso_cv$lambda.min )

	# check RMSE
	rmse(results_df$percentage, lasso_preds)

	return(lasso_preds)
}


preds_using2008 <- combinePredictorsIntoModel(polls2008_iowa, results2008)
preds_using2012 <- combinePredictorsIntoModel(polls2012_iowa, results2012) # bugging

preds_using2008

scaleToVoteShares(preds_using2008)

# applyModelTo2016
# ----------------
# Use the model/submodels (predictFromPollTrends, etc) to forecast vote shares for the 2016 candidates.
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)





















