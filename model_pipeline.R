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
polls2008_national <- read.csv('pollster/data/cleaned_2008_national.csv', stringsAsFactors=FALSE)
polls2012_national <- read.csv('pollster/data/cleaned_2012_national.csv', stringsAsFactors=FALSE)
polls2016_national <- read.csv('pollster/data/cleaned_2016_national.csv', stringsAsFactors=FALSE)





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

polls2008_national <- cleanPolling(polls2008_national)
polls2012_national <- cleanPolling(polls2012_national)
polls2016_national <- cleanPolling(polls2016_national)

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














##############
# PREDICT POLL TRENDS
##############


# linearExtrapolateCandidate
# --------------------------
# Given a candidate, polling_df, and results_df, generate a simple linear model regressing vote shares over time
# return predicted vote share on election day

linearExtrapolateCandidate <- function(candidate, polling_df, results_df, plot) {

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

		if(plot == TRUE) {

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
		}

		# predict vote share on election day
		election_day_df <- data.frame(c(candidate),c("0"))
		colnames(election_day_df) <- c("candidate", "days_to_caucus")
		election_day_df$days_to_caucus <- as.numeric(as.character(election_day_df$days_to_caucus))

		pred_vote_share <- predict(lm_model, newdata=election_day_df)
		
		return(pred_vote_share)
	}
}



complexLinearExtrapolateCandidate <- function(candidate, polling_df, results_df, plot) {

	# find col index for that candidate's name
	cand_indx <- which(colnames(polling_df) == candidate)

	# check if there are any rows that aren't NA
	if (all(is.na(polling_df[,cand_indx]))) {
		print(candidate)
		print("All observations were NA")
		return(0)
	} else {

		# simple linear model
		lm_model <- lm(polling_df[,cand_indx] ~ days_to_caucus + I(days_to_caucus^2), data=polling_df)

		if(plot == TRUE) {

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
		}

		# predict vote share on election day
		election_day_df <- data.frame(c(candidate),c("0"))
		colnames(election_day_df) <- c("candidate", "days_to_caucus")
		election_day_df$days_to_caucus <- as.numeric(as.character(election_day_df$days_to_caucus))

		pred_vote_share <- predict(lm_model, newdata=election_day_df)
		
		return(pred_vote_share)
	}
}

linearExtrapolateCandidate('mccain', polls2008_iowa, results2008, TRUE)

complexLinearExtrapolateCandidate('mccain', polls2008_iowa, results2008, FALSE)


# predCandidates
# --------------
# Given a list of candidates, polling data, and a model, generate predictions for each candidate
# returns df with candidate names and predictions
predCandidates <- function(polling_df, results_df, model) {

	# init df to store predictions
	pred_votes <- data.frame(results_df$candidate, c(NA))
	colnames(pred_votes) <- c('candidate', 'vote_share')

	for (cand in results_df$candidate) {
		pred_votes$vote_share[pred_votes$candidate == cand] <- model(cand, polling_df, results_df, FALSE)
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








##############
# BUILD & TEST COMBINED MODEL OPTIONS
##############


# buildPredictionMatrix
# ---------------------
# Given polling and results dfs, construct a matrix of observations to use in regression models
buildPredictionMatrix <- function(state_polling_df, natl_polling_df, results_df) {

	# construct vector of predictor names
	pred_names <- c('lm_1_iowa', 'lm_2_iowa', 'lm_1_natl')

	# build matrix of predictors
	preds <- matrix(nrow=nrow(results_df), ncol=length(pred_names))
	rownames(preds) <- results_df$candidate

	# add polling model predictions
	preds[,1] <- predCandidates(state_polling_df, results_df, linearExtrapolateCandidate)$vote_share
	preds[,2] <- predCandidates(state_polling_df, results_df, complexLinearExtrapolateCandidate)$vote_share
	preds[,3] <- predCandidates(natl_polling_df, results_df, linearExtrapolateCandidate)$vote_share

	# add predictor names to matrix
	colnames(preds) <- pred_names

	return(preds)
}


# scaleToVoteShares
# ------------------------------------
# Assuming the predictions from our model don't add up to 100, scale them proportionally: voteshare[candidate] / sum(voteshare) * 100
scaleToVoteShares <- function(raw_preds) {
	scaled_preds <- (raw_preds / sum(raw_preds)) * 100
	return(scaled_preds)
}


# scaleSortResults
# ----------------
# Scale and sort predictions
scaleSortResults <- function(preds) {
	scaled_preds <- scaleToVoteShares(preds)
	sorted_preds <- scaled_preds[order(scaled_preds[,1], decreasing=TRUE),]
	df_preds <- data.frame(sorted_preds)
	return(df_preds)
}


# combinePredictorsIntoModel
# --------------------------
# Using the outputs from predictFromPollTrends and predictFromLatestPolling outputs for both Iowa and national polling, build a model applying these predictors to 2008 and 2012 data. See which combinations work best, maybe with LOOCV? If have time, could try additional predictors -- contributions, prediction markets, etc.
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)

combinePredictorsIntoModel <- function(polling_df, results_df, combined) {

	if (combined) {

		# create matrices to use in models
		preds_2008 <- buildPredictionMatrix(polls2008_iowa, polls2008_national, results2008)
		preds_2012 <- buildPredictionMatrix(polls2012_iowa, polls2012_national, results2012)

		# combine into overall matrix
		preds_train <- rbind(preds_2008, preds_2012)
		results_train <- rbind(results2008, results2012)

	} else {

		# create matrix to use in models
		preds_train <- buildPredictionMatrix(polling_df, results_df)
		results_train <- results_df
	}

	# build matrix to predict 2016
	preds_2016 <- buildPredictionMatrix(polls2016_iowa, candidates_2016)
	

	# run simple linear regression 
	# linear_model <- lm(results_df$percentage ~ ., data=as.data.frame(preds_train))
	# summary(linear_model)
	# lm_preds <- predict(linear_model, newdata=as.data.frame(preds_train))

	# # check RMSE
	# rmse(results_df$percentage, lm_preds)

	# # try predicting 2016 using linear model
	# lm_preds_2016 <- predict(linear_model, newdata=as.data.frame(preds_2016))




	# run LASSO on predictors
	lasso <- glmnet(x = preds_train, y = results_train$percentage)
	summary(lasso)

	# cross validate lasso
	lasso_cv<- cv.glmnet(x = preds_train, y = results_train$percentage)
	plot(lasso_cv)

	# TURN ^ INTO LOOCV

	# check RMSE
	lasso_train_preds <- predict(lasso, newx=preds_train, s = lasso_cv$lambda.min )
	print("RMSE:")
	print(rmse(results_train$percentage, lasso_train_preds))

	# predict 2016 using lasso
	lasso_preds <- predict(lasso, newx=preds_2016, s = lasso_cv$lambda.min )


	# scale and sort results
	df_preds_2016 <- scaleSortResults(lasso_preds)

	return(df_preds_2016)
}

combinePredictorsIntoModel(polls2008_iowa, results2008, FALSE)
combinePredictorsIntoModel(polls2012_iowa, results2012, TRUE)



predCandidates(polls2008_national, results2008, linearExtrapolateCandidate)$vote_share



















