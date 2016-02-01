##############
# IMPORT LIBRARIES
##############
library(glmnet)
library(Metrics)
library(lpridge)



##############
# CONSTANTS
##############
FIRST_POLLING_COL <- 12
VOTE_SHARE_COL <- 3
EP_BW <- 13 # found by testing MSE at different bandwidths in 2008 & 2012
EP_TRENDLINE <- 14 # found by testing MSE at bandwidth 13 and different trendlines in 2008 & 2012

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


# clean results data
cleanResults <- function(df) {

	# convert all colnames to lower case
	colnames(df) <- tolower(colnames(df))

	# convert candidate names to lower case
	df$candidate <- tolower(df$candidate)

	return(df)
}

results2008 <- cleanResults(results2008)
results2012 <- cleanResults(results2012)



# loadMergeEpanechnikov
# ---------------------
# read, clean, and concat Epanechnikov predictions.
# return combined ep preds.
loadMergeEpanechnikov <- function() {

	# load CSVs
	ep_2008_2012_iowa <- read.csv("ep_interp_log_pred.csv", stringsAsFactors=FALSE)
	ep_2008_natl <- read.csv("2008_predictions_national.csv", stringsAsFactors=FALSE)
	ep_2012_natl <- read.csv("2012_predictions_national.csv", stringsAsFactors=FALSE)
	ep_2016_iowa <- read.csv("2016_predictions.csv", stringsAsFactors=FALSE)
	ep_2016_natl <- read.csv("2016_predictions_national.csv", stringsAsFactors=FALSE)

	# put all into a list
	ep_dfs <- list(ep_2008_2012_iowa, ep_2016_iowa, ep_2008_natl, ep_2012_natl, ep_2016_natl)

	# strip out unneeded columns to allow table merging
	ep_dfs_sub <- lapply(ep_dfs, subset, select=c(window_type, bw_value, year, scope, trendline_width, candidate, estimate))

	# merge all tables
	ep_preds <- do.call(rbind, ep_dfs_sub)

	# clean up candidate column
	ep_preds$candidate <- tolower(ep_preds$candidate)

	return(ep_preds)
}

ep_preds <- loadMergeEpanechnikov()




##############
# PREDICT POLL TRENDS
##############

#### SIMPLE AVERAGES

# averageLatestPollingCandidate
# --------------------------
# Given a candidate, polling_df, results_df, and time period, calculate a simple average of the vote shares
averageLatestPollingCandidate <- function(candidate, polling_df, results_df, time_period, plot) {

	# hack == return zero if bachmann
	if(candidate == "bachmann") {
		return(0)
	} else {

		# subset polling_df to the specificed candidate and time period
		polls <- subset(polling_df, days_to_caucus < time_period & days_to_caucus > 1, select=candidate)
		poll_mean <- mean(polls[,1])
		poll_sd <- sd(polls[,1])

		return(poll_mean)
	}
}

averageLatestPollingCandidate("mccain", polls2008_national, results2008, 20)

#### LINEAR MODELS

# linearExtrapolateCandidate
# --------------------------
# Given a candidate, polling_df, and results_df, generate a simple linear model regressing vote shares over time
# return predicted vote share on election day

linearExtrapolateCandidate <- function(candidate, polling_df, results_df, time_period, plot) {

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

linearExtrapolateCandidate("mccain", polls2008_iowa, results2008, 0, TRUE)



complexLinearExtrapolateCandidate <- function(candidate, polling_df, results_df, time_period, plot) {

	# find col index for that candidate's name
	cand_indx <- which(colnames(polling_df) == candidate)

	# check if there are any rows that aren't NA
	if (all(is.na(polling_df[,cand_indx]))) {
		print(candidate)
		print("All observations were NA")
		return(0)
	} else {

		# simple linear model
		lm_model <- lm(polling_df[,cand_indx] ~ days_to_caucus + I(days_to_caucus^2) + I(days_to_caucus^3), data=polling_df)

		if(plot == TRUE) {

			# print model
			# print(summary(lm_model))

			# plot data
			plot(x=polling_df$days_to_caucus,
				y=polling_df[,cand_indx],
				# ylim=c(0, results_df[results_df$Candidate==candidate, VOTE_SHARE_COL] + 10),
				xlim=c(max(na.omit(polling_df$days_to_caucus)), min(na.omit(polling_df$days_to_caucus))),
				xlab="Days to Caucus",
				ylab="Vote Share",
				main=candidate)

			# add regression line
			lines(x=polling_df$days_to_caucus, 
				  y=predict(lm_model), type='l', col="red", lwd=2)


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


#### EPANECHNIKOV

# chooseEpanechnikovBandwidthTrendline <- function(eplog_mse) {
# 	lowest_mse_indx <- which(eplog_mse$mse == min(eplog_mse$mse))
# 	lowest_mse_bw <- eplog_mse$bw_value[lowest_mse_indx]
# 	return(lowest_mse_indx)
# }

epanechnikovExtrapolateCandidate <- function(candidate_name, year_scope, results_df, time_period, plot) {

	# extract estimated vote share
	pred_vote_share <- subset(ep_preds, year==year_scope[1] & scope==year_scope[2] & bw_value==EP_BW & trendline_width==EP_TRENDLINE & candidate==candidate_name, select=estimate)

	return(pred_vote_share[1,1])
}

# epanechnikovExtrapolateCandidate("mccain", c("2008", "national"), results2008, FALSE)


##### OTHER Approaches



plotExtrapolateCandidate <- function(candidate, polling_df, results_df, time_period, plot) {

	# find col index for that candidate's name
	cand_indx <- which(colnames(polling_df) == candidate)

	# check if there are any rows that aren't NA
	if (all(is.na(polling_df[,cand_indx]))) {
		print(candidate)
		print("All observations were NA")
		return(0)
	} else {

		if(plot == TRUE) {

			# print model
			# print(summary(lm_model))

			# plot data
			plot(x=polling_df$days_to_caucus,
				y=polling_df[,cand_indx],
				col="grey",
				# ylim=c(0, results_df[results_df$Candidate==candidate, VOTE_SHARE_COL] + 10),
				xlim=c(max(na.omit(polling_df$days_to_caucus)), min(na.omit(polling_df$days_to_caucus))),
				xlab="Days to Caucus",
				ylab="Vote Share",
				main=toupper(candidate))

			# simple linear model
			lm_model <- lm(polling_df[,cand_indx] ~ days_to_caucus, data=polling_df)

			# add simple regression line
			abline(lm_model,
				col="red",
				lwd="2")

			# add complex regression line
			lm_model_complex <- lm(polling_df[,cand_indx] ~ days_to_caucus + I(days_to_caucus^2) + I(days_to_caucus^3), data=polling_df)
			
			# lines(x=polling_df$days_to_caucus, 
				  # y=predict(lm_model_complex), type='l', col="orange", lwd=2)


			# add lowess regression line
			cand_lowess = lowess(polling_df$days_to_caucus, polling_df[,cand_indx], f=.2)
			lines(cand_lowess, 
				col="blue",
				lwd="2")

			# add gaussian line
			gaussian_smooth = ksmooth(polling_df$days_to_caucus, polling_df[,cand_indx], "normal", bandwidth = 4)
			lines(gaussian_smooth,  col="orange", lwd=2)

			# add ep line
			cand_epan <- lpepa(polling_df$days_to_caucus, polling_df[,cand_indx], bandw=13, order=2, n.out=1000)
			lines(cand_epan$x.out, cand_epan$est,  col="purple")


			# add actual results line
			abline(h=results_df[results_df$candidate==candidate, VOTE_SHARE_COL],
				lwd=2,
				col="green")
		}
	}
}

plotExtrapolateCandidate("romney", polls2008_iowa, results2008, 0, TRUE)






# predCandidates
# --------------
# Given a list of candidates, polling data, and a model, generate predictions for each candidate
# returns df with candidate names and predictions
predCandidates <- function(polling_df, results_df, model, time_period) {

	# init df to store predictions
	pred_votes <- data.frame(results_df$candidate, c(NA))
	colnames(pred_votes) <- c('candidate', 'vote_share')

	for (cand in results_df$candidate) {

			pred_votes$vote_share[pred_votes$candidate == cand] <- model(cand, polling_df, results_df, time_period, FALSE)
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

# calcMSE
# --------
# Given a df of prediction and a df with actual results, compute the rmse for the predictions
calcMSE <- function(pred_votes, results_df) {

	# order both dfs by candidate name, just to be safe
	pred_votes <- pred_votes[order(pred_votes$candidate),]
	results_df <- results_df[order(results_df$candidate),]

	mse <- sum((results_df$percentage - pred_votes$vote_share)^2) / nrow(pred_votes)
	return(mse)
}



# testPollTrendsModel
# -------------------
# Given polling and results for an election, see how well a specified model extrapolates from polling data
testPollTrendsModel <- function(polling_df, results_df, model, time_period) {

	pred_votes <- predCandidates(polling_df, results_df, model, time_period)
	rmse <- calcMSE(pred_votes, results_df)
	return(rmse)
}

mse_simpleLinear <- (testPollTrendsModel(polls2008_iowa, results2008, linearExtrapolateCandidate, 0) + testPollTrendsModel(polls2012_iowa, results2012, linearExtrapolateCandidate, 0)) / 2

mse_simpleLinear

mse_complexLinear <- (testPollTrendsModel(polls2008_iowa, results2008, complexLinearExtrapolateCandidate, 0) + testPollTrendsModel(polls2012_iowa, results2012, complexLinearExtrapolateCandidate, 0)) / 2


## see what time period produces the lowest MSE for simple polling averages
chooseAverageInterval <- function(polling_df, results_df) {
	avg_days_test <- vector(length=21)

	for (i in 1:28) {
		avg_days_test[i] <- testPollTrendsModel(polling_df, results_df, averageLatestPollingCandidate, i)
	}

	return(
		which(avg_days_test == min(avg_days_test, na.rm =TRUE))
	)
}

AVG_INTERVAL <- chooseAverageInterval(polls2008_iowa, results2008) # 4 in 2008, 3 in 2012, go with 4

##############
# BUILD & TEST COMBINED MODEL OPTIONS
##############

predCandidates(polls2008_national, results2008, averageLatestPollingCandidate, AVG_INTERVAL)

# buildPredictionMatrix
# ---------------------
# Given polling and results dfs, construct a matrix of observations to use in regression models
buildPredictionMatrix <- function(state_polling_df, natl_polling_df, year, scope, results_df) {

	# construct vector of predictor names
	pred_names <- c('lm_1_iowa', 'lm_2_iowa', 'lm_1_natl', 'ep_iowa', 'ep_natl', 'avg_iowa')

	# build matrix of predictors
	preds <- matrix(nrow=nrow(results_df), ncol=length(pred_names))
	rownames(preds) <- results_df$candidate

	# add polling model predictions
	preds[,1] <- predCandidates(state_polling_df, results_df, linearExtrapolateCandidate, AVG_INTERVAL)$vote_share
	preds[,2] <- predCandidates(state_polling_df, results_df, complexLinearExtrapolateCandidate, AVG_INTERVAL)$vote_share
	# preds[,3] <- predCandidates(natl_polling_df, results_df, linearExtrapolateCandidate, AVG_INTERVAL)$vote_share
	preds[,3] <- 0
	preds[,4] <- predCandidates(c(year, scope), results_df, epanechnikovExtrapolateCandidate, AVG_INTERVAL)$vote_share
	preds[,5] <- predCandidates(c(year, "national"), results_df, epanechnikovExtrapolateCandidate, AVG_INTERVAL)$vote_share
	# preds[,6] <- predCandidates(state_polling_df, results_df, averageLatestPollingCandidate, AVG_INTERVAL)$vote_share
	preds[,6] <- 0



	# add predictor names to matrix
	colnames(preds) <- pred_names

	# replace any NAs with zeros
	preds[is.na(preds)] <- 0

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

combinePredictorsIntoModel <- function(state_polling_df, natl_polling_df, results_df, combined) {

	########### BUILD MATRICES ###########

	# extract year and state from state_polling_df name
	polling_df_str <- deparse(substitute(state_polling_df))
	# strip out "polls"
	year_scope_str <- sub("polls", "", polling_df_str)
	# create list with year and scope
	year_scope <- strsplit(year_scope_str, "_", fixed=TRUE)
	year <- year_scope[[1]][1]
	scope <- year_scope[[1]][2]

	if (combined) {

		# create matrices to use in models
		preds_2008 <- buildPredictionMatrix(polls2008_iowa, polls2008_national, "2008", "iowa", results2008)
		preds_2012 <- buildPredictionMatrix(polls2012_iowa, polls2012_national, "2012", "iowa", results2012)

		# combine into overall matrix
		preds_train <- rbind(preds_2008, preds_2012)
		results_train <- rbind(results2008, results2012)

		print(preds_train)

	} else {

		# create matrix to use in models
		preds_train <- buildPredictionMatrix(state_polling_df, natl_polling_df, year, scope, results_df)
		results_train <- results_df
	}

	# build matrix to predict 2016
	preds_2016 <- buildPredictionMatrix(polls2016_iowa, polls2016_national, "2016", "iowa", candidates_2016)

	print(preds_2016)


	########### BUILD MODELS ###########
	
	# run simple linear regression 
	linear_model <- lm(results_train$percentage ~ ., data=as.data.frame(preds_train))
	print("############## LINEAR ##############")
	print(summary(linear_model))
	print(predict(linear_model, newdata = as.data.frame(preds_2016)))

	# run LASSO on predictors
	lasso <- glmnet(x = preds_train, y = results_train$percentage)

	# cross validate lasso
	lasso_cv<- cv.glmnet(x = preds_train, y = results_train$percentage, nfolds = nrow(preds_train))
	# plot(lasso_cv)

	# check RMSE
	lasso_train_preds <- predict(lasso, newx=preds_train, s = lasso_cv$lambda.min )
	print("Training RMSE:")
	print(rmse(results_train$percentage, lasso_train_preds))

	# print out final model chosen by lasso
	lasso_lowestMSE_index <- which(lasso$lambda == lasso_cv$lambda.min)
	print("############## LASSO BETAS ##############")
	print(lasso$beta[,lasso_lowestMSE_index])



	########### PREDICT 2016 ###########

	# predict 2016 using lasso
	lasso_preds <- predict(lasso, newx=preds_2016, s = lasso_cv$lambda.min )

	# scale and sort results
	df_preds_2016 <- scaleSortResults(lasso_preds)

	return(df_preds_2016)

	# return(lasso)

	# return(linear_model)
}


combinePredictorsIntoModel(polls2012_iowa, polls2012_national, results2012, TRUE)

combinePredictorsIntoModel(polls2008_iowa, polls2008_national, results2008, FALSE)
combinePredictorsIntoModel(polls2012_iowa, polls2012_national, results2012, FALSE)










