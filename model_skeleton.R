# load & clean datasets

# weightPollingBy538Rankings
# --------------------------
# Given a df of polling data and a df (or some other object) of 538's pollster rankings, duplicate polling observations to weight by 538's rankings.
# Return a df with weighted polling data

# predictFromPollTrends
# ---------------------
# Predict election day vote share by extrapolating from weighted polls regressed against time. 
# Return df/matrix of candidate names (rows) and predicted vote shares from different models (cols)

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