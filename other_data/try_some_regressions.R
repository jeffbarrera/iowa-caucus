## may need to install lpridge
# install.packages("lpridge")
# More: http://artax.karlin.mff.cuni.cz/r-help/library/lpridge/html/lpepa.html

# more about epanechnikov : http://www.bauer.uh.edu/rsusmel/phd/ec1-26.pdf (see p. 20 )
library(lpridge)

##############
# CONSTANTS
##############
FIRST_POLLING_COL <- 12
VOTE_SHARE_COL <- 3


# fix

setwd("/Users/jfenton/github-whitelabel/polisci_355b/iowa-caucus")

##############
# LOAD AND CONVERT DATASETS
##############


# load results datasets
results2008 <- read.csv('results_2008.csv', stringsAsFactors=FALSE)
results2012 <- read.csv('results_2012.csv', stringsAsFactors=FALSE)

# load polling datasets
polls2008 <- read.csv('pollster/data/cleaned_2008_iowa.csv', stringsAsFactors=FALSE)
polls2012 <- read.csv('pollster/data/cleaned_2012_iowa.csv', stringsAsFactors=FALSE)




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
polls2012 <- cleanData(polls2012)





##############
# play with data
##############

#candidate = "ROMNEY"
candidate = "Santorum"


# polling_df = na.omit(polls2012)

polling_df = polls2012
results_df = results2012

cand_indx <- which(colnames(polling_df) == candidate)


this_df = data.frame(polls2012[,cand_indx], polls2012$days_to_caucus)
colnames(this_df) <- c("candidate_poll", "days_to_caucus")
this_df = na.omit(this_df)
this_df$days_to_caucus = -this_df$days_to_caucus


# plot(candidate_poll ~ days_to_caucus, data=this_df)
plot(candidate_poll ~ days_to_caucus, data=this_df, xlim=c(-100,0), ylim=c(0,40))

# add actual results line
abline(h=results_df[results_df$Candidate==candidate, VOTE_SHARE_COL], lwd=2)
	
# Try lowess regression using a 20% sample of the data. (default is 2/3). 
cand_lowess = lowess(this_df$days_to_caucus, this_df$candidate_poll, f=.2)
lines(cand_lowess, col="orange")


# Use linear regression with an epanechnikov window
# see chart on p. 8 here: http://www.bauer.uh.edu/rsusmel/phd/ec1-26.pdf
#  Epanechnikov is entirely contained within the -1,1 range and is 'steeper' than a gaussian
#  https://en.wikipedia.org/wiki/Kernel_density_estimation
# The Epanechnikov kernel is optimal in a mean square error sense
cand_epan <- lpepa(this_df$days_to_caucus, this_df$candidate_poll, bandw=5)
lines(cand_epan$x.out, cand_epan$est,  col="red")


# Second order polynomial regression with an epanechnikov window
cand_epan <- lpepa(this_df$days_to_caucus, this_df$candidate_poll, bandw=5, order=2, n.out=1000)
lines(cand_epan$x.out, cand_epan$est,  col="red")

# it doesn't work with predict, so use a linear interpolation function
max_x = max(cand_epan$x.out)
trend_start_x = max_x - 5
y_estimate_trend_start = approx(cand_epan$x.out, cand_epan$est,trend_start_x)
y_val_trend_end = approx(cand_epan$x.out, cand_epan$est,max_x)

# just using 5 as arbitrary value, but linearly interpolate to y intercept

poll_trend <- structure(list(  y=c(trend_start_x, max_x), x=c(y_estimate_trend_start$y,y_val_trend_end$y)), .Names = c("x", "y"))
# switch the axes so we get the x_intercept
x_intercept = unname(lm(y~x,data=poll_trend)$coefficients[1])


# Standard linear regression
lm_model <- lm(this_df$candidate_poll ~ this_df$days_to_caucus)
abline(lm_model, col="red")


# "The Nadaraya–Watson kernel regression estimate" ; smoothing with a gaussian kernel
# https://en.wikipedia.org/wiki/Kernel_regression
gaussian_smooth = ksmooth(this_df$days_to_caucus, this_df$candidate_poll, "normal", bandwidth = 10)
lines(gaussian_smooth,  col="pink", lwd=2)

# max x value: 
max_x = max(gaussian_smooth$x)
# interpolate in sample value.
y_estimate = approx(gaussian_smooth$x, gaussian_smooth$y,max_x-5)
# This won't work out of sample though. 



