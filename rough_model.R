
# Jeff's working directory -- comment out and override
setwd("~//Documents/School/Stanford/Classes/Poli Sci 355B/Challenge 1/iowa-caucus")

# load provided dataset
data2012 <- read.csv('2012-pollData.csv')
data2016 <- read.csv('2016-pollData.csv')

head(data2012)

rownames(data2016) <- paste (data2016$candidateName, data2016$primaryPoll, sep = "_")

# create test dataset to play with
test_2012 <- subset(data2012, primaryPoll=='iowa', select = betfair:poll10)

test_2012$polls <- sum()

mean(as.vector(t(as.matrix(test_2012[1,2:11]))))

test_2016 <- subset(data2016, primaryPoll=='iowa', select = betfair:poll10)
test_vote_share <- data2012[data2012$primaryPoll=='iowa', 5]

test_vote_share


test_lm <- lm(test_vote_share ~ ., data=test_2012)
summary(test_lm)

predict(test_lm, newdata=test_2016)


# LASSO regression - 
library(glmnet)
lasso_logist <- glmnet(x = as.matrix(test_2012), y = test_vote_share)


# # ?subset


paste (data2016$candidateName, data2016$primaryPoll, sep = "_")


# # data2012[,-5]

# # ?glmnet


# class(test_vote_share)

# as.matrix(test_dataset)