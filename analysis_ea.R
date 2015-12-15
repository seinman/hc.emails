########################################################################
# PART 4: ANALYSIS EA
########################################################################

# This file brings together the functions created by createfunctions.R, the 
# outliers identified by import_trends.R, and the emails imported and cleaned
# in import_emails.R.
# The steps taken are the following: 
# Each word is regressed on two auto-regressive terms plus the likelihood of
# outlierhood. The intercept from each of these added together represents the
# overall 'background' likelihood of a term being used.

filepath <- '/home/beeb/Documents/Data_Science/HC emails'
setwd(filepath)

source('import_trends_ea.R')
source('import_emails_ea.R')
source('createfunctions.R')

varnames <- tolower(names(almightier))

obs <- length(almightier[[1]])
almightybeta0 <- rep(0, 21)
j <- 1

# There's a number of problems that get thrown up here. Firstly, we remove
# the 'week' variable from the list of names because it's not a regressor.
ea.words.2 <- setdiff(ea.words, 'Week')
# A couple of the words in our list throw up non-invertible matrix errors when
# I run my glm code on them, but not when I run R's inbuilt glm function. This
# is the list of them.
singular <- c('china', 'jintao', 
              'taipei', 'thailand', 'tokyo',
              'philippines', 'tibet', 'seoul', 'pyong', 'asean',
               'yen', 'renminbi')
ea.words.2 <- setdiff(ea.words.2, singular)
# There were some codes that for various reasons we couldn't back out the 
# probability of outlierhood for. These are they.
no.probs <- c('beijing', 'korea', 
              'india')
ea.words.2 <- setdiff(ea.words.2, no.probs)
# There are even some words that pull the hat-trick of being *both* non-invertible
# AND not having probabilities.
both <- c('jinping', 'taiwan', 'malaysia')
ea.words.2 <- setdiff(ea.words.2, both)
almightybeta0 <- rep(0, 21)
almightybetacov <- rep(0,21)
listofwords <- rep('a', 21)
j <- 1
for(word in ea.words.2) {
  cat(word)
  positions <- grep(word, varnames)
  regressor <- unlist(almightier[positions[[1]]])
  lag1 <- unlist(almightier[positions[[2]]])
  lag2 <- unlist(almightier[positions[[3]]])
  outlierprob <- unlist(almightier[positions[[4]]])
  testmatrix <- matrix(1, nrow = obs, ncol = 4)
  testmatrix[,2] <- lag1
  testmatrix[,3] <- lag2
  testmatrix[,4] <- outlierprob
  q <- poisson.iwls(regressor, testmatrix)
  almightybeta0[j] <- q$beta[1]
  almightybetacov[j] <- q$cov[1,1]
  listofwords[j] <- word
  j <- j + 1
  }

for(word in singular) {
  cat(word)
  positions <- grep(word, varnames)
  regressor <- unlist(almightier[positions[[1]]])
  lag1 <- unlist(almightier[positions[[2]]])
  lag2 <- unlist(almightier[positions[[3]]])
  outlierprob <- unlist(almightier[positions[[4]]])
  q <- glm(regressor ~ lag1 + lag2 + outlierprob, family=poisson(link='log'))
  almightybeta0[j] <- q$coefficients[1]
  almightybetacov[j] <- summary(q)$cov.scaled[1,1]
  listofwords[j] <- word
  j <- j + 1
}

# now we do the same for the words that we don't have outlier probabilities for
for(word in no.probs) {
  cat(word)
  positions <- grep(word, varnames)
  regressor <- unlist(almightier[positions[[1]]])
  lag1 <- unlist(almightier[positions[[2]]])
  lag2 <- unlist(almightier[positions[[3]]])
  testmatrix <- matrix(1, nrow = obs, ncol = 3)
  testmatrix[,2] <- lag1
  testmatrix[,3] <- lag2
  q <- poisson.iwls(regressor, testmatrix)
  almightybeta0[j] <- q$beta[1]
  almightybetacov[j] <- q$cov[1,1]
  listofwords[j] <- word
  j <- j+1
}

# fuck the fucking world
for(word in both) {
  cat(word)
  positions <- grep(word, varnames)
  regressor <- unlist(almightier[positions[[1]]])
  lag1 <- unlist(almightier[positions[[2]]])
  lag2 <- unlist(almightier[positions[[3]]])
  q <- glm(regressor ~ lag1 + lag2, family=poisson(link='log'))
  almightybeta0[j] <- q$coefficients[1]
  almightybetacov[j] <- summary(q)$cov.scaled[1,1]
  listofwords[j] <- word
  j <- j+1
}
# Collect the results into a data frame
results.ea <- data.frame(word = listofwords, beta0 = almightybeta0, var = almightybetacov, 
                      stdev = sqrt(almightybetacov),
                      beta0.lower = almightybeta0 - 1.96 * sqrt(almightybetacov),
                      beta0.higher = almightybeta0 + 1.96 * sqrt(almightybetacov),
                      estimate = exp(almightybeta0))

results.ea <- mutate(results.ea, estimate.higher = exp(beta0.higher),
                  estimate.lower = exp(beta0.lower))