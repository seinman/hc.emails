########################################################################
# PART 4: ANALYSIS ME
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

source('import_trends_me.R')
source('import_emails_me.R')
source('createfunctions.R')

# We are working off the 'almightier' dataset painstakingly created over the
# last 5-day marathon of coding hell. I dream of a day when I can frolic freely
# through El Raval again. Twill be many more days yet, I fear.
# Method:
# For each of the words in me.words, perform a Poisson regression (being sure
# to pronounce the word 'poisson' in a bad French accent) and find the value
# of beta0 for each one and then just add the fuckers up.
# Yes. That is what I intend to do with my carefully-won statistics skills.
# I intend to add numbers up. Problems? Problems?! I think not.
# I can haz MSc nao plz?
# On a serious note, if X1 ~ Po(lambda1) and X2 ~ Po(lambda2) then X1 + X2 ~
# Po(lambda1 + lambda2). We are interested in the 'background' likelihood of 
# an email about the ME being sent - ie, the likelihood if there was no email
# sent for two days before and there was no external stimulus. This will be
# equal to the exponential of all the b0s for all the different words added
# together.
# I CAN HAZ MSC NAO PLZ
# God I hope this works.

#First, we only examine the days where a lot of emails were sent/received:
almightier <- filter(almightier, date > "2009-03-19" & date < "2011-01-01")

varnames <- tolower(names(almightier))
# For some of the words, the EM algorithm didn't work due to singularity.
# We will regress these words on their lagged values alone.

# An additional number of words won't work in the GLM model due to reasons.
singular <- c( 'sunni', 'shiite', 'jerusalem', 'palestin',
                'libya', 'damascus', 'kurd',
               'morocco',  
               'tunisia')

me.words.new <- setdiff(me.words.new, singular)

# Palestine became palestin when we searched it into hc's emails so as to
# pick up words like 'palestinian', whereas it is palestine in the trends data
me.words.new <- setdiff(me.words.new, 'palestine')

# Similarly, we searched 'Jordan' into Google trends and 'jordan' into HC's
# emails so as to not pick up references to Michael Jordan etc
me.words.new <- tolower(me.words.new)

obs <- length(almightier[[1]])
almightybeta0 <- rep(0, 32)
almightybetacov <- rep(0,32)
listofwords <- rep('a', 32)
j <- 1
for(word in me.words.new) {
  cat(word)
  cat(j)
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
  #if(j==28|j==29) { cat(word)}
  j <- j + 1
}

# Now we deal with the singular ones
for(word in singular) {
  cat(word)
  cat(j)
  positions <- grep(word, varnames)
  regressor <- unlist(almightier[positions[[1]]])
  lag1 <- unlist(almightier[positions[[2]]])
  lag2 <- unlist(almightier[positions[[3]]])
  outlierprob <- unlist(almightier[positions[[4]]])
  q <- glm(regressor ~ lag1 + lag2 + outlierprob, family=poisson(link='log'))
  almightybeta0[j] <- q$coefficients[1]
  almightybetacov[j] <- summary(q)$cov.scaled[1,1]
  listofwords[j] <- word
  #if(j==28|j==29) { cat(word)}  
  j <- j + 1
}

# now we do the same for the words that we don't have outlier probabilities for
# bare in mind that some of them also throw up singular matrices
both <- c('ayatollah','benghazi','morsi', 'mubarak', "tripoli","gaddafi")
no.probs <- setdiff(no.probs, both)
for(word in no.probs) {
  cat(word)
  cat(j)
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
  #if(j==28|j==29) { cat(word)}  
  j <- j + 1
}

for(word in both) {
  cat(word)
  cat(j)
  positions <- grep(word, varnames)
  regressor <- unlist(almightier[positions[[1]]])
  lag1 <- unlist(almightier[positions[[2]]])
  lag2 <- unlist(almightier[positions[[3]]])
  q <- glm(regressor ~ lag1 + lag2, family=poisson(link='log'))
  almightybeta0[j] <- q$coefficients[1]
  almightybetacov[j] <- summary(q)$cov.scaled[1,1]
  listofwords[j] <- word
  #if(j==28|j==29) { cat(word)}  
  j <- j+1
}

results.me <- data.frame(word = listofwords, beta0 = almightybeta0, var = almightybetacov, 
                      stdev = sqrt(almightybetacov),
                      beta0.lower = almightybeta0 - 1.96 * sqrt(almightybetacov),
                      beta0.higher = almightybeta0 + 1.96 * sqrt(almightybetacov),
                      estimate = exp(almightybeta0))

results.me <- mutate(results.me, estimate.higher = exp(beta0.higher),
                      estimate.lower = exp(beta0.lower))
