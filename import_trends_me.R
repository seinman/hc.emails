#################################################
# PART 2: IMPORT TRENDS & IDENTIFY OUTLIERS, MIDDLE EAST
##################################################
# I theorise that there are two groups of data: 'normal' observations and
# 'exceptional' observations. The latter group would correspond to a
# reaction to a major international event. We seek to identify using data
# from Google trends which observations are exceptional. We will use this
# to construct an 'exceptional' variable in our dataset of emails. We will
# use an EM algorithm to do so.

gfile <- "/home/beeb/Documents/Data_Science/HC emails/"
setwd(gfile)

# Bring in pre-made functions
source('createfunctions.R')
trends <- read.csv("trendsme/report(1).csv", skip = 4, header = TRUE, 
                   nrows = 140, stringsAsFactors = FALSE)

# import all the data for those data which are available weekly
for(i in 2:30) {
  filename <- paste0("trendsme/report(", i, ").csv")
  newfile <- read.csv(filename, skip = 4, header = TRUE, 
                      nrows = 140, stringsAsFactors = FALSE)
  trends <- merge(trends, newfile, by = "Week")
}

#take a snapshot of the names for future use
me.words <- names(trends)

# Our model will take into account two auto-regressive terms
vars <- length(trends)
# create the lagged variables
for(j in 2:vars) {
  position1 <- (vars-3) + 2 * j
  position2 <- (vars-2) + 2 * j
  trends[position1] <- create.lag(trends[[j]])
  trends[position2] <- create.lag(trends[[j]], steps = 2)
  term <- names(trends[j])
  newname1 <- paste0(term, "lag1")
  newname2 <- paste0(term, "lag2")
  names(trends)[position1:position2] <- c(newname1, newname2)
}

# add in the week variable
weekbeg1 <- substr(trends$Week, 1, 10)
weekbeg <- strptime(weekbeg1, format = "%Y-%m-%d")

weekend1 <- substr(trends$Week, 14, 24)
weekend <- strptime(weekend1, format = "%Y-%m-%d")

trends$weekbeg <- weekbeg
trends$weekend <- weekend

# We will now create AN ALMIGHTY DATAFRAME from whence all calculations shall
# occur.
# Step one: some of the words throw up errors when the EM algorithm works
# on them, due to singularity. These words I will simply not run the algorithm
# on.
me.words.new <- setdiff(me.words, 'Week')
no.probs <- c( 'syria', 'iran', 'tehran', 
              'qatar', 'egypt', 'algeria', 'yemen',
              'netanyahu',  'ayatollah','benghazi',
              'morsi', 'mubarak', "tripoli","gaddafi")
me.words.new <- setdiff(me.words.new, no.probs)

# Step two: create vectors of all the probabilities of being an outlier
almighty <- data.frame(rep(0,138))
q <- 2
for( i in me.words.new) {
  cat(i)
  positions <- grep(i, names(trends))
  t <- trends[positions[[1]]]
  l1 <- unlist(trends[positions[[2]]])
  l2 <- unlist(trends[positions[[3]]])
  y <- unlist(t)
  l1 <- l1[3:length(y)]
  l2 <- l2[3:length(y)]
  y <- y[3:length(y)]  
  x <- matrix(append(l1, l2), nrow = length(y), ncol = 2)
  x <- cbind(1, x)
  d <- em(y, x, 0.1)
  almighty$d <- d$probabilities
  names(almighty)[q] <- i
  q <- q + 1
}
# For some godforsaken reason, the EM algorithm has sometimes assigned a
# probability of p=1 being an outlier, and sometimes p=0. The next few
# lines of code will fix this.
for(i in setdiff(2:length(almighty), 8)) {
  c0 <- sum(almighty[i] < 0.5)
  c1 <- sum(almighty[i] > 0.5)
  if (c1 > c0) {
    almighty[i] <- 1 - almighty[i]
  }
}

# HURRAH!
almighty$week <- groupweek(weekbeg)[3:140]

