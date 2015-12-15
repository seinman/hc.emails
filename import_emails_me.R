##########################################################
# PART 3: IMPORT ME EMAILS & CLEAN DATA
#########################################################
# This file imports the data from Hillary Clinton's emails, classifies
# them depending on what country they're about, and creates a 

filepath <- '/home/beeb/Documents/Data_Science/HC emails'
setwd(filepath)
library(stringr)
library(dplyr)
library(ggplot2)
source('createfunctions.R')
source('import_trends_me.R')
emails <- read.csv('output/Emails8.csv', header=TRUE, stringsAsFactors = FALSE)

#MidEast words
me.words <- c("islam", "arab", "muslim", "sunni", "shiite", "kurd", "turk",
              "libya", "benghazi", "tripoli", 'gaddafi',
              "syria", "asad", "damascus",  "morsi",
              "iran", "ayatollah", "ahmadinejad", "tehran",
              "israel", "palestin", "jerusalem", "netanyahu",
              "egypt", "saudi", "lebanon", "jordan", "tunisia", "mubarak",
              "morocco", "algeria", "qatar", "yemen"
)
#Extract the body text of the emails as lower-case
body.text <- str_to_lower(as.vector(emails$ExtractedBodyText))

#Extract the body text of the emails as lower-case
body.text <- str_to_lower(as.vector(emails$ExtractedBodyText))
body.text <- strsplit(body.text, " ")
categorise.emails.me.body <- data.frame(rep(0,7945))
for(i in 1:length(me.words)) {
  test <- lapply(body.text, grep, pattern = me.words[i])
  test.2 <- sapply(test, length)
  categorise.emails.me.body[i] <- test.2
}
names(categorise.emails.me.body) <- me.words

# Find which emails feature words to do with the middle east, by word
#categorise.emails.me.body <- as.data.frame(sapply(me.words, grepl, x=body.text))
#names(categorise.emails.me.body) <- me.words

# Find which emails feature at least one word to do with the middle east
categorise.emails.me.body$all.me <- rowSums(categorise.emails.me.body)

# Now do the same with the subject of the email
subject <- str_to_lower(as.vector(emails$MetadataSubject))
categorise.emails.me.subject <- as.data.frame(sapply(me.words, grepl, x=subject))
names(categorise.emails.me.subject) <- me.words
categorise.emails.me.subject$all.me <- rowSums(categorise.emails.me.subject)

# Code to check how many of each word appears
colSums(categorise.emails.me.subject)
colSums(categorise.emails.me.body)

# Put them together and add a date
categorise.emails.me <- data.frame(body = categorise.emails.me.body$all.me, 
                                   subject = categorise.emails.me.subject$all.me,
                                   date = strptime(substr(emails$MetadataDateSent, 1, 10),
                                                   format = "%Y-%m-%d"))
categorise.emails.me$date <- as.POSIXct(categorise.emails.me$date)

# These lines of code add in the individual observations for each word
i = 1
for(k in me.words) {
  categorise.emails.me$newvar <-  categorise.emails.me.body[[i]] + categorise.emails.me.subject[[i]]
  names(categorise.emails.me)[length(categorise.emails.me)] <- k
  i <- i + 1
}

# Drop observations without a date
categorise.emails.me <- filter(categorise.emails.me, is.na(date) == FALSE)
# Group observations by week
categorise.emails.me$week <- groupweek(categorise.emails.me$date)
categorise.emails.me <- group_by(categorise.emails.me, week) %>%
  summarise(islam = sum(islam), arab = sum(arab), muslim = sum(muslim),
            sunni = sum(sunni), shiite = sum(shiite), kurd = sum(kurd),
            turk = sum(turk), libya = sum(libya), benghazi = sum(benghazi),
            tripoli = sum(tripoli), syria = sum(syria),
            damascus = sum(damascus), gaddafi = sum(gaddafi),
            iran = sum(iran), ayatollah = sum(ayatollah), ahmadinejad = sum(ahmadinejad),
            tehran = sum(tehran), 
            israel = sum(israel), palestin = sum(palestin), jerusalem = sum(jerusalem),
            netanyahu = sum(netanyahu), egypt = sum(egypt), saudi = sum(saudi),
            lebanon = sum(lebanon), jordan = sum(jordan), tunisia = sum(tunisia),
            morsi = sum(morsi), mubarak = sum(mubarak), morocco = sum(morocco),
            algeria = sum(algeria), qatar = sum(qatar), yemen = sum(yemen),
            date = min(date))

#Set a copy of this aside for making a graph
categorise.emails.me.for.graph <- filter(categorise.emails.me,  
                                         date > "2009-04-01" & date < "2011-01-12")
  
# create the lags
len <- length(categorise.emails.me)
for(j in 2:(len-1)) {
  position1 <- (len - 3) + 2 * j
  position2 <- (len - 2) + 2 * j
  categorise.emails.me[position1] <- create.lag(categorise.emails.me[[j]])
  categorise.emails.me[position2] <- create.lag(categorise.emails.me[[j]], steps = 2)
  term <- names(categorise.emails.me[j])
  newname1 <- paste0(term, "lag1")
  newname2 <- paste0(term, "lag2")
  names(categorise.emails.me)[position1:position2] <- c(newname1, newname2)
}

almightier <- merge(categorise.emails.me, almighty, by.x = 'week', by.y = 'week')
attach(categorise.emails.me)

#test <- islam + arab + muslim + sunni + shiite + kurd + turk + libya +
#  benghazi + tripoli + iran + ayatollah + ahmadinejad + tehran +
#  israel + palestin + jerusalem + netanyahu + egypt + saudi + lebanon +
#  jordan + tunisia + morsi + mubarak + morocco + algeria + qatar + yemen
