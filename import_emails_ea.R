##########################################################################
# EAST ASIA
########################################################################
filepath <- '/home/beeb/Documents/Data_Science/HC emails'
setwd(filepath)
library(stringr)
library(dplyr)
library(ggplot2)
source('createfunctions.R')
source('import_trends_ea.R')
emails <- read.csv('output/Emails8.csv', header=TRUE, stringsAsFactors = FALSE)

# Create a vector of East Asia-specific words
ea.words <- c("china", "chinese", "beijing", "jintao", "jinping", "renminbi",
              "taiwan", "taipei", "vietnam", "india", "thailand",
              "korea", "seoul", "pyong", 'asean', 'philippines', 'tibet',
              "japan", "tokyo", "yen", 'malaysia'
)
#Extract the body text of the emails as lower-case
body.text <- str_to_lower(as.vector(emails$ExtractedBodyText))
body.text <- strsplit(body.text, " ")
categorise.emails.ea.body <- data.frame(rep(0,7945))
for(i in 1:length(ea.words)) {
  test <- lapply(body.text, grep, pattern = ea.words[i])
  test.2 <- sapply(test, length)
  categorise.emails.ea.body[i] <- test.2
}
names(categorise.emails.ea.body) <- ea.words

# Categorise the emails from their body text
#categorise.emails.ea.body <- as.data.frame(sapply(ea.words, grepl, x=body.text))
#names(categorise.emails.ea.body) <- ea.words
categorise.emails.ea.body$all.ea <- rowSums(categorise.emails.ea.body)

# Categorise the emails from their subject
subject <- str_to_lower(as.vector(emails$MetadataSubject))
categorise.emails.ea.subject <- as.data.frame(sapply(ea.words, grepl, x=subject))
names(categorise.emails.ea.subject) <- ea.words
categorise.emails.ea.subject$all.ea <- rowSums(categorise.emails.ea.subject)

# Put them together and add a date
categorise.emails.ea <- data.frame(body = categorise.emails.ea.body$all.ea, 
                                   subject = categorise.emails.ea.subject$all.ea,
                                   date = strptime(substr(emails$MetadataDateSent, 1, 10),
                                                   format = "%Y-%m-%d"))
categorise.emails.ea$date <- as.POSIXct(categorise.emails.ea$date)


# This sees how many emails have EA words in ONLY body or ONLY subject
test.emails.ea <- filter(categorise.emails.ea, body >0 & subject == 0 |
                           body == 0 & subject > 0)

# These lines of code add in the individual observations for each word
i = 1
for(k in ea.words) {
  categorise.emails.ea$newvar <-  categorise.emails.ea.body[[i]] + categorise.emails.ea.subject[[i]]
  names(categorise.emails.ea)[length(categorise.emails.ea)] <- k
  i <- i + 1
}

# Drop observations without a date
categorise.emails.ea <- filter(categorise.emails.ea, is.na(date) == FALSE)
# Group observations by week
categorise.emails.ea$week <- groupweek(categorise.emails.ea$date)
categorise.emails.ea <- group_by(categorise.emails.ea, week) %>%
  summarise(china = sum(china), beijing = sum(beijing),jinping = sum(jinping),
      jintao = sum(jintao), renminbi = sum(renminbi), taiwan = sum(taiwan), taipei = sum(taipei),
      korea = sum(korea), seoul = sum(seoul), pyong = sum(pyong), japan = sum(japan),
      vietnam = sum(vietnam), india = sum(india), thailand = sum(thailand),
      philippines = sum(philippines), tibet = sum(tibet), asean = sum(asean),
      malaysia = sum(malaysia), chinese = sum(chinese),
      tokyo = sum(tokyo), yen = sum(yen), date = min(date))


#Set a copy of this aside for making a graph
categorise.emails.ea.for.graph <- filter(categorise.emails.ea,  
                                         date > "2009-04-01" & date < "2011-01-12")

# create the lags
len <- length(categorise.emails.ea)
for(j in 2:(len)) {
  position1 <- (len - 3) + 2 * j
  position2 <- (len - 2) + 2 * j
  categorise.emails.ea[position1] <- create.lag(categorise.emails.ea[[j]])
  categorise.emails.ea[position2] <- create.lag(categorise.emails.ea[[j]], steps = 2)
  term <- names(categorise.emails.ea[j])
  newname1 <- paste0(term, "lag1")
  newname2 <- paste0(term, "lag2")
  names(categorise.emails.ea)[position1:position2] <- c(newname1, newname2)
}

almightier <- merge(categorise.emails.ea, almighty, by.x = 'week', by.y = 'week')
attach(categorise.emails.ea)
test <- china + beijing + jintao + renminbi + taiwan + taipei + korea + seoul +
  pyong + japan + tokyo + chinese  + thailand + yen + asean + vietnam +
  india + philippines + tibet + asean + tokyo 