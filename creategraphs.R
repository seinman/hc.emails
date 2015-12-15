##############################################################3
# GRAPHS
####################################################################

# Alright, wasn't that fun?
# Now for something a bit more relaxed.
filepath <- '/home/beeb/Documents/Data_Science/HC emails'
setwd(filepath)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(tables)
source('import_emails_ea.R')
source('import_emails_me.R')
# First, we show that the EM algorithm has actually worked.
source('import_trends_me.R')

#############################################################################
# Create a plot showing how the EM algorithm worked for search term 'arab'
###############################################################################
prep <- data.frame( probs = almighty$arab, 
                    week = almighty$week,
                    weekbeg = trends$weekbeg[3:length(trends$weekbeg)],
                    fiftyplus = 0,
                    timemap = NA,
                    hits = trends$arab[3:length(trends$arab)])
prep <- merge(prep, categorise.emails.me, by = 'week')
prep$arab <- (prep$arab/max(prep$arab))*100
prep$fiftyplus <- prep$probs>0.5
prep$map <- prep$fiftyplus * prep$probs
prep$timemap[prep$fiftyplus] <- as.POSIXct(prep$weekbeg[prep$fiftyplus], origin = '1970-01-01')

ggplot(prep, aes(x = weekbeg, y = hits)) +
  geom_line(colour='black') +
  geom_vline(aes(xintercept=timemap, colour = map)) +
  scale_colour_gradient(high = 'deeppink1', low = 'lightpink2',
                        expression('Probability of Outlier'),
                        limits=c(0.5,1)) +
  ylim(c(0,100)) +
  labs(x = 'Date', y = 'Internet Searches, % of max', 
       title = 'Results of EM Algorithm on Search Term: "Arab"')

########################################################################################
# Now compare how the algorithm worked on various different search terms
#######################################################################################
almighty$Week <- trends$Week[3:length(trends$Week)]
prep <- merge(almighty, categorise.emails.me, by = 'week')
prep <- merge(prep, trends, by = 'Week')
prep2 <- data.frame( probs = 0, 
                    week = prep$week,
                    weekbeg = prep$date,
                    fiftyplus = 0,
                    timemap = NA,
                    hits = 0)

for( word in setdiff(names(almighty), c('rep.0..138.', 'week', 'Week'))) {
  cat(word)
  prep2$probs <- prep[[grep(word, names(prep))[1]]]
  prep2$emails <- prep[[grep(word, names(prep))[2]]]
  #standardise
  prep2$emails <- (prep2$emails/max(prep2$emails))*100
  avg.emails <- round(mean(prep2$emails), 2)
  prep2$hits <- prep[[grep(word, names(prep))[5]]]
  prep2$fiftyplus <- prep2$probs>0.5
  prep2$map <- prep2$fiftyplus * prep2$probs
  prep2$timemap[prep2$fiftyplus==TRUE] <- as.POSIXct(prep2$weekbeg[prep2$fiftyplus==TRUE],
                                                     origin = '1970-01-01')
q <-  ggplot(prep2, aes(x = weekbeg, y = emails)) +
    geom_line() +
    geom_line(aes(y = hits), colour = 'darkblue') +
    annotate('text', x =  as.POSIXct("2009-02-01"), y = 80, size = 3.5,
             label = paste('Average Emails Per Day:', avg.emails)) +
    geom_vline(aes(xintercept=timemap, colour = map)) +
    scale_colour_gradient(high = 'deeppink1', low = 'lightpink2',
                          expression('Probability of Outlier'),
                          limits=c(0.5,1)) +
    ylim(c(0,100)) +
    labs(x = 'Date', y = 'Internet Searches, % of max',
         title = paste('Results of EM Algorithm on Search Term:', word))
assign(word, q)
  prep2$timemap <- NA
}
########################################################################################
# Create a plot of number of emails for different regions by time
#######################################################################################
prep <- merge(categorise.emails.me.for.graph, categorise.emails.ea.for.graph,
              by.x = 'week', by.y = 'week') %>%
  mutate(all.me = islam + arab + muslim + sunni + shiite + kurd + turk +
              libya + benghazi + tripoli + syria + damascus + gaddafi + iran +
              ayatollah + ahmadinejad + tehran + israel + palestin + jerusalem +
              netanyahu + egypt + saudi + lebanon + jordan + tunisia + morsi +
              mubarak + morocco + algeria + qatar + yemen,
            all.ea = china + beijing + jinping + jintao + renminbi + taiwan +
              taipei + korea + seoul + pyong + japan + vietnam + india + 
              thailand + philippines + tibet + asean + malaysia + chinese +
              tokyo + yen ,
         date.y = as.Date(date.y)) %>%
  select(all.me, all.ea, date.y) %>%
  melt(id.vars = 'date.y', measure.vars = c('all.me', 'all.ea'))

ggplot(prep, aes(x = date.y, y = value, colour = variable, fill = variable)) + 
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('deeppink2', 'deepskyblue'), 
                    labels = c('Middle East', 'East Asia'), name = 'Region') +
  labs(x = 'Date', y = 'Number of Emails', 
       title = 'Mentions of Different Regions in Emails Over Time') +
  scale_x_date(breaks ="1 year", labels = date_format("%Y-%m")) +
  scale_y_discrete(breaks = seq(0, 60, 10))

# Now repeat the above exercise but only for the time period under consideration
prep <- filter(prep, date.y > "2009-04-01" & date.y < "2011-01-12")
ggplot(prep, aes(x = date.y, y = value, fill = variable)) + geom_bar(stat = 'identity') +
  scale_fill_manual(values = c('deeppink2', 'deepskyblue'), 
                    labels = c('Middle East', 'East Asia'), name = 'Region') +
  labs(x = 'Date', y = 'Number of Emails', 
       title = 'Mentions of Different Regions in Emails Over Time') +
  scale_x_date(breaks ="2 months", labels = date_format("%Y-%m")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(breaks = seq(0, 60, 10))

###############################################################################
# Now we get the overall number of emails sent over the time period
###############################################################################
prep <- data.frame(date = strptime(substr(emails$MetadataDateSent, 1, 10), format = "%Y-%m-%d"))
prep$date <- as.Date(prep$date)
prep <- group_by(prep, date) %>% summarise(count = n())

ggplot(data = prep, aes(x = date, y = count)) + geom_bar(colour = 'darkorchid1', stat = 'identity') +
  geom_vline(xintercept = as.numeric(as.Date("2009-03-19"))) +
  geom_vline(xintercept = as.numeric(as.Date("2011-01-01"))) +   
  scale_x_date(breaks ="6 months", labels = date_format("%Y-%m")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(breaks = seq(0, 60, 10)) +
  labs(x = 'Date', y = "Number of Emails", title = "Number of Emails Sent and Received by Clinton Over Time")

##################################################################################################
# Show the final results of the analysis
###################################################################################################

source('analysis_ea.R')
source('analysis_me.R')

results.ea$region <- 'ea'
results.me$region <- 'me'
results <- rbind(results.ea, results.me)
results <- arrange(results, region, desc(estimate))

#Some of these estimates have come up with extremely high variances. We exclude these.
results <- filter(results, var<100)

for(region in c('me', 'ea')){
  other <- sum(results$estimate[results$estimate < 1 & results$region == region])
  assign(paste0('other.', region), other)
  other.higher <- sum(results$estimate.higher[results$estimate< 1 & results$region == region])
  assign(paste0('other.', region, '.higher'), other.higher)
  other.lower <- sum(results$estimate.lower[results$estimate< 1 & results$region == region])
  assign(paste0('other.', region, '.lower'), other.lower)
}

results2 <- filter(results, estimate >= 1) %>%
  rbind(data.frame(word = c('other me', 'other ea'), beta0 = c(NA, NA), var = c(NA, NA),
                   stdev = c(NA, NA), beta0.lower = c(NA, NA), beta0.higher = c(NA, NA),
                   estimate = c(other.me, other.ea), 
                   estimate.higher = c(other.me.higher, other.ea.higher),
                   estimate.lower = c(other.me.lower, other.ea.lower),
                   region = c('me', 'ea')))
results2 <- arrange(results2, region, desc(estimate))
results2$wordplace <- max(results2$estimate[results2$region=='ea'])
results2$wordplace[results2$region=='me'] <- max(results2$estimate[results2$region=='me'])
for(i in 2:nrow(results2)){
  if(results2$region[i] == results2$region[i-1]) {
    results2$wordplace[i] <- results2$wordplace[i-1] + results2$estimate[i]
  }
}

ggplot(results2, aes(x = as.factor(region), y = estimate, fill = word)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = word, y = wordplace - 0.35)) +
  labs(x = 'Region', y = 'Mentions', title = 'Estimated Number of Mentions Per Day') +
  ylim(c(0,35))


# Now we prepare the data frame for analysing the highest EA with the lowest ME.
results2$compare <- results2$estimate.higher
results2$compare[results2$region=='me'] <- results2$estimate.lower[results2$region=='me']

results2 <- arrange(results2, region, desc(compare))
results2$wordplace.compare <- max(results2$compare[results2$region=='ea'])
results2$wordplace.compare[results2$region=='me'] <- max(results2$compare[results2$region=='me'])
for(i in 2:nrow(results2)){
  if(results2$region[i] == results2$region[i-1]) {
    results2$wordplace.compare[i] <- results2$wordplace.compare[i-1] + results2$compare[i]
  }
}


ggplot(results2, aes(x = as.factor(region), y = compare, fill = word)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = word, y = wordplace.compare - 0.35)) +
  labs(x = 'Region', y = 'Emails', 
       title = 'Estimated Max Number of Mentions Per Day EA vs Min Number of Mentions Per Day ME') +
  ylim(c(0,35))


#######################################################################################
# Now we do the same, but for the initial data
###################################################################################

initial.ea <- colMeans(select(categorise.emails.ea.for.graph, -week, -date))
initial.me <- colMeans(select(categorise.emails.me.for.graph, -week, -date))
initial.ea <- melt(initial.ea)
initial.ea$region <- 'ea'
initial.me <- melt(initial.me)
initial.me$region <- 'me'
initial <- rbind(initial.me, initial.ea)
initial$word <- rownames(initial)
other.me <- sum(initial$value[initial$value < 1 & initial$region == 'me'])
other.ea <- sum(initial$value[initial$value < 1 & initial$region == 'ea'])

initial2 <- filter(initial, value > 1) %>%
  rbind(data.frame(value = c(other.me, other.ea), region = c('me', 'ea'), word = c('other me',
                                                                                   'other ea')))
initial2 <- arrange(initial2, region, desc(value))
initial2$wordplace <- max(initial2$value[initial2$region=='ea'])
initial2$wordplace[initial2$region=='me'] <-  max(initial2$value[initial2$region=='me'])
for(i in 2:nrow(initial2)){
  if(initial2$region[i] == initial2$region[i-1]) {
    initial2$wordplace[i] <- initial2$wordplace[i-1] + initial2$value[i]
  }
}

ggplot(initial2, aes(x = as.factor(region), y = value, fill = word)) + 
  geom_bar(stat = 'identity') +
  geom_text(aes(label = word, y = wordplace - 0.65)) +
  labs(x = 'Region', y = 'Mentions', title = 'Actual Number of Mentions Per Day') +
  ylim(c(0,35))
##################################################################################
# Create a table of the different values of beta
###################################################################################

initial.total <- group_by(initial2, region) %>%
  summarise(mentions = sum(value))
initial.total$start <- 'initial'

results.total <- group_by(results2, region) %>%
  summarise(mentions = sum(estimate))
results.total$start <- 'estimate'

results.maxmin <- group_by(results2, region) %>%
  summarise(mentions.higher = sum(estimate.higher), mentions.lower = sum(estimate.lower))

results.maxmin$mentions <- results.maxmin$mentions.lower
results.maxmin$mentions[results.maxmin$region == 'ea'] <- results.maxmin$mentions.higher[results.maxmin$region == 'ea']
results.maxmin <- select(results.maxmin, region, mentions)
results.maxmin$start <- 'max EA vs min ME'

compare <- rbind(initial.total, results.total)
compare <- rbind(compare, results.maxmin)
compare <- spread(compare, start, mentions)
compare <- select(compare, -region)
compare <- sapply(compare, round, digits = 2)
rownames(compare) <- c("EA", "ME")

write.table.tabular(compare)
