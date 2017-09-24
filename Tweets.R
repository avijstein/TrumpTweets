# Data Crunching #
library(tidyverse)
library(lubridate)

clean_plot =  theme(axis.line = element_line(colour = "blue"), panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(), panel.border = element_blank(),
                    panel.background = element_blank(), legend.key = element_blank())


tweets = read.csv('TrumpTweets.csv', stringsAsFactors = F)

tweets$Date = ymd(tweets$Date)
tweets$Time = strptime(tweets$Time, format = '%H:%M:%S')
# tweets$Time = hms(tweets$Time)

test = tweets[,c(1:3)]
test$dt = paste(test$Date, test$Time)
test$dt = strptime(test$dt, format = '%y-%m-%d %H:%M:%S')
test$Time = strptime(test$Time, format = '%H:%M:%S')
test$hour_cut = cut(test$Time, breaks = 'hour') # this is so wrong, setting it to today's date
test$month_cut = cut(test$dt, breaks = 'month')
# test$hour_cut = as.Date(test$hour_cut, format = '%y-%m-%d %H:%M:%S')

hours = data.frame(table(test$hour_cut))
hours$Var1 = as.integer(hours$Var1)
names(hours) = c('hour', 'count')

months = data.frame(table(test$month_cut))
months$Var1 = as.integer(months$Var1)
names(months) = c('month', 'count')

# first plot of tweets per hour, hopefully will get month variations soon
ggplot() +
  geom_rect(aes(xmin = 0, xmax = 7, ymin=-Inf, ymax=Inf), alpha = .1, fill = 'grey') +
  geom_rect(aes(xmin = 19, xmax = 24, ymin=-Inf, ymax=Inf), alpha = .1, fill = 'grey') +
  geom_line(data = hours, aes(x = hour, y = count), color = 'blue') +
  # geom_line(aes(x = hour, y = count), color = 'blue', size = 1) +
  labs(x = 'Hour', y = 'Number of Tweets', title = 'Tweets by Hour') +
  clean_plot


# seeing what hour Trump tweets at.
ggplot(data = tweets[1:1000,]) +
  geom_point(aes(x = Date, y = Retweets)) +
  clean_plot


# grepping for exclamation points
exclaim = (gregexpr(pattern = "!", text = tweets$Tweet_Text))
numexclaim = c()
# because I'm python trash and use for loops as defaults. learn dplyr, geez.
for (i in 1:length(exclaim)){
  if (sum(exclaim[[i]]) == -1){
    numexclaim[i] = 0
  }else{
    numexclaim[i] = length(exclaim[[i]])
  }
}

exclaimtweets = data.frame(tweets$Tweet_Text[1:length(exclaim)], numexclaim, tweets$Retweets[1:length(exclaim)])
names(exclaimtweets) = c('text', 'count', 'retweets')

# graphing the number of retweets against the number of exclamation points in the tweet.
ggplot(data = exclaimtweets) +
  geom_point(aes(x = count, y = retweets)) +
  clean_plot





##### OBSERVATIONS #####

# There are a few tweets, I believe right after Trump was elected, that there's an order of magnitude 
# more retweets/favorites. In coming analyses, I might want to exclude this points so they don't skew the 
# analysis and presentation of the data.

# It also looks like sometimes Trump retweeted people who used an awful amount of exclamation points,
# and that showed up as one of his tweets.

