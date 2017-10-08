##### SET UP #####
# Data Crunching #
library(tidyverse) # does this not include dplyr???
library(lubridate)
library(dplyr)
library(ggplot2)

clean_plot =  theme(axis.line = element_line(colour = "blue"), panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(), panel.border = element_blank(),
                    panel.background = element_blank(), legend.key = element_blank())


tweets = read.csv('TrumpTweets.csv', stringsAsFactors = F)


##### HOURLY TWEETS EXPERIMENTING #####
test2 = tweets[,c(1:3,9,10)]
test2$hour_cat = cut(test$Time, breaks = 'hours', labels = F)
names(test2)[4] = 'Favorites'

test2_data = test2 %>%
  group_by(hour_cat) %>%
  summarise('count' = n(),
            'total_rt' = sum(Retweets)/1000000,
            'total_fav' = sum(Favorites)/1000000,
            'tweet_effect' = sum(Retweets/count))

# plotting retweets on volume of tweets per hour
ggplot(data = test2_data) +
  geom_rect(aes(xmin = 0.5, xmax = 7, ymin=0, ymax=Inf), alpha = 1, fill = 'grey') +
  geom_rect(aes(xmin = 19, xmax = 24.5, ymin=0, ymax=Inf), alpha = 1, fill = 'grey') +
  geom_line(aes(x = hour_cat, y = count, color = total_rt), size = 2) +
  scale_color_continuous(low = 'blue', high = 'orange', name = 'Retweets (millions)') +
  labs(x = 'Hours of the Day', y = 'Number of Tweets', title = 'Volume and Popularity of Tweets') +
  clean_plot

ggplot(data = test2_data) +
  geom_rect(aes(xmin = 0.5, xmax = 7, ymin=0, ymax=Inf), alpha = 1, fill = 'grey') +
  geom_rect(aes(xmin = 19, xmax = 24.5, ymin=0, ymax=Inf), alpha = 1, fill = 'grey') +
  geom_line(aes(x = hour_cat, y = tweet_effect), color = 'slateblue1', size = 2) +
  labs(x = 'Hour of the Day', y = 'Retweets per Tweet', title = 'Effectiveness of Tweets') +
  clean_plot


##### MONTHLY TWEET EXPERIMENTING #####
test = tweets[,c(1:3)]
test$dt = paste(test$Date, test$Time)
test$dt = strptime(test$dt, format = '%y-%m-%d %H:%M:%S')
test$Time = strptime(test$Time, format = '%H:%M:%S')
test$month_cut = cut(test$dt, breaks = 'month', labels = F)
test$hour_cut = NA
# test$hour_cut = cut(test$Time, breaks = 'hour') # this is so wrong, it's setting it to today's date
# test$hour_cut = as.Date(test$hour_cut, format = '%y-%m-%d %H:%M:%S')

# I need to find a dplyr method or apply method for this process because for loops are pretty weak.
fulltimecut = data.frame()
for (i in unique(test$month_cut)){
  if (is.na(i)){next}
  print(i)
  hold = test[test$month_cut == i, c('Time', 'dt', 'month_cut', 'hour_cut')]
  hold$hour_cut = cut(hold$Time, breaks = 'hour', labels = F)
  fulltimecut = rbind(fulltimecut, hold)
}

# I don't know why, but this process is converting some values to NAs. I'm dropping them to get preliminary
# results, but I'll need to come back and figure this out.
fulltimecut = fulltimecut[!is.na(fulltimecut$hour_cut),]

comptimecut = data.frame()
for (i in unique(fulltimecut$month_cut)){
  hold = data.frame(table(fulltimecut[fulltimecut$month_cut == i,]$hour_cut))
  hold$Var1 = as.integer(hold$Var1)
  names(hold) = c('hours', 'count')
  hold$month = i
  comptimecut = rbind(comptimecut, hold)
}

Months = factor(fulltimecut$month_cut)
# okay, got a plot breaking it down by month, but it's not that descriptive. It's kinda there.
ggplot() +
  geom_rect(aes(xmin = 0.5, xmax = 7, ymin=0, ymax=Inf), alpha = 1, fill = 'grey') +
  geom_rect(aes(xmin = 19, xmax = 24.5, ymin=0, ymax=Inf), alpha = 1, fill = 'grey') +
  geom_bar(data = fulltimecut, aes(x = hour_cut, fill = Months)) +
  labs(x = 'Hours', y = 'Count', title = 'Trump Tweets each Hour by Month') +
  clean_plot

comp_hours = data.frame(table(fulltimecut$hour_cut))
comp_hours$Var1 = as.integer(comp_hours$Var1)
names(comp_hours) = c('hours', 'count')
comp_hours2 = merge(comp_hours, fulltimecut[,c('hour_cut', 'month_cut')], by.x = 'hours', by.y = 'hour_cut',
                    all = T)


##### EXCLAMATION POINTS #####

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


##### HASHTAGS ##### 
tweetstb = tbl_df(tweets)
# tweetstb = tweetstb %>%
#   filter(!grepl('^RT', Tweet_Text)) # if you need to get rid of retweets, but throws up warnings
hash = paste(tweetstb$Hashtags, collapse = ';')
hash = unlist(strsplit(hash, ';+'))
hashdf = tbl_df(table(hash)) %>% arrange(-n) %>% filter(n > 50)

tweetstb %>%
  select(Hashtags, Retweets) %>%
  arrange(-Retweets) %>%
  filter(Hashtags %in% hashdf[,1]) %>%
  # group_by(Hashtags) %>%
  head(10)
  # apply(x = hashdf$hash, MARGIN = 1, FUN = function(x) filter(tweetstb, grepl(x, Hashtags))) %>%
  # filter(grepl(hashdf[1,1], Hashtags)) %>%
  # summarise(x1 = sum(Retweets))



tweets %>%
  select(Hashtags, Retweets) %>%
  # filter(Hashtags != '') %>%
  # filter(grepl('MAGA', Hashtags)) %>%
  arrange(-Retweets) %>%
  filter(Retweets < 100000) %>%
  # head(10)
  summarise(withMAGA = sum(grepl('MAGA', Hashtags)),
            onlyMAGA = sum(grepl('^MAGA$', Hashtags)),
            noMAGA = sum(!grepl('MAGA', Hashtags)),
            RTwithMAGA = sum(grep('MAGA', Hashtags)),
            RTonlyMAGA = sum(grep('^MAGA$', Hashtags)),
            RTnoMAGA = sum(grep('MAGA', Hashtags, invert = T)),
            perWithMAGA = RTwithMAGA / withMAGA,
            perOnlyMAGA = RTonlyMAGA / onlyMAGA,
            perNoMAGA = RTnoMAGA / noMAGA)

##### OBSERVATIONS #####

# There are a few tweets, I believe right after Trump was elected, that there's an order of magnitude 
# more retweets/favorites. In coming analyses, I might want to exclude this points so they don't skew the 
# analysis and presentation of the data.

# It also looks like sometimes Trump retweeted people who used an awful amount of exclamation points,
# and that showed up as one of his tweets.

# It's confirmed that Favorites and Retweets can be proxies for each other. They're very closely correlated 
# (Adj R^2 is 0.8815)


##### TODO #####
# look at increase of effectiveness of tweets per time
# look at hourly rate of tweets with exlcamation points
# collect tweets with rtweet and do some simple analysis following up this stuff.

