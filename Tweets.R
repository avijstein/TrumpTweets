##### SET UP #####
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

clean_plot =  theme(axis.line = element_line(colour = "blue"), panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(), panel.border = element_blank(),
                    panel.background = element_blank(), legend.key = element_blank())


tweets = read.csv('TrumpTweets.csv', stringsAsFactors = F)
names(tweets)[9] = 'Favorites'

##### HOURLY TWEETS EXPERIMENTING #####
# setting the data into the time intervals I want them in.
newtweets = tweets[,c(1:3,9,10)]
newtweets$Time = strptime(newtweets$Time, format = '%H:%M:%S')
newtweets$Date = strptime(newtweets$Date, format = '%y-%m-%d')
newtweets$hour_cat = cut(newtweets$Time, breaks = 'hours', labels = F)
newtweets$month_cat = cut(newtweets$Date, breaks = 'months', labels = F)

# summary table of tweet data.
newtweets_data = newtweets %>%
  select(-Time, -Date) %>%
  group_by(hour_cat) %>%
  summarise('count' = n(),
            'total_rt' = sum(Retweets)/1000000,
            'total_fav' = sum(Favorites)/1000000,
            'tweet_effect' = sum(Retweets/count))

# plotting tweets per hour.
ggplot(data = newtweets_data) +
  geom_rect(aes(xmin = 0.5, xmax = 7, ymin=0, ymax=Inf), fill = 'grey') +
  geom_rect(aes(xmin = 19, xmax = 24.5, ymin=0, ymax=Inf), fill = 'grey') +
  geom_line(aes(x = hour_cat, y = count), color = 'slateblue1', size = 2) +
  labs(x = 'Hours of the Day', y = 'Number of Tweets', title = 'Hourly Preference of Tweets') +
  clean_plot

# plotting effectiveness of tweets hourly.
ggplot(data = newtweets_data) +
  geom_rect(aes(xmin = 0.5, xmax = 7, ymin=0, ymax=Inf), fill = 'grey') +
  geom_rect(aes(xmin = 19, xmax = 24.5, ymin=0, ymax=Inf), fill = 'grey') +
  geom_line(aes(x = hour_cat, y = tweet_effect), color = 'slateblue1', size = 2) +
  labs(x = 'Hour of the Day', y = 'Retweets per Tweet', title = 'Effectiveness of Tweets') +
  clean_plot

clout = newtweets %>%
  select(-Time, -Date) %>%
  group_by(month_cat) %>%
  summarize('count' = n(),
            'total_rt' = sum(Retweets)/1000000,
            'total_fav' = sum(Favorites)/1000000,
            'tweet_effect' = sum(Retweets/count)/1000)

# retweetiness over time.
ggplot(data = clout) +
  geom_line(aes(x = month_cat, y = tweet_effect), size = 2, color = 'slateblue1') +
  labs(x='Months Since Campaign Start', y = 'Retweets per Tweet (thousands)', title = 'Retweetiness over Time') +
  clean_plot


##### MONTHLY TWEET EXPERIMENTING #####
# categorizing month and hour data.
boxtable = tweets %>%
  filter(Retweets < 100000) %>%
  select(Date, Time, Favorites, Retweets) %>%
  mutate(newDate = as.Date(Date, format = '%y-%m-%d')) %>%
  mutate(dateCut = cut(newDate, breaks = 'month', labels = F))
boxtable$newTime = strptime(boxtable$Time, format = '%H:%M:%S')
boxtable$timeCut = cut(boxtable$newTime, breaks = 'hours', labels = F)  

# creating frequencies of monthly/hourly tweets/retweets.
boxtable = boxtable %>% 
  select(-newTime, -newDate) %>%
  mutate(joinpoint = paste(dateCut, timeCut)) %>%
  group_by(dateCut, timeCut, joinpoint) %>%
  summarise('count' = n(),
            'retweets' = mean(Retweets))

# filling in missing values.
filler = data.frame(fake_month = sort(rep(seq(1:17), 24)),
                    fake_hour = rep(seq(1:24), 17),
                    joinpoint = paste(filler$fake_month, filler$fake_hour))

boxtable = boxtable %>%
  right_join(filler, by = 'joinpoint')


# tweets through time
ggplot(data = boxtable, aes(x = fake_month, y = fake_hour)) +
  geom_tile(aes(fill = rescale(count))) +
  scale_fill_gradient(name = 'Tweet Density', low = 'blue', high = 'orange') +
  labs(x='Months Since Campaign Start', y = 'Hours of the Day', title = 'Tweets through Time') +
  clean_plot

# retweets through time
ggplot(data = boxtable, aes(x = fake_month, y = fake_hour)) +
  geom_tile(aes(fill = rescale(retweets))) +
  scale_fill_gradient(name = 'Retweet Density', low = 'blue', high = 'orange') +
  labs(x='Months Since Campaign Start', y = 'Hours of the Day', title = 'Retweets through Time') +
  clean_plot


##### EXCLAMATION POINTS #####

# grepping for exclamation points, unlisting while hunting for null values and subset values proved difficult. 
exclaim = (gregexpr(pattern = "!", text = tweets$Tweet_Text))
numexclaim = c()
for (i in 1:length(exclaim)){
  if (sum(exclaim[[i]]) == -1){
    numexclaim[i] = 0
  }else{
    numexclaim[i] = length(exclaim[[i]])
  }
}

# putting them into a dataframe.
exclaimtweets = data.frame('text' = tweets$Tweet_Text, 'count' = numexclaim, 'time' = as.character(tweets$Time),
                           'retweets' = tweets$Retweets, stringsAsFactors = F)

# formatting date and time, then creating a summary table.
exclaimtweets$time = strptime(exclaimtweets$time, format = '%H:%M:%S')
exclaimtweets$hour_cat = cut(exclaimtweets$time, breaks = 'hours', labels = F)
extb = exclaimtweets %>% 
  select(-time) %>%
  group_by(hour_cat) %>%
  summarise('class' = 'exclaim',
            'count' = sum(count))

# combining with the table for all tweets.
extb = rbind(extb, data.frame('hour_cat' = newtweets_data$hour_cat,
                        'class' = rep('All Tweets', nrow(newtweets_data)), stringsAsFactors = F,
                        'count' = newtweets_data$count))

# plotting exclamation points hourly preferences.
ggplot(data = extb) +
  geom_line(aes(x = hour_cat, y = count, color = factor(class)), size = 2) +
  labs(x = 'Hours of the Day', y = 'Normalized Count', title = 'Hourly Exclamations!') +
  scale_color_manual(name = 'Groups', values = c('slateblue1', 'orange'), labels=c('All Tweets','Exclamations Points'))+
  clean_plot


##### HASHTAGS ##### 

# duplicating so I don't destroy it.
tweetstb = tweets

# finding most popular hashtags from the hashtag list.
hash = paste(tweetstb$Hashtags, collapse = ';')
hash = unlist(strsplit(hash, ';+'))
hashdf = tbl_df(table(hash)) %>% arrange(-n) %>% filter(n > 50)


# fetching values for the top hashtags.
tophash = data.frame()
for (i in 1:nrow(hashdf)){
  print(hashdf$hash[i])
  x = tweetstb %>%
    filter(grepl(pattern = hashdf$hash[i], Hashtags))
  x$Time = strptime(x$Time, format = '%H:%M:%S')
  x$hour_cat = cut(x$Time, breaks = 'hours', labels = F)
  hash1 = tbl_df(table(x$hour_cat)) %>% transmute('hashtag' = hashdf$hash[i],'hour_cat' = as.numeric(Var1), 'count' = n)
  tophash = rbind(tophash, hash1)
}

# taking only the columns we want and joining them with the table for all tweets. 
tophash = tophash %>% filter(hashtag %in% c('Trump2016','MakeAmericaGreatAgain'))
tophash = rbind(tophash, 
          data.frame('hashtag' = rep('All Tweets', nrow(newtweets_data)), stringsAsFactors = F,
                     'hour_cat' = newtweets_data$hour_cat,
                     'count' = newtweets_data$count/10))

# Hourly hashtags preference.
ggplot(data = tophash) +
  geom_line(aes(x = hour_cat, y = count, color = factor(hashtag)), size = 2) +
  labs(x = 'Hours of the Day', y = 'Count', title = 'Hourly Hashtag Preference') +
  scale_color_discrete(name = 'Hashtags', labels = c('(1/10 of All Hashtags)', '#MakeAmericaGreatAgain', '#Trump2016'))+
  clean_plot

# finding the midpoint of when each of the popular tweets took off, to normalize against retweetiness.
middate = tweetstb %>%
  filter(grepl(pattern = hashdf$hash[i], Hashtags)) %>%
  select(Hashtags, Retweets, Date) %>%
  mutate(newdate = as.Date(Date, format = '%y-%m-%d'))
middate = middate %>%
  summarise('meantime' = mean(middate$newdate))


# calculating the average retweets on tweets with certain hashtags.
eff_hash = data.frame()
for (i in 1:nrow(hashdf)){
  print(hashdf$hash[i])
  middate = tweetstb %>%
    filter(grepl(pattern = hashdf$hash[i], Hashtags)) %>%
    select(Hashtags, Retweets, Date) %>%
    mutate(newdate = as.Date(Date, format = '%y-%m-%d'))
  out = middate %>%
    summarise('hashtag' = paste0('#', hashdf$hash[i]),
              'count' = n(),
              'avg_rt' = mean(Retweets),
              'meantime' = mean(middate$newdate))
  eff_hash = rbind(eff_hash, out)
  middate = tweetstb %>%
    filter(!grepl(pattern = hashdf$hash[i], Hashtags)) %>%
    select(Hashtags, Retweets, Date) %>%
    mutate(newdate = as.Date(Date, format = '%y-%m-%d'))
  out = middate %>%
    summarise('hashtag' = paste0('#', hashdf$hash[i]),
              'count' = n(),
              'avg_rt' = mean(Retweets),
              'meantime' = mean(middate$newdate))
  eff_hash = rbind(eff_hash, out)
}

# normalizing for increased retweetibility over time.
eff_hash$presence = seq(1,0)
eff_hash$midpoint = round(as.numeric(difftime(eff_hash$meantime, min(as.Date(newtweets$Date)), units = 'weeks')/4))
eff_hash = merge(eff_hash, clout[,c(1,5)], by.x = 'midpoint', by.y = 'month_cat', all.x = T)
eff_hash$norm = eff_hash$avg_rt / eff_hash$tweet_effect

# Impact of Hashtag on Retweet Potential / Normalization
ggplot(data = eff_hash) +
  geom_point(aes(x = hashtag, y = avg_rt, color = factor(presence)), size = 3) +
  labs(x='Hashtag', y = 'Average Retweet', title = 'Hashtag Impact on Retweet Potential') +
  ylim(0,12000) +
  scale_color_manual(name = 'Present', labels = c('No', 'Yes'), values = c('slateblue1', 'orange')) +
  clean_plot

ggplot(data = eff_hash) +
  geom_point(aes(x = hashtag, y = norm, color = factor(presence)), size = 4) +
  labs(x='Hashtag', y = 'Normalized Average Retweet', title = 'Normalized Hashtag Impact on Retweet Potential') +
  ylim(0,12000) +
  scale_color_manual(name = 'Present', labels = c('No', 'Yes'), values = c('slateblue1', 'orange')) +
  clean_plot




##### OBSERVATIONS #####

# There are a few tweets, I believe right after Trump was elected, that there's an order of magnitude 
# more retweets/favorites. In coming analyses, I might want to exclude this points so they don't skew the 
# analysis and presentation of the data.

# It also looks like sometimes Trump retweeted people who used an awful amount of exclamation points,
# and that showed up as one of his tweets.

# It's confirmed that Favorites and Retweets can be proxies for each other. They're very closely correlated 
# (Adj R^2 is 0.8815)

# Separating his tweets from him weirdly quoting tweets and him retweeting people is more difficult than I thought.
# So, I'm using the assumption that when he retweets something that uses #MAGA or "I love Trump!!!!" it's because 
# he's willing to highlight this tweet, even if he's not writing it himself.

# With this in mind, he tweets or retweets exclamation marks with no real hourly preference that's different than the 
# normal flow of tweets.

# There is also no significant trend for his most popular hashtags (and the less popular ones don't have enough
# to show trends yet).

# Effects of using top hashtags on retweets: could be influenced by time in campaign it was tweeted. Use retweetiness
# to normalize it. In fact, it is. There may be other factors at play here, but the tweetiness is the biggest player
# for sure.


##### TODO #####
# collect tweets with rtweet and do some simple analysis following up this stuff.

