#BIMA CAPSTONE PROJECT
#Michael Deneff, Levi Wolf, David Shrem, Zev Jarashow

##########################################################
#IN ORDER TO RUN THE PROGRAM, ALL PACKAGES MUST BE RUNNING 
#AND BOTH APIS HAVE TO BE AUTHORIZED.
##########################################################

#all packages needed
install.packages("rtweet")
install.packages("reactable")
install.packages("glue")
install.packages("httpuv")
install.packages("dplyr")
install.packages('twitteR')
install.packages('tidytext')
install.packages('sentimentr')
install.packages('scales')
install.packages('stringr')

#libraries needed 
library(rtweet)
library(dplyr)
library(httpuv)
library(reactable)
library(twitteR)
library(tidytext)
library(lubridate)
library(sentimentr)
library(scales)
library(stringr)


#setting up API rtweet
appname= "stocks_tweets_yu_capstone"
consumer_key= "bmYMV1bl2GYbVnVjOC3Pe01L1"
consumer_secret= "5LjGVcbOXDwPlZUas253dDelJq9qLOBUYB3OOGoFb5EZtz65VW"
access_token="3307763386-By6FQ2LI7PeGLRKuPnO3mFC1SI2XzuxtkRv2IfT"
access_secret= "9hROXRkbJ528pvGmLyvj2OL4kTSqqxpEmRcO45BuMho9S"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

#setting up twitteR
setup_twitter_oauth("bmYMV1bl2GYbVnVjOC3Pe01L1","5LjGVcbOXDwPlZUas253dDelJq9qLOBUYB3OOGoFb5EZtz65VW", 
                    "3307763386-By6FQ2LI7PeGLRKuPnO3mFC1SI2XzuxtkRv2IfT","9hROXRkbJ528pvGmLyvj2OL4kTSqqxpEmRcO45BuMho9S")

###############################################
#TO RUN THE TWITTER PROGRAM, RUN LINE 55 BELOW. 
###############################################
results=twitter()

########################################################################################
#ALL CODE BELOW
########################################################################################
twitter=function(){
cat("Welcome to Twitter VS the Stock Market!\nEnter S to start or Q to quit.")  
userA= readline()
while(toupper(userA)!= "Q"){
print("Which company do you want to search for? Enter its trading abbreviation. Ex: $AAPL")
company= readline()
toupper(company)
print("How many tweets would you like to collect? (Up to 800 at one time)")
n=readline()
n=as.numeric(n)
print("How would you like your search results filtered? (recent, popular, or mixed)")
results= readline()
tolower(results)

#Getting a data set of tweets by searching twitter for what is in parameter #1
tweets <- search_tweets(company ,n, FALSE, type = results)
                       #,retryonratelimit =TRUE, max_id=TRUE)

#keeping only Relevant columns
tweets <- tweets[,c(1,3,4,5,12,13,14,17,32)]

#returns tweets with added necessary columns and values
tweets= metrics(tweets)
#View(tweets)

#returns our tweet data grouped by individual date, along with other added cols
groomed_tweets= final(tweets)

result <- list(tweets,groomed_tweets)

print(result)
cat("To access results, refer to variable- (result) in the console. \n
      You can call: View(reults) to view the list of two data frames, \n
                    View(results[[1]] to view the tweets collected and created metrics, \n
                    or View(results[[2]] to view the summarized data grouped by day. \n
                    ------------------------------------------------------------------ \n
                    Want to run another company? Click S \n
                    Want to quit? Click Q")
userA=readline()
      }
print("Thanks for coming!")
}

#writing our data set into csv in order to merge it with FD
write.csv(official_data_set,"C:/Users/Mdene/OneDrive/Documents/$TESLA.csv", row.names=FALSE)

#function which provides value based on profiles follower count
followerfunction = function(followersCount, metric=0) {
  
  if (followersCount>=50000){
    metric= metric+5
  }
  else if (followersCount >=10000) {
    metric = metric + 4
  }
  else if (followersCount >=5000) {
    metric = metric + 3
  }
  else if (followersCount >=1000) {
    metric = metric + 2
  }
  else if (followersCount >=500) {
    metric = metric + 1
  }
  else {
    metric=metric
  }
  return(metric)
}

#function which provides value based on if a profile is verified or not
verifiedfunction= function (verified, metric=0){
  if(verified==TRUE){
    metric=3
  }
  else{
    metric= metric
  }
  return(metric)
}

#function which provides value based on profiles list count
listfunction= function (listedCount, metric=0){
  if(listedCount>=500){
    metric=2
  }
  else if (listedCount>=50){
    metric=1
  }
  else{
    metric=metric
  }
  return(metric)
}

#Function which calls the 3 previous functions and gives each tweet its user metric,
#which shows the value of each profile
Userweights <- function( Usernames ){ 
  
  count=1
  totalmetric = 0
  newRow=0
  
  while(count<=length(Usernames)){
    
    follower <- getUser(Usernames[count])
    #follower <- try(getUser(Usernames[count]))
    #Sys.sleep(1)
    verified <- follower$verified 
    listedCount <- follower$listedCount 
    followersCount <- follower$followersCount 
    
    value=followerfunction(followersCount)
    totalmetric= totalmetric+value
    value=0
    
    value=verifiedfunction(verified)
    totalmetric= totalmetric+value 
    value=0
    
    value=listfunction(listedCount)
    totalmetric= totalmetric+value 
    
    newRow=c(newRow, totalmetric)
    count=count+1
    
    value=0
    totalmetric=0
    
  }
  
  return(newRow[1:length(Usernames)+1])
  
}

#function which creates a value for each tweet based on its favorites
favorite_function= function(favoritesCount,place, metric=0){
  if(tweets$favorite_count[place]>5000){
    metric=metric+5
  }
  else if(tweets$favorite_count[place]>1250){
    metric=metric+4
  }
  else if(tweets$favorite_count[place]>750){
    metric=metric+3
  }
  else if(tweets$favorite_count[place]>250){
    metric=metric+2
  }
  else if(tweets$favorite_count[place]>50){
    metric=metric+1
  }
  else {
    metric=metric
  }
  return(metric)
}

#function which creates a value for each tweet based on its retweets
retweet_function= function(retweetCount, place, metric=0){
  if(tweets$retweet_count[place]>5000){
    metric=metric+5
  }
  else if(tweets$retweet_count[place]>1000){
    metric=metric+4
  }
  else if(tweets$retweet_count[place]>500){
    metric=metric+3
  }
  else if(tweets$retweet_count[place]>100){
    metric=metric+2
  }
  else if(tweets$retweet_count[place]>25){
    metric=metric+1
  }
  else {
    metric=metric
  }
  return(metric)
}

#function which calls the previous 2 functions and gives each tweet a tweet metric,
#showing the value of each tweet. 
tweet_weight= function(favorites, retweets){
  
  count=1
  totalmetric = 0
  newRow=0
  
  while(count<=length(favorites)){
  
    value=favorite_function(favorites, count)
    totalmetric= totalmetric+value
    value=0
    
    value=retweet_function(retweets, count)
    totalmetric= totalmetric+value
    value=0 
    
    newRow=c(newRow, totalmetric)
    count=count+1
    
    value=0
    totalmetric=0
    
  }
  
  return(newRow[1:length(favorites)+1])
  
}

#now that we have a user metric, and tweet metric, we will use them to decide which tweets
#we want to keep based on our if statements below. 
filter= function(new_df){
  count=1
  tweets= new_df$user_metric <2
  user= new_df$tweet_metric <1
  rows=0
  while(count<=length(tweets)){
    if(tweets[count]==FALSE || user[count]==FALSE){
      rows=c(rows, count)
      count=count+1
    }
    else{count=count+1}
    
  }
  new_df= new_df[rows,]
  return(new_df)
}

#this function combines all previous functions. It takes a data set of tweets and
#relevant info, creates a user metric col, a tweet metric col, a sentiment score col,
#and extracts the exact day from the "created_at" value, then assigns associated values. 
#Lastly, it removes all rows which do not make it through our filter function. 
metrics= function(data_set){
  
  usernames= data_set$screen_name
  user_metric= Userweights(usernames)
  
  tweet_metric= tweet_weight(data_set$favorite_count, data_set$retweet_count)
  
  sentiment= sentiment_by(data_set$text)
  
  data_set= cbind(data_set, 
                  user_metric=user_metric, 
                  tweet_metric=tweet_metric, 
                  sentiment_score= sentiment[,4],
                  exact_date= day(as.Date(substr(data_set$created_at, 1, 11))))
  
  return(filter(data_set))
}

#in order to merge our tweet data with out financial data, we need to turn all our twitter
#data into single rows grouped by days, since that is the form of the financial data.
#So, this function groups by the day the tweet was published, and provides
#the avg tweet_metric, user_metric, _sentiment score, as well as how many tweets there
#were for each day. 
summarise_rows=function(data_set){
                   new= data_set%>%
                   group_by(exact_date)%>%
                   summarize(avg_tweet_metric= mean(tweet_metric),
                             avg_user_metric= mean(user_metric), 
                             avg_sentiment= mean(ave_sentiment))
                   days= data_set%>%
                     group_by(exact_date)%>%
                     count(exact_date)
                   new= cbind(new, amt_per_day= days$n)
                   return(new)
}

#the final function utilizes the previous one to give us a percent col, which 
#tells us how valuable each day was, in terms of tweet per that day. 
#we also needed the full date year-mm-dd in this format, so that was worked through 
#in order to obtain that info. 
final= function(data_set){
  data_set= summarise_rows(data_set)
  count=1
  dates=numeric(0)
  percent=numeric(0)
  todays_date= substring(Sys.Date(),1,8)
  trimws(data_set$exact_date, which = "left")
  while(count<=length(data_set$exact_date)){
    percent=c(percent, percent(data_set$amt_per_day[count]/sum(data_set$amt_per_day),.01))
    dates=c(dates, paste(todays_date, data_set$exact_date[count]))
    dates[count]=str_replace(dates[count]," ","")
    count=count+1
  }
  data_set= cbind(data_set, 
                  percent_of_pop= percent,
                  real_date= dates)
  return(data_set)
}




##########################################################################################
#reactable(tweets, filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE, sortable = TRUE, 
#          defaultPageSize = 25, defaultSortOrder = "desc")
##########################################################################################