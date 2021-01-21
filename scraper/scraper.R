library(tm)
library(wordcloud2)
library(twitteR)
library(rtweet)
library(dplyr)
library(ggplot2)

# Mengambil data dari twitter
consumer_key <- "gbR3irq6ngyKd6h0CQroSpKox"
consumer_secret <- "OsmBAxv7XDMh1Z1LjNAiXrsgfzjCgkJG2IGzY1fxuIgru0jrOT"
access_token <- "1029748538748559360-GSF1YhKHsglz5ai1epYriuKaEc24Sm"
access_secret <- "nPxSkCS26dp8ft5GCG1OpqxowQR5ZjrMZXVmUYljOIuV0"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

trump_tweets = searchTwitter("convict + trump", n=1000, lang="en")

#Data cleaning
trumpTweets <- sapply(trump_tweets, function(x) x$getText())

catch.error = function(x)
{
  #buat missing value untuk tujuan tes
  y = NA
  #test untuk mengecek error (NA) yang telah kita dibuat
  catch_error = tryCatch(tolower(x), error=function(e) e)
  #if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  #check result if error exists, otherwise the function works fine
  return(y)
}

cleanTweets <- function(tweet) {
  #bersihkan tweet untuk sentiment analysis
  #remove html links:
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  #remove retweet entities:
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  #remove #hashtags:
  tweet = gsub("#\\w+", " ", tweet)
  #remove all "@people":
  tweet = gsub("@\\w+", " ", tweet)
  #remove all punctuations:
  tweet = gsub("[[:punct:]]", " ", tweet)
  #remove numbers, kita hanya butuh teks untuk analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  #remove unnecessary spaces (white spaces, tabs, etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet = gsub(",", " ", tweet)
  #jika ada lagi yang dirasa ingin dihilangkan, bisa. Contohnya, slang words/bahasa gaul dapat dihilangkan dengan cara serupa di atas.
  #ubah semua kata menjadi lowercase:
  tweet = catch.error(tweet)
  tweet
}

cleanTweetsAndRemoveNAs <- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  #remove "NA" tweets:
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  #remove repetitive tweets:
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

trump_tweetsCleaned = cleanTweetsAndRemoveNAs(trumpTweets)

#cek jumlah tweets setelah pembersihan:
length(trump_tweetsCleaned)

#Ambil data kata-kata positif dan negatif
opinion.lexicon.pos = scan("~/Sentiment_Analysis/scoring/s-pos.txt", what = "character", comment.char = ";")
opinion.lexicon.neg = scan("~/Sentiment_Analysis/scoring/s-neg.txt", what = "character", comment.char = ";")

#Custom positive and negative words
pos.words = c(opinion.lexicon.pos, "upgrade", "stay")
neg.words = c(opinion.lexicon.neg, "convict","convicttrum","jail","trum","donaldtrum","convicttrump","convicttrumis","idiotconvicttrump","convicttrumexltheseditionists"," convicttrumdomesticterrorism","convicttrumexltheseditionists","convicttrumcovid","convicttrumconvictanddisqualifytrump"," convicttrumpresidenttrumrdongate")

#membuat fungsi score.sentiment(), yang bisa menghitung hasil sentimen mentah berdasarkan algoritma pencocokan sederhana:
getSentimentScore = function(sentences, pos.words, neg.words, .progress = "none")
{
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    #remove digit, punctuation, dan special/control character:
    sentence = gsub("[[:cntrl:]]", "", gsub("[[:punct:]]", "", gsub("\\d+", "", sentence)))
    
    #convert semua teks menjadi lowercase:
    sentence = tolower(sentence)
    
    #pisahkan setiap kalimat menggunakan spasi (space delimiter):
    words = unlist(str_split(sentence, "\\s+"))
    
    #lakukan boolean match dari setiap kata-kata menggunakan pos &amp;amp;amp; neg opinion-lexicon:
    pos.matches = !is.na(match(words, pos.words))
    neg.matches = !is.na(match(words, neg.words))
    
    #score sentimen = total positive sentiment - total negative:
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  #return data frame berisi kalimat beserta sentimennya:
  return(data.frame(text = sentences, score = scores))
}

#terapkan ke data tweet yang telah kita bersihkan:
trumpResult = getSentimentScore(trump_tweetsCleaned, pos.words, neg.words)

#CONVERT SCORE TO SENTIMENT
trumpResult$klasifikasi<- ifelse(trumpResult$score<0, "Negatif",ifelse(trumpResult$score==0, "Netral", "Positif"))
trumpResult$klasifikasi
View(trumpResult)

#export to csv:
write.csv(trumpResult, file = "~/Sentiment_Analysis/dataset/tweets.csv")

