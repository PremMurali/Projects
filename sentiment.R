var1="D:/R_code/Sentimentoutput.csv"

#var2="D:/capstone/dataemail.csv"
args<-commandArgs(trailingOnly=T)
#var2<-as..numeric(args[1])

var2<-as.character(args[1])

tweets=read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)
tweets$date=as.Date(tweets$date,format = "%d-%m-%Y")
count(tweets$date)
#install.packages("twitteR")
#install.packages("RCurl")
#install.packages("httr")
library(twitteR)
library(RCurl)
library(httr)
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("RcolorBrewer")
library(tm)
library(wordcloud)
library(syuzhet)

tweets <- read.csv(var2)
View(tweets)

tweets.df = tweets
View(tweets.df)



## CLEANING TWEETS
head(tweets.df$Body)
tweets.df$Body=gsub("&amp", "", tweets.df$Body)
tweets.df$Body = gsub("&amp", "", tweets.df$Body)
tweets.df$Body = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$Body)
tweets.df$Body = gsub("@\\w+", "", tweets.df$Body)
tweets.df$Body = gsub("[[:punct:]]", "", tweets.df$Body)
tweets.df$Body = gsub("[[:digit:]]", "", tweets.df$Body)
tweets.df$Body = gsub("http\\w+", "", tweets.df$Body)
tweets.df$Body = gsub("[ \t]{2,}", "", tweets.df$Body)
tweets.df$Body = gsub("^\\s+|\\s+$", "", tweets.df$Body)
tweets.df$Body = gsub("\n","",tweets.df$Body)

tweets.df$Body <- iconv(tweets.df$Body, "UTF-8", "ASCII", sub="")

sent.value <- get_sentiment(tweets.df$Body)


nrc_sent.value = get_nrc_sentiment(corpusABG$content)
View(nrc_sent.value)
View(sent.value)
tweets.df["Sentimentvalue"] <- sent.value
View(tweets.df)

simple_plot(sent.value)

corpusABG = Corpus(VectorSource(tweets.df$Body))
corpusABG = tm_map(corpusABG, tolower)

tweets.df$Body[1232]
corpusABG1 = tm_map(corpusABG, removePunctuation)
wordcloud(corpusABG1,colors=rainbow(7),max.words=50)
corpusABG = tm_map(corpusABG, removeWords, c(stopwords("english"),"hi","hello","thanks","mam","sir","madam","regards","regs","Hi"))
corpusABG=tm_map(corpusABG,removeWords,c("siri","mani","nirmala","please","mamsir","mamsirmy","regs"))
wordcloud(corpusABG,colors=rainbow(7),max.words=35,fixed.asp = TRUE)
?wordcloud
corpusABG = tm_map(corpusABG, stemDocument)

frequenciesABG = DocumentTermMatrix(corpusABG)

sparseABG = removeSparseTerms(frequenciesABG, 0.995)

ABGSparse = as.data.frame(as.matrix(sparseABG))

colnames(ABGSparse) = make.names(colnames(ABGSparse))

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0.5, "Positive", "Neutral"))
ABGSparse$Polarity = category_senti
ABGSparse$Polarity
install.packages("qdap")
library(qdap)
polarity(ABGSparse)
?removeSparseTerms
tweets.df["Sentiment_category"] <- category_senti

table(ABGSparse$Polarity)

var1="D:/R_code/Sentimentoutput.csv"
write.csv(tweets.df, file = var1, row.names = FALSE)

table(ABGSparse$Polarity)

library(dplyr)
twee=inner_join(ABGSparse,bing,by=c("term"="word"))
polarity(tweets$Body)[1224]
nrc_sent.value[1224]
