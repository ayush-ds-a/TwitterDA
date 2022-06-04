library(tidyr) 
library(twitteR)
library(tidyverse)
library(tidytext)
appname <- "ABC "
key <- "your key"
secret <- "your key"
access<-"your access code"
access_secret="your access secret code"


setup_twitter_oauth(key, secret, access, access_secret)
#This function will issue a search of Twitter based on a supplied search string.
#covid means string to be searched
#n means number of tweets to return
#lang means language
a <- searchTwitter("#COVID",n=100,lang = "en")
a
a_df <- twListToDF(a) # Convert to data frame
write.csv(a_df,file="COVID2.csv")
read.csv("COVID2.csv")

# Q1)
a_df=read.csv("COVID2.csv")
head(a_df$text,n=1)


#Q2)
install.packages("syuzhet")
library(syuzhet) #package for sentiment analysis
mysentiment_tech<-get_nrc_sentiment((a_df$text))
Sentimentscores_tech<-data.frame(colSums(mysentiment_tech[,])) #to get frequency of words used for the
#emotions
names(Sentimentscores_tech)<-"Score"
Sentimentscores_tech<-cbind("sentiment"=rownames(Sentimentscores_tech),Sentimentscores_tech)
rownames(Sentimentscores_tech)<-NULL
view(Sentimentscores_tech)

#***********************************************************************************

#stat
ggplot(data=Sentimentscores_tech,aes(x=sentiment,y=Score))
    geom_bar(aes(fill=sentiment),stat = "identity")+coord_flip()+
    theme(legend.position="none")+
    xlab("Sentiments")+ylab("scores")+ggtitle("COVID RESPONSE ANALYSIS")

#Q3
library(tm)

corpus = iconv(a_df$text, "latin1", "UTF-8")
corpus<- Corpus(VectorSource(corpus))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs=corpus
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
corpus=docs
corpus<- tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)# remove puntuations like , .
corpus<- tm_map(corpus,removeNumbers)
cleanset<-tm_map(corpus,removeWords,stopwords('english'))# remove common words
removeURL<- function(x)gsub('http[[:alnum:]]=','',x)
cleanset<-tm_map(cleanset,content_transformer(removeURL))
x=cleanset


tdm<-TermDocumentMatrix(cleanset)
tdm # display information
tdm<-as.matrix(tdm)
v=sort(rowSums(tdm))

library(wordcloud)
w<-data.frame(names(v),v)
colnames(w)<-c('word','freq')
set.seed(1234)
wordcloud(words=w$word,freq=w$freq)
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(w, size=10,color = "random-light", backgroundColor = "grey")
wordcloud2(w,size=5,shape = 'circle')
