################################
# Introduction to R            #           
# Final Project                #                    
# Authors:                     #
# Melissa Hudson,              # 
# Janae Logan, and             #
# Mikia Thomas                 #
# Date: 7/18/2019              #
################################

#Import Dataset
LungCancer <- read.csv("CANCERDATA.csv")

#Descriptive Statistics
dim(LungCancer)
str(LungCancer)
summary(LungCancer)

#Rename Rural_Suburban_Urban variable and values
names(LungCancer)[names(LungCancer)=="Rural_Suburban_Urban"] <- "Region"
#Rename
LungCancer$Region1[LungCancer$Region=="R"] <- 1
LungCancer$Region1[LungCancer$Region=="S"] <- 2
LungCancer$Region1[LungCancer$Region=="U"] <- 3

#Rename Stage Values
LungCancer$Stage1[LungCancer$Stage == "I"] <- 1
LungCancer$Stage1[LungCancer$Stage == "II"] <- 2
LungCancer$Stage1[LungCancer$Stage == "III"] <- 3
LungCancer$Stage1[LungCancer$Stage == "IV"] <- 4
LungCancer$Stage1[LungCancer$Stage == "OCCULT"] <- 5
LungCancer$Stage1[LungCancer$Stage == "UNK"] <- 6
LungCancer$Stage1[LungCancer$Stage == "NA"] <- 99

#Correlation Test
cor(LungCancer[,c(19,1,3,5,7,13,14,16,19,20)], method="pearson", use="complete")
pairs(~Stage1+Zip+Histo+Age+Race+Sex+TobaccoHistory+FamHX+Region1, data=LungCancer, main="Simple Scatterplot Matrix")

#Find if each quantitative variable is statistically significant to Stage
cor.test(LungCancer$Stage1, LungCancer$Zip, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Histo, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Age, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Race, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Sex, use="complete")
cor.test(LungCancer$Stage1, LungCancer$TobaccoHistory, use="complete")
cor.test(LungCancer$Stage1, LungCancer$FamHX, use="complete")
cor.test(LungCancer$Stage1, LungCancer$Region1, use="complete")

#1. Does the different age groups affect the stage of cancer?
#Using ANOVA for since Age.Group variable has 8 levels
lc.aov <- aov(LungCancer$Stage1 ~ LungCancer$Age.Group)
anova(lc.aov)

TukeyHSD(lc.aov)
colors<-c("Yellow", "Blue", "Green", "Red", "Orange", "Grey", "Violet")
boxplot(LungCancer$Stage1~LungCancer$Age.Group, col=colors, main = "Figure 1: Plot of Lung Cancer Stage and Age Group", xlab = "Age Group", ylab="Lung Cancer Stage")

#2. How does the family history of an individual affect the stages of cancer?
#Using ANOVA for more than 2 levels
lc1.aov <- aov(LungCancer$Stage1~LungCancer$FamHx_Desc)
anova(lc1.aov)

TukeyHSD(lc1.aov)
colors<-c("Yellow", "Red", "Orange", "Blue", "Grey", "Violet", "Green")
boxplot(LungCancer$Stage1~LungCancer$FamHx_Desc, col=colors, main = "Figure 2: Plot of Lung Cancer Stage and Family History", xlab = "Family History", ylab="Lung Cancer Stage")

#3. How does tobacco history affect various lung cancer stages?
#Using ANOVA 
lc2.aov <- aov(LungCancer$Stage1~LungCancer$Tobacco_Desc)
anova(lc2.aov)

TukeyHSD(lc2.aov)
colors<-c("Yellow", "Red", "Orange", "Blue", "Grey", "Violet", "Green")
boxplot(LungCancer$Stage1~LungCancer$Tobacco_Desc, col=colors, main = "Figure 3: Plot of Lung Cancer Stage and Tobacco Description", xlab = "Tobacco Description", ylab="Lung Cancer Stage")

#TwitteR Analysis
#==========================
# Part 1. Twitter Scraping 
#==========================

#### Install Packages
# check if the package is installed; if not, install it.
if (!require(devtools)) {install.packages("devtools")} 

# check if the package is installed; if not, install it.
if (!require(twitteR)) {install.packages("twitteR")} 

#### Load Packages
library(httr)
library(devtools)
library(twitteR)

#### Collect Tweets
# Get Tweet API credentials following 
# https://www.slickremix.com/docs/how-to-get-api-keys-and-tokens-for-twitter/
consumer_key <- "puxmqW5Fb9zV3wddFzwGYmcCj"
consumer_secret <- "rtP4AYjMxvaKIzIl2a9JNBP26cnBHYJQb7H35cpcHuQC0tEzVY"
access_token <- "2607567668-XhrG0Ywtzh13FgcSgzobhAIqz1KrG0w4xJNxRQ3"
access_secret <- "xOhkVZQrIwfJnxR865B51rgNq1tqLb7WDVRRGmoIiMFbp"

# Set Tweet API credentials 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Collect Tweets
tw1 <- searchTwitter("lung cancer", n=100, lang='en', since = "2019-07-27", until = "2019-07-28")
tw2 <- searchTwitter("lung cancer", n=100, lang='en', since = "2019-07-28", until = "2019-07-29")
tw3 <- searchTwitter("lung cancer", n=100, lang='en', since = "2019-07-29", until = "2019-07-30")

# Convert the returned tweets in list to dataframe
df1 <- twListToDF(tw1)
df2 <- twListToDF(tw2)
df3 <- twListToDF(tw3)

# Combine tweets into one dataframe
df <- rbind(df1, df2, df3)

# Subset only two columns with tweet text and created date
df <- subset(df, select=c(text, created))

# Format tweet created date
df$created <- strftime(df$created, '%Y-%m-%d')

# save the returned tweets to local csv files
write.csv(df, file = "tweets_fp.csv", row.names=FALSE)

#=======================
# Part 2. Word Cloud 
#=======================

# check if the package is installed; if not, install it.
if (!require(tm)) { 
  install.packages("tm")
} 

# check if the package is installed; if not, install it.
if (!require(SnowballC)) { 
  install.packages("SnowballC")
} 

# check if the package is installed; if not, install it.
if (!require(wordcloud)) { 
  install.packages("wordcloud")
} 

# check if the package is installed; if not, install it.
if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer")
} 

# check if the package is installed; if not, install it.
if (!require(RCurl)) { 
  install.packages("RCurl")
} 

# check if the package is installed; if not, install it.
if (!require(XML)) { 
  install.packages("XML")
} 


# reference for code below: 
# http://www.sthda.com/english/wiki/word-cloud-generator-in-r-one-killer-function-to-do-everything-you-need

#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() : Word cloud generator
# - http://www.sthda.com
#+++++++++++++++++++++++++++++++++++
# x : character string (plain text, web url, txt file path)
# type : specify whether x is a plain text, a web page url or a file path
# lang : the language of the text
# excludeWords : a vector of words to exclude from the text
# textStemming : reduces words to their root form
# colorPalette : the name of color palette taken from RColorBrewer package, 
# or a color name, or a color code
# min.freq : words with frequency below min.freq will not be plotted
# max.words : Maximum number of words to be plotted. least frequent terms dropped
# value returned by the function : a list(tdm, freqTable)
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=3, max.words=200)
{ 
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer") 
  
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0, scale = c(3, 1),
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}
#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text<-function(url){
  library(RCurl)
  library(XML)
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also do not want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}

# read tweets 
df <- read.csv("tweets_fp.csv")

# generate word cloud
rquery.wordcloud(x=df$text, type="text", lang="english", excludeWords = NULL, 
                 textStemming = FALSE,  colorPalette="Dark2",min.freq=10,
                 max.words=200)

# an example of drawing a wordcloud from a websit
url = "https://datascience.kennesaw.edu"
rquery.wordcloud(x=url, type="url")

#===================================
# Part 3. Sentiment Classification
#===================================

# check if the package is installed; if not, install it.
if (!require(sentiment)) { 
  install_github('okugami79/sentiment140')
} 

library(sentiment)

# read tweets 
df <- read.csv("tweets_fp.csv")

# Perform sentiment classificaiton tweets
sentiment_df <- sentiment(df$text)

# rename the column polarity to sentiment
df$sentiment <- sentiment_df$polarity

# change the data type to factor
df$created <- as.factor(df$created)
df$sentiment <- as.factor(df$sentiment)

# count frequency in each group
library(plyr)
group_count <- count(df, c('created', 'sentiment'))
tot_count <- count(df, c('created'))

# rename the column 
group_count <- rename(group_count, c("freq"="sentiment_count"))
tot_count <- rename(tot_count, c("freq"="tot_count"))

# merge the group count and total count
final_count <- merge(group_count, tot_count, by="created")

# calculate the ratio
final_count$sentiment_ratio <- final_count$sentiment_count / final_count$tot_count

# draw the plot
library(ggplot2)
ggplot(final_count, aes(x=created, y=sentiment_ratio)) + geom_line(aes(group=sentiment, color=sentiment), size=2)

# get only the positive results
final_count_positive <- subset(final_count, sentiment=='positive')

# draw the plot on the positive only
ggplot(final_count_positive, aes(x=created, y=sentiment_ratio, group = 1)) + geom_line(size=2, color='cornflowerblue')

