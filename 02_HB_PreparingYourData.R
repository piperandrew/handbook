######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Section 1.0: Preparing your data #############

#Welcome to the fish and the painting. This is going to be so much fun.
#In this section you will learn how to ingest and prepare text data for
#analysis. This is actually the less fun part.

#The first thing you will need to do is install your libraries
#You only need to do this the first time.
#install.packages("tm")
#install.packages("textstem")
#install.packages("slam")

#load libraries (you need to do this every time)
library("tm")
#library("textstem")
library("slam")

#Set your working directory
#this is the folder *above* where your texts are located
setwd("~/Data")

#### Reading in Your Data: Option 1 (TM Package) #####

#Read in your corpus
#the name in "" is the name of your folder where your texts are
#set the language appropriately
corpus1 <- VCorpus(DirSource("txtlab_Novel150_English", encoding = "UTF-8"), readerControl=list(language="English"))

#Inspect a sample document (metadata)
inspect(corpus1[26]) #the number in brackets refers to the document number

#Inspect your data (see a portion of the actual text)
strwrap(corpus1[[26]])[1:5] #the second number in brackets 1:5 refers to the first five lines


#### Normalizing Your Data 1: Textual Normalization #####

#make all lowercase
corpus1 <- tm_map(corpus1, content_transformer(tolower))
#remove numbers
corpus1 <- tm_map(corpus1, content_transformer(removeNumbers))
#remove punctuation
#corpus1 <- tm_map(corpus1, content_transformer(removePunctuation))
f<-content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus1 <- tm_map(corpus1, f, "[[:punct:]]")
#strip white space
corpus1 <- tm_map(corpus1, content_transformer(stripWhitespace)) 


#Option: lemmatize your data (not often recommended)
#corpus1.lemma <- tm_map(corpus1, lemmatize_strings)
#corpus1.lemma <- tm_map(corpus1, PlainTextDocument)

#inspect
strwrap(corpus1[[26]])[1:5]

######## Make a document term matrix ###########

#run the function on your corpus variable
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept

#if you want to generate a table of ngrams (multiple words in sequence)

#first create function that defines the n in ngrams. 2 = bigrams or 2 words in a row
BigramTokenizer2 <- function(x)unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

#you can also create a function that captures 1- and 2grams
BigramTokenizer12 <- function(x)unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)

#rerun the DTM function w the bigram function inside
dtm.bigram <- DocumentTermMatrix(corpus1, control=list(tokenize = BigramTokenizer2, wordLengths=c(1,Inf)))


######## Beginning to understand your data: some initial metrics ###########

#How many documents do you have?
nrow(corpus1.dtm)

#How many word types?
ncol(corpus1.dtm)

#How many words overall?
sum(corpus1.dtm)

#Generate a list of word counts for each document
row_sums(corpus1.dtm) #if you want to save this as a variable (for example to export as a table, then do: wc<-row_sums(corpus1.dtm))

#what is the distribution of word counts across your data?
#histograms are a great way to assess your data
options(scipen=999)
hist(row_sums(corpus1.dtm))

#what is the average length of a novel?
#the summary function does something similar without the visualisation
summary(row_sums(corpus1.dtm))

#How can you find out which is the longest novel?
which.max(row_sums(corpus1.dtm))
row_sums(corpus1.dtm)[13]

#### Normalizing Your Data 2: Mathematical Normalization #####
#Because word counts vary considerably from word to word and
#from document to document for many tasks it is important 
#to adjust the raw frequencies above (but not always!)
#the first method does so by adjusting frequencies according to the
#document lengths; the second does so by adjusting word frequencies by
#their document frequency (i.e. in how many documents they appear)
#see the book for further explanation

#Method 1: Scaling
#divide the counts by the total number of words in each document.
dtm.scaled<-corpus1.dtm/row_sums(corpus1.dtm)

#Method 2: Tf-Idf
#tfidf = term frequency * inverse document frequency
#this weights words by how infrequent they are in the corpus
#the more infrequent they are across documents the higher the word's score
dtm.tfidf<-weightTfIdf(corpus1.dtm, normalize = TRUE)


