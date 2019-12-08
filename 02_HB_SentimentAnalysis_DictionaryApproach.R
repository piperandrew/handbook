######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Section 2.1: Sentiment Analysis Module #############

#this script assumes you have run 01_Fish_PreparingYourData and 
#02_Fish_FeatureSelection

#as a shortcut you can load the sample R workspace
#this contains all of the necessary DTMs for the following
load("~/Data/00_Fish_DTM_Novel150.RData")

#ingest sentiment dictionary
#it contains 2 columns, one with the words, the other with the valence
setwd("~/Data/Dictionaries")
sent<-read.csv("Bing_Sentiment_All.csv")

#subset your scaled dtm by the sentiment dictionary
dtm.sent<-dtm.scaled[, which(colnames(dtm.scaled) %in% as.character(sent$term))]

#now collapse this multidimensional thing into a single dimension called "sentiment"
sent.score<-row_sums(dtm.sent)

#what if you just wanted to create a feature based on "positive" words?
pos.score<-row_sums(dtm.sent[,colnames(dtm.sent) %in% sent$term[sent$valence == "pos"]])

#let's turn this data into a question:
#DO WOMEN NOVELISTS OF THE 19C USE MORE POSITIVE LANGUAGE THAN MEN?

#to answer this we need to subset our data twice.

#First, keep only positive words
pos<-sent[sent$valence == "pos",]
dtm.pos<-dtm.scaled[, which(colnames(dtm.scaled) %in% as.character(pos$term))]

#Next, we need to create two separate DTMs for our two categories
#(There are other ways to do this but this is for demo purposes to make
#things clearer)

#ingest table of metadata about the authors / documents
setwd("~/Data")
meta<-read.csv("txtlab_Novel150_English.csv")

#subset metadata by men/women
meta.men<-meta[meta$gender == "male",]
meta.women<-meta[meta$gender == "female",]

#subset DTM by men / women
dtm.men<-dtm.pos[row.names(dtm.pos) %in% as.character(meta.men$filename),]
dtm.women<-dtm.pos[row.names(dtm.pos) %in% as.character(meta.women$filename),]

#check to see if the # rows of the DTM matches the # rows of the metadata
#i.e. did your subset work??
nrow(dtm.men) == nrow(meta.men)
nrow(dtm.women) == nrow(meta.women)

#now run a statistical test to estimate whether women use more positive terms
#than men on average

#the first thing we do is create a super variable called "positivity"
#this represents the summed frequency for every word in our list, i.e. the total frequency
#at which positive words are used
men.pos<-row_sums(dtm.men)
women.pos<-row_sums(dtm.women)

#now that we have two "sample populations" we can compare the distribution
#of the rates of positivity to assess whether we think these samples are
#being drawn from different populations or the same one

#first test whether the variance of the two samples is equal
var.test(women.pos, men.pos)

#what you should see is that women have considerably *more* variance than men
#this is interesting, but it also means we need a test that does not require
#equal variance

#second test whether both have normal distributions
#first visualize
hist(women.pos)
hist(men.pos)
#then run a test for normality
shapiro.test(women.pos)
shapiro.test(men.pos)
#in this example both appear to be normal 

#now that we know that they have normally distributed data but not
#equal variance, we use a welch's t.test
t.test(women.pos, men.pos)

#we should see that women have a higher avg. rate of positive vocabulary
#by about .5% per text (or about 2.5 words/page)
#given the low p-value it is very unlikely that these authors come from the same overall
#population of writers. Thus, with respect to the use of positive language
#(which is being drawn from contemporary dictionaries not 19C ones!)
#we see a significant difference in how men and women deploy positive language
#in 19C novels

#Follow-up question: do women use a higher rate of sentiment vocabulary overall
#(whether negative or positive)?

#here we subset the overall sentiment table rather than the positive table
dtm.men<-dtm.sent[row.names(dtm.sent) %in% as.character(meta.men$filename),]
dtm.women<-dtm.sent[row.names(dtm.sent) %in% as.character(meta.women$filename),]
men.sent<-row_sums(dtm.men)
women.sent<-row_sums(dtm.women)
var.test(women.sent, men.sent)
hist(women.sent)
hist(men.sent)
shapiro.test(women.sent)
shapiro.test(men.sent)
t.test(women.sent, men.sent)







