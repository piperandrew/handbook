######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Sentiment Analysis Module 1: Working with Dictionaries #############
#in this script you are going to learn how to subset your feature space 
#according to a predefined dictionary (or "lexicon"). You can construct any set of words
#that you feel represents a category of semantic interest. For example, "pronouns"
#would be a larger category that you construct using a list of words.

#"Sentiment Analysis" is a popular application to the study of texts. We will be 
#exploring a few different implementations during this course. The
#simplest relies on a fixed set of words that have been weighted
#by their "valence" (i.e. positive or negative), "arousal" (intensity), and "dominance" (control).
#Researchers have also experimented with dictionaries of emotion words to estimate the distribution of emotions
#represented in a text.


#this script assumes you have run 02_HB_PreparingYourData and 
#04_HB_FeatureSelection

#as a shortcut you can load the sample R workspace
#this contains all of the necessary DTMs for the following
#make sure to insert your proper filepath here, this is an example
load("~/Data/txtlab_Novel150_English_DTM.RData")
library(tm)
library(slam)

#load metadata
setwd("~/Data")
meta<-read.csv("txtlab_Novel150_English.csv")

##### #Example 1: Make your own dictionary (or "feature space") #####
ppronouns<-c("she", "he", "his","her","hers", "him", "himself", "herself","we","us","themselves","them", "their","theirs")

#subset your scaled dtm by the dictionary
dtm.dict<-as.matrix(dtm.scaled[, which(colnames(dtm.scaled) %in% ppronouns)])

#take the mean values of your rows
#this tells you the overall mean value of all of your words in your dictionary per document
dict.results<-row_means(dtm.dict)

#sort from highest to lowest
dict.results<-sort(dict.results, decreasing = F)

#### subset your DTM by metadata #####
dtm.m<-dtm.dict[row.names(dtm.dict) %in% as.character(meta$filename[meta$gender == "male"]),]
dtm.f<-dtm.dict[row.names(dtm.dict) %in% as.character(meta$filename[meta$gender == "female"]),]

#Based on your previous work, you should be able to estimate now the rates at which
#men and women authors use pronouns. I'll ask you below to undertake this task and ones like it.

##### Example 2: Working with sentiment ####

### First let's test emotions

#ingest emotion dictionary
#it contains 3 columns, one with words, the second with the emotions (plus positive/negative), and the third with 0/1 for yes/no
setwd("~/Data/Dictionaries")
emolex<-read.csv("NRC-Emotion-Lexicon-Wordlevel-v0.92.txt", sep="\t", header=F)

#subset our DTM by a particular emotion: i.e. "joyful" words
#subset the emotion lexicon by "joy"
joy<-emolex[emolex$V2 == "joy",]
joy<-joy[joy$V3 == 1,]
#subset your DTM by those words
dtm.joy<-dtm.scaled[, which(colnames(dtm.scaled) %in% as.character(joy$V1))]

#take your mean values for all terms in your novel
joy.results<-sort(row_means(dtm.joy), decreasing = F)

#now you can repeat this for any emotion or combination of emotions
#to condition on two variables here is an OR statement
joy.trust<-emolex[emolex$V2 == "joy" | emolex$V2 == "trust",]

### Now let's work on "valence"

#ingest dictionary
sent<-read.csv("NRC-VAD-Lexicon.txt", sep="\t", header=T)

#this time we need to multiply our raw frequencies by the valence value
#so for every word in our dictionary we multiply the column in our DTM by that word's "valence"

#first subset our dtm by words in the valence dictionary
dtm.valence<-as.matrix(corpus1.dtm[,which(colnames(corpus1.dtm) %in% as.character(sent$Word))])

#then subset our valence dictionary by words in our DTM (i.e. make them match)
sent.sub<-sent[as.character(sent$Word) %in% colnames(dtm.valence),]

#check results
which(!colnames(dtm.valence) %in% as.character(sent.sub$Word))

#now multiply every column by its value in the Valence table
val.df<-t(t(dtm.valence)*sent.sub$Valence)

#now we sum the rows (which are the combined valence score for each word) and divide by the wordcount of each novel
#i.e. we take the average valence score for the novel
val.results<-sort(row_sums(val.df)/row_sums(dtm.valence), decreasing = F)


######## Sample Questions #########

#1. Compare the rate of joy words for novels by women to novels by men. What do you see?

#2. Ditto for anger and fear combined.

#3. Compare the avg. valence of novels by women and men. What do you see?

#4. Extra Credit: Can you do this for arousal and dominance as well?

#5. Compare the rates of masculine and feminine pronouns for novels by men and women. 
#Remember you will need to subset by pronoun type *and* by novel type.
#to report your results can you provide the raw values but you can also report ratios.
#(Men use x-times as many masculine pronouns as men in...etc.)

#6. Extra Credit 2: What % of joy in novels does "love" account for?

#In future models we will learn how to run hypothesis testing to see if these differences
#are statistically meaningful.





