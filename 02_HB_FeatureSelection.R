######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Section 2.0: Feature Selection and Construction #############

## this code assumes that you have run 01_Fish_PreparingYourData.R

##### Visualizing Zipf's Law(ish) #####

#sort your words by raw counts in descending order
top.words<-sort(col_sums(corpus1.dtm), decreasing = T)

#inspect the top 10
top.words[1:10]

#create a graph of the top 1000 words
#(don't worry, you'll learn much cooler graphing techniques later)
options(scipen = 999)
plot(top.words[1:1000], xlab="words", ylab="frequency")
text(1:3, unname(top.words[1:3]), labels=c(names(top.words[1:3])), cex=.7, pos=4)


############# Keeping Only Stopwords #############

#here is a list of TM's stopwords
stopwords("en") #notice how the punctuation is still there

#subset your DTM by keeping only stopwords

#create a variable of your stopwords w punctuation removed
stop<-stopwords("en")
stop<-gsub("[[:punct:]]", "", stop)

#if you want to add additional words
#stop<-append(stop, c("INSERTWORD", "INSERTWORD", "ETC"))

#keep only stopwords in your DTM
dtm.stop<-dtm.scaled[ ,which(colnames(dtm.scaled) %in% stop)]


########## Removing stopwords #############

#keep all words NOT in the stopword list
dtm.nostop<-dtm.scaled[ ,which(!colnames(dtm.scaled) %in% stop)]


########## Removing the long tail ##########

## Approach 1: keep the top N words

#first create a variable of the top N words (default = 10K)
top.words2<-sort(col_means(dtm.nostop), decreasing = T)[1:10000]

#subset your dtm by these words
dtm.top10k<-dtm.scaled[,which(colnames(dtm.scaled) %in% names(top.words2))]

## Approach 2: keep words that appear in a majority of documents

#the integer is the *inverse* percentage of documents you want a word to 
#appear in
#in other words if you want words to appear in at least 60% of your documents
#then you use 0.4; if you want words that appear in a minimum of 5% of documents
#then you use 0.95
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.4)

#inspect the top 5 and bottom 5 words of your matrix
sort(col_means(dtm.sparse), decreasing = T)[1:5]
sort(col_means(dtm.sparse), decreasing = T)[(length(colnames(dtm.sparse))-4):length(colnames(dtm.sparse))]

#notice how this includes words like "said" and also chapter headings. ugh.
#so we want to remove these things. Here's how.

#first remove words that are less than 3 letters.
dtm.nostop<-dtm.nostop[, which(!nchar(colnames(dtm.nostop)) < 3)]

#then create a custom list of words
stop.xtra<-c("said", "one", "will")

#append a list of roman numerals
stop.xtra<-append(stop.xtra, tolower(as.roman(1:1000)))

#remove these words
dtm.nostop<-dtm.nostop[, which(!colnames(dtm.nostop) %in% stop.xtra)]

#rerun remove sparse terms
dtm.sparse<-removeSparseTerms(dtm.nostop, 0.4)

#recheck word list
sort(col_means(dtm.sparse), decreasing = T)[1:5]
sort(col_means(dtm.sparse), decreasing = T)[(length(colnames(dtm.sparse))-4):length(colnames(dtm.sparse))]

#You're good to go!


