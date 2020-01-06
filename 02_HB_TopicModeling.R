######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Section 2.2: Topic Modeling #############

#the following allows you run a topic model on a given set of documents
#it allows you to observe the words in each topic, the leading topics in each document,
#and also the overall distributions of topics within documents
#at the end there is an activity that allows you to test whether your topics are 
#significantly more present in one collection v. another.

#you can bypass the below by loading the topic modeling workspace "00_Fish_TopicModel_Novel150.RData"
#jump to line 120

library("tm")
library("proxy")
library("stats")
library("topicmodels")
library("slam")

###### Step 1: Chunk your documents ######
#Skip this step if your documents are thematically coherent and/or short

setwd("~/Data")
f.names<-list.files("txtlab_Novel150_English")

#create a function that reads in the texts, cleans them,
#and stores them in a variable called "work"
text.prep<-function(x){
  #first scan in the document
  work<-scan(x, what="character", quote="", quiet=T)
  #remove numbers
  work<-gsub("\\d", "", work)
  #remove punctuation
  work<-gsub("\\W", "", work)
  #make all lowercase
  work<-tolower(work)
  #remove blanks
  work<-work[work != ""]
}

#next write a loop that goes through every document
#and for each document, writes a chunk to a new folder as a separate file

#set the chunk size
chunk<-1000

for (i in 1:length(f.names)){
  #set your working directory inside of your novels
  setwd("~/Data/txtlab_Novel150_English")
  #see how fast things are going
  print(i)
  #ingest and clean each text
  work.v<-text.prep(f.names[i])
  #set your working directory to a new folder for the chunks
  setwd("~/Data/txtlab_Novel150_English_Chunks_1000")
  #set file number integer
  n=0
  #go through entire novel and divide into equal sized chunks
  for (j in seq(from=1, to=length(work.v)-chunk, by=chunk)){
    n=n+1
    sub<-work.v[j:(j+(chunk-1))]
    #collapse into a single paragraph
    sub<-paste(sub, collapse = " ")
    #write to a separate directory using a custom file name
    new.name<-gsub(".txt", "", f.names[i])
    new.name<-paste(new.name, sprintf("%03d", n), sep="_")
    new.name<-paste(new.name, ".txt", sep="")
    write(sub, file=new.name)
  }
}

###### Step 2: Create a DTM ######

setwd("~/Data")
corpus1 <- VCorpus(DirSource("txtlab_Novel150_English_Chunks_1000", encoding = "UTF-8"), readerControl=list(language="English"))
#no cleaning necessary as we already did that
#create a DTM
corpus1.dtm<-DocumentTermMatrix(corpus1, control=list(wordLengths=c(1,Inf))) #(1,Inf) refers to the range of word lengths kept

#only keep words that are in a custom dictionary
#this is the inverse of removing words we don't want

#ingest dictionary
setwd("~/Data/Dictionaries")
keep<-read.csv("Dict_English_NovelWords_3000_NoStop.csv", header=F, stringsAsFactors = F)

#subset your dtm by the dictionary
dtm<-corpus1.dtm[,which(colnames(corpus1.dtm) %in% keep$V1)]

#remove rows with all 0 values (this is caused by only keeping non sparse words)
row_zero<-row_sums(dtm)
length(which(row_zero == 0))

# if length > 0 !!!!! run these lines
row_sub<-as.vector(which(row_zero == 0))
corpus2<-dtm[-row_sub,]
mode(corpus2) <- "numeric"

#if length = 0 then run this line
corpus2<-as.matrix(dtm)

#define number of topics
k=20

#set parameters
#the key variable to change is "alpha"
#this determines whether you are looking for topics that are more unique to individual documents
#or whether you are looking for topics that are more well-distributed across the entire corpus
#a low alpha will give you very distinct topics (documents will tend to have one strong topic and the rest are meaningless)
#a high alpha will give you several topics that are associated in similar manner with a single document
#50/k is recommended for a high alpha, 0.001 for a low alpha
control_LDA_Gibbs<-list(alpha=(50/k), estimate.beta=TRUE, iter=1000, burnin=20, best=TRUE, seed=2)

#run topic model
#this can take awhile depending on how many texts you have. Be patient.
topicmodel<-LDA(corpus2, method="Gibbs", k=k, control = control_LDA_Gibbs) # k = # topics

#examine your results

### PROBABILITIES ###

#the first thing to do is generate outputs of probabilities of
#either your words to topics or your topics to documents
#this lets you see the nature of the distribution of these categories
probabilities<-posterior(topicmodel)


### WORDS ####

# this tells you top x terms per topic (common is to use 10 or 20)
#the integer changes the number of words shown.
#remember that all words are in every topic just with very low probabilities
term_dis<-terms(topicmodel, 20) 

#to see all words per topic
term_dis_all<-terms(topicmodel, ncol(corpus2)) 

#to write the top words per topic to a table
#write.csv(term_dis, file="NameOfCorpus_HowManyTopics_WordsPerTopic_Top20_MyAlpha.csv") #writes terms per topic

#to observe the probabilities associated with your words 
topic_word_probs<-as.data.frame(probabilities$terms)
#select your topic
topic.no<-1

#subset by your topic
prob_sample<-topic_word_probs[topic.no,]

#sort in descending order
title.g<-paste("Word Probabilities\nTopic ", topic.no, sep="")
prob_sample<-sort(prob_sample, decreasing=T)
plot(t(prob_sample), main=title.g, xlab="Words", ylab="Probability")

#inspect the probabilities of the top 20 words for this topic
prob_sample[1:20] 


### TOPICS ####

#this tells you the top topic per document
topic_dis_1<-topics(topicmodel, 1) 

#this tells you all the topic probabilities per document
top_dis_all<-topics(topicmodel, k)

#to write top topic per document
#write.csv(topic_dis_1, file="Corpus_HowManyTopics_Document_Top1_MyAlpha.csv") # writes top topic per document

#this generates a table of document to topic probabilities
topic_doc_probs<-as.data.frame(probabilities$topics)

#to check distribution of topics across documents - should be evenly distributed
#this is asking if all topics re equally represented across your documents
#or whether some topics appear more often than others
#x-axis = topic number. y-axis = frequency of that topic as the "top topic" in a document
plot(table(topic_dis_1), main="Topic Distribution", xlab="Topics", ylab="Frequency")

#to inspect topics associated with a single document
test.doc<-20
prob_sample<-topic_doc_probs[test.doc,] #the integer here = the document you want to inspect

#plot to identify which topic is highest and by how much
#y-axis = probability (between 0 and 1)
title.c<-paste("Probability of Topics in Document ", test.doc, sep="")
plot(t(prob_sample), main=title.c, xlab="Topics", ylab="Probability")

#plot in ranked order to observe distribution
#warning: the topic numbers do not refer to the actual topic numbers here
#they are just the ranked order (i.e. 20 is highest, not topic 20)
title.d<-paste("Topic to Document Probabilities\nfor Document ", test.doc, sep="")
prob_sample_sort<-sort(prob_sample)
plot(t(prob_sample_sort), main = title.d, xlab="Topic Ranks", ylab="Probability")

#next you can observe all of the documents associated with a topic

#first, define your topic
topic.no<-9

#then plot in descending order to understand the distribution
#warning again: the document #s do not refer to row numbers of actual docs, just their rank
title.f<-paste("Topic to Document Probabilities\nTopic ", topic.no, sep="")
prob_sample_sort<-sort(prob_sample)
plot(prob_sample_sort, main = title.f,xlab = "Documents", ylab = "Probability")


###### Further Model Diagnostics ######

#### Within Model Diagnostics ####

#this gives you a range of diagnostics with which to understand your
#topics before analyzing their content
#it goes through and calculates a set of scores for every topic
#so you can compare any chosen topic to the population of topics
#see p. 76f. in Enumerations for a discussion
#the measures are also commented below
library(splitstackshape)

#establish probability threshold for topic association with a document
#below this probability a topic will not be associated with a document
#this takes a probability that is 2sd above the overall mean probability
#the advantage is it allows multiple topics to be in a document
#disadvantage is it is a single arbitrary score
cut<-mean(as.matrix(topic_doc_probs))+(2*sd(as.matrix(topic_doc_probs)))

topic.df<-NULL
for (i in 1:ncol(topic_doc_probs)){
  print(i)
  #for every topic
  topic.no<-i
  #no. tokens
  #takes the top 20 words and counts their overall frequency in the corpus
  tok.sub<-corpus2[,which(colnames(corpus2) %in% as.character(term_dis[,topic.no]))]
  no.tokens<-sum(tok.sub)
  #no. documents
  #counts the number of documents that exhibit a given topic above some artificial threshold (here 0.2)
  doc.sub<-data.frame(row.names(topic_doc_probs), topic_doc_probs[,topic.no])
  doc.sub<-doc.sub[which(doc.sub$topic_doc_probs...topic.no. > cut),]
  no.docs<-nrow(doc.sub)
  if (no.docs > 5){
    #no. novels
    #counts how many novels the documents belong to
    nov.sub<-cSplit(doc.sub, "row.names.topic_doc_probs.", sep="_")
    no.novels<-nlevels(factor(nov.sub$row.names.topic_doc_probs._4))
    #heterogeneity
    #finds the number of documents that belong to the single most dominant novel to see the extent to
    #which the topic is dominated by a single novel
    #higher = more heterogenous, less dominated by a single novel
    concentration<-max(table(factor(nov.sub$row.names.topic_doc_probs._4)))/nrow(nov.sub)
    #avg. date
    #average date of the documents
    avg.date<-round(mean(nov.sub$row.names.topic_doc_probs._2))
    #sd.date
    #standard deviation of the document dates
    sd.date<-round(sd(nov.sub$row.names.topic_doc_probs._2))
    #coherence
    #see David Mimno's article -- measures co-document frequency relative to document frequency
    #the more words appear together in documents (co-document frequency) versus in a single document (doc frequency)
    #the more semantically coherent the topic
    tdm<-t(corpus2)
    tdm<-tdm[,colnames(tdm) %in% as.character(doc.sub$row.names.topic_doc_probs.)] #keep docs in the top topic list
    tdm<-tdm[row.names(tdm) %in% as.character(term_dis[,topic.no]),] #keep only top 20 topic words
    russel.dist<-as.matrix(simil(tdm, method = "Russel", convert_distances = TRUE))
    russel.final<-russel.dist*ncol(tdm)
    russel.final[is.na(russel.final)]<-0
    coherence.total<-0
    for (k in 1:nrow(tdm)) {
      doc.freq<-length(which(tdm[k,] != 0))
      vec1<-0
      for (m in 1:nrow(russel.final)) {
        if (russel.final[k,m] != 0){
          co.doc.freq<-as.integer(russel.final[k,m])
          coherence1<-log((co.doc.freq+1)/doc.freq)
          vec1<-vec1 + coherence1
        }
      }
      coherence.total<-coherence.total + vec1
    }
    #store in data frame
    temp.df<-data.frame(topic.no, no.tokens, no.docs, no.novels, concentration, avg.date, sd.date, coherence.total)
    topic.df<-rbind(topic.df, temp.df)
  }
}

##### Between Model Diagnostics ######

### Measure Topic Stability for Every Topic ###
#the goal of this script is to give you an idea how "stable" a given topic is
#across multiple models. It assumes you have run numerous topic models on the
#same text data and then selected one of them to be the model you choose to 
#work with. For any given topic you want to study further, it provides an avg.
#similarity score to all other models. In other words, it gives you a sense how
#consistent your topic is across all models. The more consistent the more confidence
#you can have that this is a stable topic independent of each model.

#To run this script it takes as input a directory of word-topic probabilities 
#from numerous topic models
#from the above code this would be:
#topic_word_probs<-as.data.frame(probabilities$terms)
#topic_word_probs is thus the output you need from every model run
#save these in a directory to be used here

#the script then assumes a primary model (the one you want to work with)
#it calculates the average divergence between
#every topic in that model and the most similar topic from every other model
#the goal is to identify which topics are more "stable", i.e. have a lower
#overall KL-divergence (information loss) across different models
#the assumption is that lower avg. KLD == more semantic stability to the topic
#important caveat: this is only relative to other topics in the same model
#i.e. this is not a universal value to compare to other models but just tells you
#among the topics in the model you've chosen, which one(s) are more similar across
#numerous runs and which one(s) are more divergent, i.e. less stable
library(topicmodels)
library(entropy)

#load all models
setwd("TK")
filenames<-list.files("topicmodel_samples")
setwd("TK/topicmodel_samples")

#load primary model
#this is the model you plan to use
#rows are words, columns are topics, values = probability of word being in topic
twp<-read.csv("model_original.csv")
#remove first column
twp<-twp[,-1]
#transpose columns and rows
twp<-t(twp)
#rename columns
colnames(twp)<-seq(1,ncol(twp))

#initialize output table
stable.df<-NULL
#run for every topic
for (i in 1:ncol(twp)){
  print(i)
  #subset by ith topic
  sub1<-twp[,i]
  #go through each model and find most similar topic
  test.t<-NULL
  #run through all but final model, which is your original
  for (j in 1:(length(filenames)-1)){
    #load next model
    comp<-read.csv(filenames[j])
    #clean
    comp<-comp[,-1]
    comp<-t(comp)
    colnames(comp)<-seq(1,ncol(comp))
    #comp should now mirror twp
    #go through every topic in comp to find most similar topic in twp
    #calculate KLD for every topic pair with the ith topic from primary model
    kld.v<-vector()
    for (k in 1:ncol(comp)){
      kld.v[k]<-KL.plugin(sub1, comp[,k])
    }
    #find minimum value, i.e. most similar topic
    top.t<-which(kld.v == min(kld.v))
    #which model
    model<-j
    #what was the divergence?
    kld.score<-kld.v[which(kld.v == min(kld.v))]
    #create data frame
    temp.df<-data.frame(model, top.t, kld.score)
    test.t<-rbind(test.t, temp.df)
  }
  #calculate mean and sd for the ith topic compared to best topic of all other models
  mean.kld<-mean(test.t$kld.score)
  sd.kld<-sd(test.t$kld.score)
  topic<-i
  temp.df<-data.frame(topic, mean.kld, sd.kld)
  stable.df<-rbind(stable.df, temp.df)
}




