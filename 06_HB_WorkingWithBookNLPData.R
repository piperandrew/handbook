######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

####### Working with bookNLP Data ########

#This script ingests bookNLP tokens files from a single directory
#It stores all tables as a single large table
#This allows you to perform analyses on categories of interest (POS, Supersense, In-Quotation, Characters, etc.)

#set your working directory to where your txt files are located
setwd("~/Data/bookNLP_child_select")

#get list of files in target directory
f.names<-list.files()

#create meta table variable to put all bookNLP tables into
book.df<-NULL
#create loop to go through and ingest each file
for (i in 1:length(f.names)){
  print(i)
  #ingest the i-th file
  a<-read.csv(f.names[i], sep="\t", quote = "", stringsAsFactors = F)
  #add filename column
  a$filename<-f.names[i]
  #add to meta-table
  book.df<-rbind(book.df, a)
}

#to tabulate a quality:

#Part of speech
sort(table(book.df$pos), decreasing = T)

#Supersense
sort(table(book.df$supersense), decreasing = T)

#to extract all persons
people.df<-book.df[book.df$ner == "PERSON",]
#to observe the top names of the corpus
sort(table(people.df$lemma), decreasing = T)[1:20]

#to extract places
places.df<-book.df[book.df$ner == "LOCATION",]
sort(table(places.df$lemma), decreasing = T)[1:20]

#measuring avg. sentence length across the corpus
mean(tapply(book.df$sentenceID, book.df$filename, function (x) mean(table(x))))

#create a subsample of random sentences from the middle of books
samp.v<-sample(seq(50,500,1), 50)
#create empty data frame
samp.df<-NULL
for (i in 1:nlevels(factor(book.df$filename))){
  #subset by book
  sub<-book.df[book.df$filename ==  levels(factor(book.df$filename))[i],]
  #sample sentences
  sub2<-sub[sub$sentenceID %in% samp.v,]
  samp.df<-rbind(samp.df, sub2)
}

#observe differences between the whole and the random sample
mean(tapply(samp.df$sentenceID, samp.df$filename, function (x) mean(table(x))))
summary(tapply(samp.df$sentenceID, samp.df$filename, function (x) mean(table(x))))
summary(tapply(book.df$sentenceID, book.df$filename, function (x) mean(table(x))))
boxplot(tapply(book.df$sentenceID, book.df$filename, function (x) mean(table(x))), tapply(samp.df$sentenceID, samp.df$filename, function (x) mean(table(x))))

####### Create POS Feature Space #######
#This is similar to creating a document term matrix, but instead of terms
#we are going to be using POS

#first get a list of all POS variables
Var1<-levels(factor(book.df$pos))
#create empty data frame
pos.df<-data.frame(Var1)
#run through each book
for (i in 1:nlevels(factor(book.df$filename))){
  #subset by book
  sub<-book.df[book.df$filename ==  levels(factor(book.df$filename))[i],]
  #table the POS
  df<-data.frame(table(sub$pos))
  #add to metatable
  pos.df<-merge(pos.df,df, by ="Var1", all.x=T)
}

#turn all NA into 0
pos.df[is.na(pos.df)]<-0

#turn POS features into row.names
row.names(pos.df)<-pos.df[,1]
pos.df<-pos.df[,-1]

#add filenames to columns
colnames(pos.df)<-levels(factor(book.df$filename))

#transpose
pos.df<-t(pos.df)

#get word counts for each work
word.count<-tapply(book.df$tokenId, book.df$filename, function (x) max(x))
word.count<-as.numeric(unname(word.count))

#divide POS counts by word count
pos.df<-pos.df/word.count



