######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Section 1.0: Preparing your data #############

############ Reading in your data: Option 2 (One-by-One) #################

#Use this when you want to calculate something specific on each document
#and output that measurement to a new table
#i.e. you are creating new features beyond just using words

#get list of files in your directory
f.names<-list.files("txtlab_Novel150_English")

#change your working directory to that folder
setwd("~/Data/txtlab_Novel150_English")

#we're going to create a function that reads in the texts, cleans them,
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

#notice how this stores your text as a "vector", a running list of individual words
#where each word is indexed by its order
#you can see this by doing the following

work.v<-text.prep(f.names[1]) #this runs the function on the first document
work.v[1:10] #this shows you the first ten words of the first document -- everything should be lowercase and have no punctuation

#the next step is to create a function that does something to the text you've ingested
#let's use type-token ratio as our example

#type token ratio divides the number of word types by the total number of words
#it is very sensitive to length and thus must be used on texts of exactly the same length
#to do this we will take a random sample of X consecutive words from
#your document and do this 100 times. We will then take the average of all
#these random samples and use this as the TTR score for the entire document

#this function takes as its object a vector of words from a work and a window size
#the window size is the length of text you are sampling (recommended = 500)
ttr<-function(work.v, winsize){
  #first locate a random starting point 
  #take one random number from the overall length of the work (not including anything closer to the end than your sample will be, i.e. going past the end)
  beg<-sample(1:(length(work.v)-winsize),1)
  #extract that segment as a new vector
  test<-work.v[beg:(beg+(winsize-1))]
  #calculate TTR
  #TTR = unique words / total words (i.e. types divided by tokens)
  ttr.sample<-length(unique(test))/length(test)
  return(ttr.sample)
}

#to run the function on a window of 500 words from a single work called work.v you run:
ttr(work.v, 500)

#to run it  multiple times on the same work
replicate(100,ttr(work.v, 500)) #replicate the function 100 times

#to take the mean to get the avg ttr for this text
mean(replicate(100,ttr(work.v, 500)))

#so now we are going to combine the two functions and do this for every text
#and store the results in a table

#we're going to use a loop because its faster than storing everything in memory
#we just want to ingest each text, clean it, calculate something, and then store the results and move on

#create an empty table where you will store your results
ttr.df<-NULL
#create a loop that is as long as the number of documents
for (i in 1:length(f.names)){
  #see how fast things are going
  print(i)
  #ingest and clean each text
  work.v<-text.prep(f.names[i])
  #calculate the mean value of a vector of 100 TTR values
  ttr.mean<-mean(replicate(100,ttr(work.v, 500)))
  #store everything in a table
  filename<-f.names[i]
  temp.df<-data.frame(filename, ttr.mean)
  ttr.df<-rbind(ttr.df, temp.df)
}



