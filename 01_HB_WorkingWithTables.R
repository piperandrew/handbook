######### The Fish and the Painting ##########
######### by Andrew Piper ####################
######### CC By 4.0 License ##################

########### Section 0.1: Working with Tables #############

#In this section you will get comfortanle manipulating tables in R and understanding
#the data they contain. Before you begin to ingest and analyze documents it is important
#to see how much information is already encoded in your metadata.

#In the next section, "Working with GGPLOT" I will walk you through
#some useful graphing parameters.

#As an example we will be using the metadata to the Novel150 data set

#set your working directory
setwd("~/Websites/git_piperandrew/handbook")

#load table
#you should see a data frame with 9 columns
a<-read.csv("txtlab_Novel150_English.csv")

#what are those columns?
colnames(a)

######### CATEGORICAL DATA #############
#There are two kinds of data stored in our data frame:
#numerical data and categorical data
#numerical data for example refers to the dates of publication or word counts
#categorical data refers to the author's gender or the novel's point of view
#R will refer to categorical data as "factors" that contain different levels.
#"gender" is a factor that containes two levels in our data for example.

#to call a column use the $ sign.
a$filename

#notice how it will list the filenames but also say "150 levels:..."
#unless you tell it otherwise R turns strings into factors. What does that mean?
#strings = strings of letters or numbers (i.e. words or numbers)
#factors = categories which can have multiple levels
#so what has happened here is that R has turned the filenames into 150 separate levels
#of a single variable called "filenames." Sometimes this is useful, sometimes not.
#You can tell if a word is a factor or a string by whether it has quotes around it.
#Notice how the titles don't have quotes. That's how you know they are factors

#this prints the first title as a string not a factor (and thus has quotes)
#ie. we can change things from one "type" to another by using the "as" function
#(you could go in reverse and turn a string into a factor using as.factor())
as.character(a$filename[1])

#factors are useful if you have a variable of interest with multiple levels.

#one example are authors. You may have multiple authors in your data set and you
#should have a general idea about how much author repetition you have. 
#In general if you want to generalize about larger social practices you don't want too many
#books by the same author

#to observe this you use the following two functions:

#this tells us how many "levels" there are in the "author" factor
nlevels(a$author)

#this is akin to asking how many unique authors there are
length(unique(a$author))

#to observe the list of authors
levels(a$author)

#to find out who has the most books
#the table function adds up categories for you, in this case author names
#the sort function puts them in order from lowest to highest
#change decreasing = T for the opposite order
#notice how we can put functions inside of functions
sort(table(a$author), decreasing = F)

#so you can see no author has more than 3 books in our data set

#Another example is look at the column called "gender".
levels(a$gender)

#similarly we can use the table function to see what the ratio is
table(a$gender)

#SAMPLE QUESTION: are women more likely to be associated with first or third person novels or neither?
#how would you find out?

#while we're getting a bit ahead of ourselves we could create what is know as a contingency
#table that records the following information:

            #Female   #Male
      #1P     x         y
      #3P     z         w

#Here we are asking what is the ratio of women writers in the first person to women writers
#in the third person *relative* to men in the same categories. If they are equally distributed
#we should see no statistically significant difference.

#first let's build a table

#how many women wrote first person novels?
#to do this I subset the data frame "a" by gender equalling women AND point of view equalling first person novels
#remember, brackets are for subsetting something. Because this is a data frame and thus
#has two dimensions, I use a comma after my conditions to indicate: keep only those rows that match these conditions
#if I put this after the comma I am asking to keep only those columns that match these criteria
x<-nrow(a[a$gender == "female" & a$person == "first",])

#how many women wrote third person novels (i.e. not first person)?
z<-nrow(a[a$gender == "female" & a$person == "third",])

#how many men wrote first person novels?
y<-nrow(a[a$gender == "male" & a$person == "first",])

#how many women wrote third person novels (i.e. not first person)?
w<-nrow(a[a$gender == "male" & a$person == "third",])

#now construct a table (i.e. a "data frame")
cont<-data.frame(c(x,z), c(y,w))

#inspect to make sure it is correct and label it to avoid confusion
colnames(cont)<-c("female", "male")
row.names(cont)<-c("first", "third")

#we're now asking if the ratio of 15/54 is roughly equal 28/53

#to do so we'll use a fisher's exact test
fisher.test(cont)

#the outputs are interesting here. First they tell us that women are about half as likely
#to write novels in the first person in our data set.
#Second it tells us that we ought not to put too much emphasis on this difference given
#the small sample size and relative closeness of the values.
#We see how the p=value is estimated at 0.1036. Typically this does not fall below our
#arbitrary threshold of 0.05, i.e. there is more than a 5% chance that if we ran this 
#experiment many more times we might find no difference in the amount of first person
#novels women write relative to men. In other words we have our human judgment, which 
#says they do it half as much which feels meaningful and our statistical judgment, which
#says this could just be due to random chance.

#what else can we learn from tables?


##########   NUMERICAL DATA     #########

#our data also contains two columns of numerical data. How do we handle that?

#first, you could take the mean of the publication dates
mean(a$date)

#1862 is the mean publication date in this data. That's very useful to know. It tells us
#where the centre of gravity is in our data. We can get fancier with summary:

summary(a$date)

#Now we know the earliest date associated with a novel and the latest
#This is useful to assess the historical boundaries of your data. If your data stops
#at 1930 you cannot talk about the "20C novel"

#We can also assess how well the mean and median line up -- the less they do so the more
#skew there is in the data. Here they are almost the same.

#a third way you can handle numerical data is to assess the overall distribution of values
#are some periods cover better than others?

#First we can use a histogram. 

hist(a$date)

#In general we see how we have more books from later decades in our data, though 
#there is decent representation across the entire timeline.
#Depending on your research goals you would either want a sample that approximates 
#titles published (and is thus skewed towards later periods because as time passes 
#there are always more novels) OR you might want to sample evenly across all periods 
#to ensure that your findings aren't skewed by a particular period. 
#See the section on data selection for addressing these complex issues.

#because there aren't so many dates we could also just plot each year individually
plot(table(a$date))

#this lets us see that we have no more than 4 novels from the same year, very few extended gaps
#and once again pretty good balance across the whole period.

#now try it yourself with the novels' word counts

#you should get a mean of 123,240
#you should get a maximum value of 356,109 and minimum of 23,275

#try making a histogram. What do you find?

#SAMPLE QUESTION: Do women tend to write shorter novels than men in our sample?

#how would you answer this? See the section on hypothesis testing to guide you 
#through a solution


####### Manipulating Numerical Values #######

#The final thing I want to look at is performing math on columns and matrices. 

#### Example 1 ####

#So for example, what happens if you realized that every single book had in fact 
#exactly 500 words of boiler plate in its front matter and so your word counts were 
#over-estimated. If you wanted to remove 500 from the length values you go:

a$adjusted.length<-a$length-500

#notice how I created a new column in case I made a mistake and preserved it as a new variable.
#I might also want to compare the old and the new columns so it makes sense to keep both.

#You should see that the new column has the same values minus 500 for each word count. R
#just performs the -500 on all values in the vector, which is very convenient.

#a few other useful functions for exploring your data

#this sums all columns in a matrix
# it is not appropriate for a data frame because some of your columns aren't numbers
colSums() 
#the same thing for rows
rowSums()


###### Example 2 ######

#let's say you wanted to study decades not years.
#let's go ahead and transform years to decades by removing the final number and adding a 0

#first we'll convert the date column to a column of strings to utilize the substring function
a$decade<-as.character(a$date)

#then we transform the 4th digit to a 0
substring(a$decade, 4, 4) <- "0"

#and convert back to integers
a$decade<-as.numeric(a$decade)

#now we can see decade-level counts of novels
plot(table(a$decade))

#SAMPLE QUESTION: what is the avg. length per decade?
#in order to run a function (in this case take the mean) over different aspects of our data
#we will learn one of the "apply" functions. These are very useful -- and very confusing in R.
#here we will learn tapply which works on "factors." We will treat our decades as factors
#in order to see what the avg. word count is by decade.

#tapply takes as input the column you want to measure, the factor you want to subset by, and
#the function you want to run. In this case, we want to measure the mean of the "length"
#column relative to the different decades (our "factor" where each level is 10 years)
dec.length<-tapply(a$length, as.factor(a$decade), mean)

#this gives us a vector of means, which we can then plot
#because we are moving between factors and numbers the plotting is a little more involved
plot(dec.length, xaxt="n", ylab="avg. word count", xlab = "decade")
axis(1, at=as.factor(a$decade), labels = a$decade)

#notice how there is a period between the 1820s and 1860s where novels appear longer
#this is particularly concentrated in the 40s, 50s and 60s

#another way to visualize this is through the use of boxplots. These allow you to see
#the range of values for each decade which will give you a better sense of those
#periods that are particularly different. Read up on boxplots as they are a useful
#way of visualizing and comparing your data.
#the boxplot function takes the following logic: we want to know the distribution of 
#word length as a function of the decade of publication. So your dependent variable
#(here word length) goes first and then your independent variable (decade) goes second
boxplot(a$length ~ a$decade, ylab="word count", xlab="decade")










