NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)

NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)


NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")


NewsTrain$Weekday = NewsTrain$PubDate$wday

NewsTest$Weekday = NewsTest$PubDate$wday


# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)



library(tm)

# Then create a corpus from the headline variable. You can use other variables in the dataset for text analytics, but we will just show you how to use this particular variable. 
# Note that we are creating a corpus out of the training and testing data.

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadline = tm_map(CorpusHeadline, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)

CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)

CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))

CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusHeadline)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of HeadlineWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(NewsTrain) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTrain"

HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))

# The tail function takes the last "n" rows of HeadlineWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(NewsTest) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTest"

HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))


HeadlineWordsTrain$NewsDesk = NewsTrain$NewsDesk

HeadlineWordsTrain$SectionName = NewsTrain$SectionName

HeadlineWordsTrain$SubsectionName = NewsTrain$SubsectionName

HeadlineWordsTrain$WordCount = NewsTrain$WordCount

HeadlineWordsTrain$Weekday = NewsTrain$Weekday

HeadlineWordsTrain$Popular = NewsTrain$Popular



HeadlineWordsTest$NewsDesk = NewsTest$NewsDesk

HeadlineWordsTest$SectionName = NewsTest$SectionName

HeadlineWordsTest$SubsectionName = NewsTest$SubsectionName

HeadlineWordsTest$WordCount = NewsTest$WordCount

HeadlineWordsTest$Weekday = NewsTest$Weekday



install.packages("caret")

library(caret)

install.packages("e1071")
library(e1071)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = (0:10)*0.001)

tr = train(Popular ~ ., data = HeadlineWordsTrain, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

NewsTest$NewsDesk <- factor(as.character(NewsTest$NewsDesk),levels=unique(NewsTrain$NewsDesk))

NewsTest$NewsDesk = relevel(NewsTest$NewsDesk , ref="")

best.tree = tr$finalModel

prp(best.tree)

best.tree.pred = predict(tr, newdata = HeadlineWordsTest)

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = best.tree.pred)

write.csv(MySubmission, "SubmissionWordCART2.csv", row.names=FALSE)
