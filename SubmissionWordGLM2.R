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

CorpusHeadlineSnippet = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline, NewsTrain$Snippet, NewsTest$Snippet)))

# You can go through all of the standard pre-processing steps like we did in Unit 5:

CorpusHeadlineSnippet = tm_map(CorpusHeadlineSnippet, tolower)

# Remember this extra line is needed after running the tolower step:

CorpusHeadlineSnippet = tm_map(CorpusHeadlineSnippet, PlainTextDocument)

CorpusHeadlineSnippet = tm_map(CorpusHeadlineSnippet, removePunctuation)

CorpusHeadlineSnippet = tm_map(CorpusHeadlineSnippet, removeWords, stopwords("english"))

CorpusHeadlineSnippet = tm_map(CorpusHeadlineSnippet, stemDocument)

# Now we are ready to convert our corpus to a DocumentTermMatrix, remove sparse terms, and turn it into a data frame. 
# We selected one particular threshold to remove sparse terms, but remember that you can try different numbers!

dtm = DocumentTermMatrix(CorpusHeadlineSnippet)

sparse = removeSparseTerms(dtm, 0.99)

HeadlineSnippetWords = as.data.frame(as.matrix(sparse))

# Let's make sure our variable names are okay for R:

colnames(HeadlineSnippetWords) = make.names(colnames(HeadlineSnippetWords))

# Now we need to split the observations back into the training set and testing set.
# To do this, we can use the head and tail functions in R. 
# The head function takes the first "n" rows of HeadlineWords (the first argument to the head function), where "n" is specified by the second argument to the head function. 
# So here we are taking the first nrow(NewsTrain) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTrain"

HeadlineSnippetWordsTrain = head(HeadlineSnippetWords, nrow(NewsTrain))

# The tail function takes the last "n" rows of HeadlineWords (the first argument to the tail function), where "n" is specified by the second argument to the tail function. 
# So here we are taking the last nrow(NewsTest) observations from HeadlineWords, and putting them in a new data frame called "HeadlineWordsTest"

HeadlineSnippetWordsTest = tail(HeadlineSnippetWords, nrow(NewsTest))


HeadlineSnippetWordsTrain$NewsDesk = NewsTrain$NewsDesk

HeadlineSnippetWordsTrain$SectionName = NewsTrain$SectionName

HeadlineSnippetWordsTrain$SubsectionName = NewsTrain$SubsectionName

HeadlineSnippetWordsTrain$WordCount = NewsTrain$WordCount

HeadlineSnippetWordsTrain$Weekday = NewsTrain$Weekday

HeadlineSnippetWordsTrain$Popular = NewsTrain$Popular



HeadlineSnippetWordsTest$NewsDesk = NewsTest$NewsDesk

HeadlineSnippetWordsTest$SectionName = NewsTest$SectionName

HeadlineSnippetWordsTest$SubsectionName = NewsTest$SubsectionName

HeadlineSnippetWordsTest$WordCount = NewsTest$WordCount

HeadlineSnippetWordsTest$Weekday = NewsTest$Weekday

HeadlineSnippetWordsLog = glm(Popular ~ ., data=HeadlineSnippetWordsTrain, family=binomial)

PredTest = predict(HeadlineSnippetWordsLog, newdata=HeadlineSnippetWordsTest, type="response")

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "SubmissionWordGLM2.csv", row.names=FALSE)
