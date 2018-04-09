#test data
data = testS$comment_text
n = nrow(testS)
s = 1
corpus = Corpus(VectorSource(data[s:n]))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeWords, stopwords("english"))

dtmtest = DocumentTermMatrix(corpus, control = list(dictionary = Terms(dtm)))
mtest = as.matrix(dtmtest)
##add more features
library(stringr)
wordcount <- str_count(data[s:n], pattern = " ") #accuracy goes up
mtest = cbind(mtest, text_features(data[s:n]))
mtest = as.data.frame(mtest)
