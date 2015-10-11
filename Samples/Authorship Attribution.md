Authorship Attribution (Naïve Bayes and Multinomial Logistic Regression)
==========

### Creating a Dense Document Term Matrix

Import necessary libraries and source necessary functions.

Import data and create a corpus.

``` r
author_dirs = Sys.glob('../data/ReutersC50/C50train/*')
file_list = NULL
labels = NULL
for(author in author_dirs) {
    author_name = substring(author, first=29)
    files_to_add = Sys.glob(paste0(author, '/*.txt'))
    file_list = append(file_list, files_to_add)
    labels = append(labels, rep(author_name, length(files_to_add)))
}

all_docs = lapply(file_list, readerPlain)
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))
my_corpus = Corpus(VectorSource(all_docs))
names(my_corpus) = labels
```

Preprocess the corpus.

``` r
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("en")) #remove basic English stopwords
my_corpus = tm_map(my_corpus, stemDocument) #stem document
```

Create a document term matrix, remove sparse terms, and create a dense matrix. Also, create a vector of training set authors

``` r
DTM = DocumentTermMatrix(my_corpus) #create a document term matrix
DTM = removeSparseTerms(DTM, 0.996) #remove words that are in ten or fewer documents
X = as.matrix(DTM) #create a dense matrix
authors=rownames(X) #create a vector of training set author names
```

Create a document term matrix and author list for the test set.

``` r
author_dirs = Sys.glob('../data/ReutersC50/C50test/*')
file_list = NULL
labels = NULL
for(author in author_dirs) {
    author_name = substring(author, first=28)
    files_to_add = Sys.glob(paste0(author, '/*.txt'))
    file_list = append(file_list, files_to_add)
    labels = append(labels, rep(author_name, length(files_to_add)))
}

all_docs = lapply(file_list, readerPlain)
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))
my_corpus_test = Corpus(VectorSource(all_docs))
names(my_corpus_test) = labels

my_corpus_test = tm_map(my_corpus_test, content_transformer(tolower)) # make everything lowercase
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeNumbers)) # remove numbers
my_corpus_test = tm_map(my_corpus_test, content_transformer(removePunctuation)) # remove punctuation
my_corpus_test = tm_map(my_corpus_test, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeWords), stopwords("en")) #remove basic English stopwords
my_corpus_test = tm_map(my_corpus_test, stemDocument) #stem document

DTM_test = DocumentTermMatrix(my_corpus_test) #create a document term matrix
DTM_test = removeSparseTerms(DTM_test, 0.996) #remove words that are in ten or fewer documents
X_test = as.matrix(DTM_test)
authors_test=rownames(X_test)
```

We need to handle words that are in the test set but not in our training set. We will drop them out of our test set because they do not help our model's predictions. We must similarly handle words that are in the training set but not in the test set so our multiplication works. We will add columns of zeros to the test columns.

``` r
X_names<-colnames(X)
X_test_names<-colnames(X_test)

drop_list<-vector(length=0) #initialize a vector of words to drop out of test
zero_list<-vector(length=0) #initialize a vector of words to drop out of train

for (stem in X_test_names)  { #find word stems that are in test but not in train
  if (!stem %in% X_names) {
    drop_list<-c(drop_list,stem)
  }
}

X_test_mod<-X_test[,!colnames(X_test) %in% drop_list] #drop words from test that are in test but not in train

for (stem in X_names)  { #find words that are in train but not in test
  if (!stem %in% X_test_names) {
    zero_list<-c(zero_list,stem)
  }
}

#add columns of zeros to test for words that are in train but not in test
zeroes<-matrix(0,dim(X_test)[1],length(zero_list))
colnames(zeroes)<-zero_list
X_test_mod<-cbind(zeroes,X_test_mod)
X_test_mod<-X_test_mod[,order(colnames(X_test_mod))]
```

### Model 1: Naive Bayes

*This model assumes words are independently distributed.*

Calculate logged multinomial probability vectors for each author.

``` r
smooth_count=1/nrow(X)
byauthor=rowsum(X+smooth_count, authors) #sum word counts for individual words by author
w=rowSums(byauthor) #sum total word count by author
w_author=log(byauthor/w) #avoid underflow
```

We multiply the modified test DTM by the transposed modified multinomial probability matrix to arrive at a matrix of documents with Naive Bayes scores per author.

``` r
nbscores<-X_test_mod%*%t(w_author)
```

We create a comparison matrix of the Naive Bayes prediction (the author with the highest logged probabilities per document) versus the actual author.

``` r
nbprediction<-colnames(nbscores)[max.col(nbscores)] #highest logged probability
nbcorrect<-as.integer(nbprediction==authors_test) #does prediction match actual?
correct_matrix<-cbind.data.frame(authors_test,nbprediction,nbcorrect) #cbind
```

This model correctly predicts the author about 64% of the time across the entire test set.

``` r
mean(nbcorrect)
```

    ## [1] 0.6388

### Model 2: Multinomial Logistic Regression Using Principal Components

**Our Naive Bayes model assumes that word counts are not correlated. This model allows us to relax that assumption. Because we have so many more features than observations, we will use principal components to reduce the number of features and thus the variance of our final prediction. We will cross-validate to choose the number of principal components we use to build our model.**

We run principal a principal components and cross-validate over a sequence of k's (number of principal components) to choose which k to use to build our final model.

``` r
set.seed(1234)

train<-createDataPartition(y = authors, p=0.5, times=1, list = FALSE) #divide the training set into two stratified datasets for cross-validation purposes

CV_train<-X[train,]
CV_test<-X[-train,]
y_train<-as.factor(authors[train])
y_test<-as.factor(authors[-train])

q<-c(2,10,100,seq(250,1000,250)) #create a sequence of k's over which to cross-validate

mean<-numeric(length=length(q)) #create an empty vector to store the accuracy rates

counter=1 #initialize a counter to ensure the means vector is accurately filled out.

pc<-prcomp(CV_train) #run a principal components analysis on the cross-validation training set.

#calculate the accuracy over the sequence of k's
for (n in q) {
  scores<-pc$x[,1:n]
  glm<-glmnet(y=y_train, x=scores, family="multinomial", alpha=0)
  loading<-pc$rotation[,1:n]
  CV_test_scores<-CV_test%*%loading
  predict<-predict(glm,newx=CV_test_scores,type="class",s=0)
  mean[counter]=mean(as.integer(y_test==predict))
  counter=counter+1
}

CV_error=cbind.data.frame(q,mean)

CV_error[which.max(CV_error$mean),] #find the k with the highest accuracy
```

    ##     q   mean
    ## 6 750 0.7672

We see that 500 principal components has the highest accuracy. We will use this to build our full model.

``` r
pc<-prcomp(X) #run principal components analysis on the entire training dataset.
X_scores<-pc$x[,1:500] #use the first 500 principal components
glm<-glmnet(y=authors,x=X_scores,family="multinomial",alpha=0) #fit a multinomial logistic regression.
```

We transform our test matrix.

``` r
loading<-pc$rotation[,1:500] #the loadings of the first 500 principal components
X_test_scores<-X_test_mod%*%loading #transform test matrix
mlrpredict<-predict(glm,newx=X_test_scores,type="class",s=0) #predictions
```

We add these predictions and correctness measures to our correct matrix.

``` r
mlrcorrect<-as.integer(mlrpredict==authors_test) #does prediction match actual?
correct_matrix<-cbind.data.frame(correct_matrix,mlrpredict,mlrcorrect)
```

This model has an accuracy of 63%, which is similar to our Naive Baye's model.

``` r
mean(mlrcorrect)
```

    ## [1] 0.6324

### Summary

I prefer the Naive Bayes model. Although PCA multinomial logistic regression and Naive Bayes have similar accuracy scores, Naive Bayes is a simpler, more interpretable model that requires fewer computing resources and less time.

The Naive Bayes model particularly struggles with the following authors. They all have accuracy scores under 50%: Fewer than 50% of their articles are correctly attributed to them.

``` r
final <- ddply(correct_matrix, .(authors_test), transform, sum.n = length(authors_test))
xtab<-ddply(final, .(authors_test, nbprediction), summarise,number = length(nbprediction), proportion = number / sum.n[1] * 100)
poor.prediction<-xtab[xtab$authors_test==xtab$nbprediction & xtab$proportion<50,]
poor.prediction #number and proportion refer to the number and proportion of predictions for that predicted author and that actual author
```

    ##         authors_test     nbprediction number proportion
    ## 10    AlexanderSmith   AlexanderSmith     23         46
    ## 15   BenjaminKangLim  BenjaminKangLim     13         26
    ## 39  DarrenSchuettler DarrenSchuettler     11         22
    ## 44       DavidLawder      DavidLawder      7         14
    ## 72  HeatherScoffield HeatherScoffield     22         44
    ## 83     JaneMacartney    JaneMacartney     20         40
    ## 213       MureDickie       MureDickie     17         34
    ## 278      ScottHillis      ScottHillis      9         18
    ## 291         TanEeLyn         TanEeLyn     15         30
    ## 310       ToddNissen       ToddNissen     22         44
    ## 321     WilliamKazer     WilliamKazer     16         32

We further examine these authors to see if particular pairs of authors give our Naive Bayes model particular difficulty. The following pairs are often confused. At least 20% (10 documents) of the actual author's documents are misclassifed as the predicted author's works.

``` r
xtab[xtab$authors_test %in% poor.prediction$authors_test & xtab$number>10 & xtab$authors_test!=xtab$nbprediction,]
```

    ##         authors_test     nbprediction number proportion
    ## 11    AlexanderSmith         JoeOrtiz     22         44
    ## 17   BenjaminKangLim    JaneMacartney     13         26
    ## 40  DarrenSchuettler HeatherScoffield     36         72
    ## 48       DavidLawder       ToddNissen     27         54
    ## 85     JaneMacartney      ScottHillis     20         40
    ## 219       MureDickie     WilliamKazer     11         22
    ## 274      ScottHillis    JaneMacartney     24         48
    ## 288         TanEeLyn    PeterHumphrey     20         40
    ## 289         TanEeLyn     SarahDavison     11         22
    ## 304       ToddNissen      DavidLawder     12         24
