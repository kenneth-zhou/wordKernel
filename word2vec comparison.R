######################### PRE-LOAD + PRE-TRAIN #####################################

## source: https://github.com/bmschmidt/wordVectors
## packages to install
# library(devtools)
#install_github("bmschmidt/wordVectors")

#loading packages
library(wordVectors)
library(stringi)

#prepare text for word2vec
#what it does:
#1) create single text file with contents of all documents
#2) clean and lowercase original text
#3) option to do n-grams (bundle_ngrams = ?)

setwd("C:/Users/Kenneth/Desktop/wordKernel/Datasets")
prep_word2vec(origin="OANC_subset_310",destination="word2vec_OANC_subset_310.txt",lowercase=T,bundle_ngrams=1)

#train model
if (!file.exists("word2vec_vectors.bin")) {model = train_word2vec("word2vec_OANC_subset_310.txt","word2vec_vectors.bin",vectors=500,threads=7,window=12,iter=5,negative_samples=5)} else model = read.vectors("word2vec_vectors.bin")

#IMdb
setwd("C:/Users/Kenneth/Desktop/wordKernel")
imdbdata = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")
imdbtest = imdbdata[20001:24722,2:3]
imdbtrain = imdbdata[1:20000,2:3]

########################## PRE-LOADING FUNCTIONS ############################

AverageVec = function(review, model)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, stripDigits = FALSE, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  #counter for number of words
  nwords = 0
  
  #empty vector
  totalvec = rep(0, ncol(model))
  
  #adding up each vector
  for (i in reviewWordVec)
  {
    if (i %in% rownames(model))
    {
      totalvec = totalvec + model[which(rownames(model) %in% i),]
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  
  return(averagevec)
}

#Function that returns the max "paragraph vector"
MaxVec = function(review, model)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, stripDigits = FALSE, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  # create empty data frame
  totalvec = data.frame()
  
  #collecting each word vector into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(model))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(model[which(rownames(model) %in% i),],nrow=1)))
    }
  }
  
  #taking the max across each column to get maxvec
  maxvec = apply(totalvec, 2, max, na.rm = TRUE)
  
  return(maxvec)
}

#Function that returns the min "paragraph vector"
MinVec = function(review, model)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, stripDigits = FALSE, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  # create empty data frame
  totalvec = data.frame()
  
  #collecting each word vector into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(model))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(model[which(rownames(model) %in% i),],nrow=1)))
    }
  }
  
  #taking the min across each column to get minvec
  minvec = apply(totalvec, 2, min, na.rm = TRUE)
  
  return(minvec)
}

#Function that returns the average "paragraph vector" for top 30% idf
AverageVecTop30 = function(review, modelidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, stripDigits = FALSE, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  ## 1) collect each word and corresponding idf value, keep top 30%
  
  #create empty data frame
  wordidf = data.frame()
  
  #collecting each word idf into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(modelidf))
    {
      #keeping last 2 columns: word name and idf value
      wordidf = bind_rows(wordidf, modelidf[which(rownames(modelidf) %in% i),(ncol(modelidf)-1):ncol(modelidf)])
    }
  }
  
  wordidf = wordidf[order(wordidf$idf, decreasing = TRUE),]
  wordidf = wordidf[1:round((0.3*nrow(wordidf))),]
  
  ## 2) taking top 30 words and getting average vector
  
  #counter for number of words
  nwords = 0
  
  #empty vector
  totalvec = rep(0, length(colnames(modelidf)) - 2)
  
  #adding up each vector
  for (i in wordidf$rownames)
  {
    if (i %in% colnames(modelidf))
    {
      totalvec = totalvec + modelidf[,which(colnames(modelidf) %in% i)]
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  return(averagevec)
}

#Function that returns the max "paragraph vector" for top 30% idf
MaxVecTop30 = function(review, modelidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, stripDigits = FALSE, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  ## 1) collect each word and corresponding idf value, keep top 30%
  
  #create empty data frame
  wordidf = data.frame()
  
  #collecting each word idf into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(modelidf))
    {
      #keeping last 2 columns: word name and idf value
      wordidf = bind_rows(wordidf, modelidf[which(rownames(modelidf) %in% i),(ncol(modelidf)-1):ncol(modelidf)])
    }
  }
  
  wordidf = wordidf[order(wordidf$idf, decreasing = TRUE),]
  wordidf = wordidf[1:round((0.3*nrow(wordidf))),]
  
  ## 2) taking top 30 words and getting max vector
  
  # create empty data frame
  totalvec = data.frame()
  
  #collecting each word vector into a dataframe
  for (i in wordidf$rownames)
  {
    if (i %in% colnames(modelidf))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(modelidf[,which(colnames(modelidf) %in% i),],nrow=1)))
    }
  }
  
  #taking the max across each column to get maxvec
  maxvec = apply(totalvec, 2, max, na.rm = TRUE)
  
  return(maxvec)
}

#Function that returns the min "paragraph vector" for top 30% idf
MinVecTop30 = function(review, modelidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, stripDigits = FALSE, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  ## 1) collect each word and corresponding idf value, keep top 30%
  
  #create empty data frame
  wordidf = data.frame()
  
  #collecting each word idf into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(modelidf))
    {
      #keeping last 2 columns: word name and idf value
      wordidf = bind_rows(wordidf, modelidf[which(rownames(modelidf) %in% i),(ncol(modelidf)-1):ncol(modelidf)])
    }
  }
  
  wordidf = wordidf[order(wordidf$idf, decreasing = TRUE),]
  wordidf = wordidf[1:round((0.3*nrow(wordidf))),]
  
  ## 2) taking top 30 words and getting min vector
  
  # create empty data frame
  totalvec = data.frame()
  
  #collecting each word vector into a dataframe
  for (i in wordidf$rownames)
  {
    if (i %in% colnames(modelidf))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(modelidf[,which(colnames(modelidf) %in% i),],nrow=1)))
    }
  }
  
  #taking the min across each column to get minvec
  minvec = apply(totalvec, 2, min, na.rm = TRUE)
  
  return(minvec)
}

#Function that returns the average "paragraph vector" weighted by idf values
MeanIdf = function(review, modelidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, stripDigits = FALSE, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  #counter for number of words
  nwords = 0
  
  #empty vector
  totalvec = rep(0, length(colnames(modelidf)) - 2)
  
  #adding up each vector
  for (i in reviewWordVec)
  {
    if (i %in% colnames(modelidf))
    {
      totalvec = totalvec + (modelidf[,which(colnames(modelidf) %in% i)] * modelidf[which(rownames(modelidf) %in% i),ncol(modelidf)])
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  
  return(averagevec)
}

################## VECTORIZING TEST AND TRAIN ################################
#definitions:
#min/max - concatenating min and max vectors to get a vector with 2x the original dimensionality
#top 30% idf - sorts the words in a text based on their idf values, take mean/max/min of top 30%
#idf-weighted - weigh each word with corresponding idf value, then take the mean


#converting distance matrix to dataframe and appending rownames column for left join
modelidf = data.frame(model)
modelidf = cbind(model, data.frame(rownames(model), stringsAsFactors = FALSE))
colnames(modelidf)[501] = c("rownames")
modelidf = left_join(modelidf, unique(tfidf[,c(1,5)]), by = c("rownames" = "word"))

#1) MEAN

#results:
#OANC, 4000 train, 1000 test - train 0.8076, test 0.7964847

#Return mean paragraph vector for 4000 reviews in train
meantrainvec = data.frame()
for (i in 1:4000)
{
  meantrainvec = bind_rows(meantrainvec, data.frame(matrix(AverageVec(imdbtrain[i,2],model),nrow=1)))
}

#Return mean paragraph vector for all reviews in test
meantestvec = data.frame()
for (i in 1:1000)
{
  meantestvec = bind_rows(meantestvec, data.frame(matrix(AverageVec(imdbtest[i,2],model),nrow=1)))
}

#2) MAX

#results:
#OANC, 4000 train, 1000 test - train 0.7575, test 0.7645202

#Return max paragraph vector for 4000 reviews in train
maxtrainvec = data.frame()
for (i in 1:4000)
{
  maxtrainvec = bind_rows(maxtrainvec, data.frame(matrix(MaxVec(imdbtrain[i,2],model),nrow=1)))
}

#Return max paragraph vector for all reviews in test
maxtestvec = data.frame()
for (i in 1:1000)
{
  maxtestvec = bind_rows(maxtestvec, data.frame(matrix(MaxVec(imdbtest[i,2],model),nrow=1)))
}

#3) MIN

#results:
#OANC, 4000 trcain, 1000 test - train 0.786, 0.7800445

#Return min paragraph vector for 4000 reviews in train
mintrainvec = data.frame()
for (i in 1:4000)
{
  mintrainvec = bind_rows(mintrainvec, data.frame(matrix(MinVec(imdbtrain[i,2],model),nrow=1)))
}

#Return min paragraph vector for all reviews in test
mintestvec = data.frame()
for (i in 1:1000)
{
  mintestvec = bind_rows(mintestvec, data.frame(matrix(MinVec(imdbtest[i,2],model),nrow=1)))
}

#4) MIN/MAX

#results:
#OANC, 4000 train, 1000 test - train 0.7714, test 0.7590041

######################### LOGISTIC REGRESSION ################################

imdbtrainvec = mintrainvec + maxtrainvec
imdbtestvec = mintestvec + maxtestvec

#Fit model
library(glmnet)
NFOLDS = 4
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = as.matrix(imdbtrainvec), y = imdbtrain$sentiment[1:4000], 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 0, 
                              #lasso penalty
                              type.measure = "auc",
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds = predict(glmnet_classifier,as.matrix(imdbtestvec),type = 'response')[,1]
glmnet::auc(imdbtest$sentiment[1:1000], preds)

