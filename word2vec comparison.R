######################## PRE-PROCESSING FUNCTION ###########################
# Shell preprocesing function
preProcessDocument = function(document, stripPunc=TRUE, stripDigits = TRUE, lowercase=TRUE, stopWord=TRUE, stripExcessWhitespace = TRUE, lemmatize=FALSE, stem = FALSE)
{
  if(stripPunc) document = gsub("[^[:alpha:] ]", " ", document)
  if(stripDigits) document = gsub("[0-9]", " ", document)
  if(lowercase) document = tolower(document)
  if(stopWord) {
    stopwords1 = c("\\ba\\b",	"\\babout\\b",	"\\babove\\b",	"\\bacross\\b",	"\\bafter\\b",	"\\bagain\\b",	"\\bagainst\\b",	"\\ball\\b",	"\\balmost\\b",	"\\balone\\b",	"\\balong\\b",	"\\balready\\b",	"\\balso\\b",	"\\balthough\\b",	"\\balways\\b",	"\\bam\\b",	"\\bamong\\b",	"\\ban\\b",	"\\band\\b",	"\\banother\\b",	"\\bany\\b",	"\\banybody\\b",	"\\banyone\\b",	"\\banything\\b",	"\\banywhere\\b",	"\\bare\\b",	"\\barea\\b",	"\\bareas\\b",	"\\baren't\\b",	"\\baround\\b",	"\\bas\\b",	"\\bask\\b",	"\\basked\\b",	"\\basking\\b",	"\\basks\\b",	"\\bat\\b",	"\\baway\\b",	"\\bb\\b",	"\\bback\\b",	"\\bbacked\\b",	"\\bbacking\\b",	"\\bbacks\\b",	"\\bbe\\b",	"\\bbecame\\b",	"\\bbecause\\b",	"\\bbecome\\b",	"\\bbecomes\\b",	"\\bbeen\\b",	"\\bbefore\\b",	"\\bbegan\\b",	"\\bbehind\\b",	"\\bbeing\\b",	"\\bbeings\\b",	"\\bbelow\\b",	"\\bbest\\b",	"\\bbetter\\b",	"\\bbetween\\b",	"\\bbig\\b",	"\\bboth\\b",	"\\bbr\\b", "\\bbut\\b",	"\\bby\\b",	"\\bc\\b",	"\\bcame\\b",	"\\bcan\\b",	"\\bcannot\\b",	"\\bcan't\\b",	"\\bcase\\b",	"\\bcases\\b",	"\\bcertain\\b",	"\\bcertainly\\b",	"\\bclear\\b",	"\\bclearly\\b",	"\\bcome\\b",	"\\bcould\\b",	"\\bcouldn't\\b",	"\\bd\\b",	"\\bdid\\b",	"\\bdidn't\\b",	"\\bdiffer\\b",	"\\bdifferent\\b",	"\\bdifferently\\b",	"\\bdo\\b",	"\\bdoes\\b",	"\\bdoesn't\\b",	"\\bdoing\\b",	"\\bdone\\b",	"\\bdon't\\b",	"\\bdown\\b",	"\\bdowned\\b",	"\\bdowning\\b",	"\\bdowns\\b",	"\\bduring\\b",	"\\be\\b",	"\\beach\\b",	"\\bearly\\b",	"\\beither\\b",	"\\bend\\b",	"\\bended\\b",	"\\bending\\b",	"\\bends\\b",	"\\benough\\b",	"\\beven\\b",	"\\bevenly\\b",	"\\bever\\b",	"\\bevery\\b",	"\\beverybody\\b",	"\\beveryone\\b",	"\\beverything\\b",	"\\beverywhere\\b",	"\\bf\\b",	"\\bface\\b",	"\\bfaces\\b",	"\\bfact\\b",	"\\bfacts\\b",	"\\bfar\\b",	"\\bfelt\\b",	"\\bfew\\b",	"\\bfind\\b",	"\\bfinds\\b",	"\\bfirst\\b",	"\\bfor\\b",	"\\bfour\\b",	"\\bfrom\\b",	"\\bfull\\b",	"\\bfully\\b",	"\\bfurther\\b",	"\\bfurthered\\b",	"\\bfurthering\\b",	"\\bfurthers\\b",	"\\bg\\b",	"\\bgave\\b",	"\\bgeneral\\b",	"\\bgenerally\\b",	"\\bget\\b",	"\\bgets\\b",	"\\bgive\\b",	"\\bgiven\\b",	"\\bgives\\b",	"\\bgo\\b",	"\\bgoing\\b",	"\\bgood\\b",	"\\bgoods\\b",	"\\bgot\\b",	"\\bgreat\\b",	"\\bgreater\\b",	"\\bgreatest\\b",	"\\bgroup\\b",	"\\bgrouped\\b",	"\\bgrouping\\b",	"\\bgroups\\b",	"\\bh\\b",	"\\bhad\\b",	"\\bhadn't\\b",	"\\bhas\\b",	"\\bhasn't\\b",	"\\bhave\\b",	"\\bhaven't\\b",	"\\bhaving\\b",	"\\bhe\\b",	"\\bhe'd\\b",	"\\bhe'll\\b",	"\\bher\\b",	"\\bhere\\b",	"\\bhere's\\b",	"\\bhers\\b",	"\\bherself\\b",	"\\bhe's\\b",	"\\bhigh\\b",	"\\bhigher\\b",	"\\bhighest\\b",	"\\bhim\\b",	"\\bhimself\\b",	"\\bhis\\b",	"\\bhow\\b",	"\\bhowever\\b",	"\\bhow's\\b",	"\\bi\\b",	"\\bi'd\\b",	"\\bif\\b",	"\\bi'll\\b",	"\\bi'm\\b",	"\\bimportant\\b",	"\\bin\\b",	"\\binterest\\b",	"\\binterested\\b",	"\\binteresting\\b",	"\\binterests\\b",	"\\binto\\b",	"\\bis\\b",	"\\bisn't\\b",	"\\bit\\b",	"\\bits\\b",	"\\bit's\\b",	"\\bitself\\b",	"\\bi've\\b",	"\\bj\\b",	"\\bjust\\b",	"\\bk\\b",	"\\bkeep\\b",	"\\bkeeps\\b",	"\\bkind\\b",	"\\bknew\\b",	"\\bknow\\b",	"\\bknown\\b",	"\\bknows\\b",	"\\bl\\b",	"\\blarge\\b",	"\\blargely\\b")
    stopwords2 = c("\\blast\\b",	"\\blater\\b",	"\\blatest\\b",	"\\bleast\\b",	"\\bless\\b",	"\\blet\\b",	"\\blets\\b",	"\\blet's\\b",	"\\blike\\b",	"\\blikely\\b",	"\\blong\\b",	"\\blonger\\b",	"\\blongest\\b",	"\\bm\\b",	"\\bmade\\b",	"\\bmake\\b",	"\\bmaking\\b",	"\\bman\\b",	"\\bmany\\b",	"\\bmay\\b",	"\\bme\\b",	"\\bmember\\b",	"\\bmembers\\b",	"\\bmen\\b",	"\\bmight\\b",	"\\bmore\\b",	"\\bmost\\b",	"\\bmostly\\b",	"\\bmr\\b",	"\\bmrs\\b",	"\\bmuch\\b",	"\\bmust\\b",	"\\bmustn't\\b",	"\\bmy\\b",	"\\bmyself\\b",	"\\bn\\b", "\n", "\\bnecessary\\b",	"\\bneed\\b",	"\\bneeded\\b",	"\\bneeding\\b",	"\\bneeds\\b",	"\\bnever\\b",	"\\bnew\\b",	"\\bnewer\\b",	"\\bnewest\\b",	"\\bnext\\b",	"\\bno\\b",	"\\bnobody\\b",	"\\bnon\\b",	"\\bnoone\\b",	"\\bnor\\b",	"\\bnot\\b",	"\\bnothing\\b",	"\\bnow\\b",	"\\bnowhere\\b",	"\\bnumber\\b",	"\\bnumbers\\b",	"\\bo\\b",	"\\bof\\b",	"\\boff\\b",	"\\boften\\b",	"\\bold\\b",	"\\bolder\\b",	"\\boldest\\b",	"\\bon\\b",	"\\bonce\\b",	"\\bone\\b",	"\\bonly\\b",	"\\bopen\\b",	"\\bopened\\b",	"\\bopening\\b",	"\\bopens\\b",	"\\bor\\b")
    stopwords3 = c("\\border\\b",	"\\bordered\\b",	"\\bordering\\b",	"\\borders\\b",	"\\bother\\b",	"\\bothers\\b",	"\\bought\\b",	"\\bour\\b",	"\\bours\\b",	"\\bourselves\\b",	"\\bout\\b",	"\\bover\\b",	"\\bown\\b",	"\\bp\\b",	"\\bpart\\b",	"\\bparted\\b",	"\\bparting\\b",	"\\bparts\\b",	"\\bper\\b",	"\\bperhaps\\b",	"\\bplace\\b",	"\\bplaces\\b",	"\\bpoint\\b",	"\\bpointed\\b",	"\\bpointing\\b",	"\\bpoints\\b",	"\\bpossible\\b",	"\\bpresent\\b",	"\\bpresented\\b",	"\\bpresenting\\b",	"\\bpresents\\b",	"\\bproblem\\b",	"\\bproblems\\b",	"\\bput\\b",	"\\bputs\\b",	"\\bq\\b",	"\\bquite\\b",	"\\br\\b", "\r",	"\\brather\\b",	"\\breally\\b",	"\\bright\\b",	"\\broom\\b",	"\\brooms\\b",	"\\bs\\b",	"\\bsaid\\b",	"\\bsame\\b",	"\\bsaw\\b",	"\\bsay\\b",	"\\bsays\\b",	"\\bsecond\\b",	"\\bseconds\\b",	"\\bsee\\b",	"\\bseem\\b",	"\\bseemed\\b",	"\\bseeming\\b",	"\\bseems\\b",	"\\bsees\\b",	"\\bseveral\\b",	"\\bshall\\b",	"\\bshan't\\b",	"\\bshe\\b",	"\\bshe'd\\b",	"\\bshe'll\\b",	"\\bshe's\\b",	"\\bshould\\b",	"\\bshouldn't\\b",	"\\bshow\\b",	"\\bshowed\\b",	"\\bshowing\\b",	"\\bshows\\b",	"\\bside\\b",	"\\bsides\\b",	"\\bsince\\b",	"\\bsmall\\b",	"\\bsmaller\\b",	"\\bsmallest\\b",	"\\bso\\b",	"\\bsome\\b",	"\\bsomebody\\b",	"\\bsomeone\\b",	"\\bsomething\\b",	"\\bsomewhere\\b",	"\\bstate\\b",	"\\bstates\\b",	"\\bstill\\b",	"\\bsuch\\b",	"\\bsure\\b",	"\\bt\\b",	"\\btake\\b",	"\\btaken\\b",	"\\bthan\\b",	"\\bthat\\b",	"\\bthat's\\b",	"\\bthe\\b",	"\\btheir\\b",	"\\btheirs\\b",	"\\bthem\\b",	"\\bthemselves\\b",	"\\bthen\\b",	"\\bthere\\b",	"\\btherefore\\b",	"\\bthere's\\b",	"\\bthese\\b",	"\\bthey\\b",	"\\bthey'd\\b",	"\\bthey'll\\b",	"\\bthey're\\b",	"\\bthey've\\b",	"\\bthing\\b",	"\\bthings\\b",	"\\bthink\\b",	"\\bthinks\\b",	"\\bthis\\b",	"\\bthose\\b",	"\\bthough\\b",	"\\bthought\\b",	"\\bthoughts\\b",	"\\bthree\\b",	"\\bthrough\\b",	"\\bthus\\b",	"\\bto\\b",	"\\btoday\\b",	"\\btogether\\b",	"\\btoo\\b",	"\\btook\\b",	"\\btoward\\b",	"\\bturn\\b",	"\\bturned\\b",	"\\bturning\\b",	"\\bturns\\b",	"\\btwo\\b",	"\\bu\\b",	"\\bunder\\b",	"\\buntil\\b",	"\\bup\\b",	"\\bupon\\b",	"\\bus\\b",	"\\buse\\b",	"\\bused\\b",	"\\buses\\b",	"\\bv\\b",	"\\bvery\\b",	"\\bw\\b",	"\\bwant\\b",	"\\bwanted\\b",	"\\bwanting\\b",	"\\bwants\\b",	"\\bwas\\b",	"\\bwasn't\\b",	"\\bway\\b",	"\\bways\\b",	"\\bwe\\b",	"\\bwe'd\\b",	"\\bwell\\b",	"\\bwe'll\\b",	"\\bwells\\b",	"\\bwent\\b",	"\\bwere\\b",	"\\bwe're\\b",	"\\bweren't\\b",	"\\bwe've\\b",	"\\bwhat\\b",	"\\bwhat's\\b",	"\\bwhen\\b",	"\\bwhen's\\b",	"\\bwhere\\b",	"\\bwhere's\\b",	"\\bwhether\\b",	"\\bwhich\\b",	"\\bwhile\\b",	"\\bwho\\b",	"\\bwhole\\b",	"\\bwhom\\b",	"\\bwho's\\b",	"\\bwhose\\b",	"\\bwhy\\b",	"\\bwhy's\\b",	"\\bwill\\b",	"\\bwith\\b",	"\\bwithin\\b",	"\\bwithout\\b",	"\\bwon't\\b",	"\\bwork\\b",	"\\bworked\\b",	"\\bworking\\b",	"\\bworks\\b",	"\\bwould\\b",	"\\bwouldn't\\b",	"\\bx\\b",	"\\by\\b",	"\\byear\\b",	"\\byears\\b",	"\\byes\\b",	"\\byet\\b",	"\\byou\\b",	"\\byou'd\\b",	"\\byou'll\\b",	"\\byoung\\b",	"\\byounger\\b",	"\\byoungest\\b",	"\\byour\\b",	"\\byou're\\b",	"\\byours\\b",	"\\byourself\\b",	"\\byourselves\\b",	"\\byou've\\b",	"\\bz\\b")
    document = gsub(pattern = paste(stopwords1, collapse = "|"), " ", document, ignore.case = TRUE)
    document = gsub(pattern = paste(stopwords2, collapse = "|"), " ", document, ignore.case = TRUE)
    document = gsub(pattern = paste(stopwords3, collapse = "|"), " ", document, ignore.case = TRUE)
  }
  #replaces any space character(space, tab), or repeats of space characters with a space character
  if(stripExcessWhitespace) document = gsub("\\s+"," ",document)
  if(lemmatize & document != "" & document != " ") {
    tagged.results <- treetag(document, treetagger="manual", format="obj",
                              TT.tknz=FALSE , lang="en",TT.options=list(path="C:/TreeTagger", 
                                                                        preset="en", no.unknown = TRUE))    #path argument needs to be set manually to local download of TreeTagger
    document = paste(tagged.results@TT.res$lemma, collapse = " ")
  }
  if (stem & document != "" & document != " ") document = paste(wordStem(strsplit(document, split = " ")[[1]]), collapse = " ")
  return(document)
}
######################### PRE-LOAD + PRE-TRAIN OANC #####################################

## source: https://github.com/bmschmidt/wordVectors
## packages to install
# library(devtools)
#install_github("bmschmidt/wordVectors")

#loading packages
library(wordVectors)
library(stringi)
library(readr)
library(dplyr)

#prepare text for word2vec
#what it does:
#1) create single text file with contents of all documents
#2) clean and lowercase original text
#3) option to do n-grams (bundle_ngrams = ?)

setwd("C:/Users/Kenneth/Desktop/wordKernel/Datasets")
prep_word2vec(origin="OANC_subset_310",destination="word2vec_OANC_subset_310.txt",lowercase=T,bundle_ngrams=1)

#train model
if (!file.exists("word2vec_vectors.bin")) {model = train_word2vec("word2vec_OANC_subset_310.txt","word2vec_vectors.bin",vectors=500,threads=7,window=12,iter=5,negative_samples=5)} else model = read.vectors("word2vec_vectors.bin")

#train-test IMdb
setwd("C:/Users/Kenneth/Desktop/wordKernel")
imdbdata = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")
imdbtest = imdbdata[20001:24722,2:3]
imdbtrain = imdbdata[1:20000,2:3]

######################### PRE-LOAD + PRE-TRAIN IMDB #####################################

## source: https://github.com/bmschmidt/wordVectors
## packages to install
# library(devtools)
#install_github("bmschmidt/wordVectors")

#loading packages
library(wordVectors)
library(stringi)
library(readr)
library(dplyr)

#prepare text for word2vec
#what it does:
#1) create single text file with contents of all documents
#2) clean and lowercase original text
#3) option to do n-grams (bundle_ngrams = ?)

#IMdb
setwd("C:/Users/Kenneth/Desktop/wordKernel")
imdbdata = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")
imdbtest = imdbdata[20001:21000,2:3]
imdbtrain = imdbdata[1:4000,2:3]

#preparing IMDb documents for word2vec pre-training
imdbcorpus = imdbtrain$review
setwd("C:/Users/Kenneth/Desktop/wordKernel/Datasets/IMDb/word2vec")

for (i in 1:length(imdbcorpus))
{
  writeLines(preProcessDocument(imdbcorpus[i]),paste("documents/",i,".txt", sep =""))
}

prep_word2vec(origin="documents",destination="word2vec_prepped.txt",lowercase=T,bundle_ngrams=1)

#train model
if (!file.exists("word2vec_vectors.bin")) {model = train_word2vec("word2vec_prepped.txt","word2vec_vectors.bin",vectors=500,threads=7,window=12,iter=5)} else model = read.vectors("word2vec_vectors.bin")

## GET IDF VALUES OF WORD EMBEDDINGS

# create empty data frame
tfidf = data.frame() 

#just pre-train on 4000 train
imdbcorpus = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")
imdbcorpus = imdbcorpus$review[1:4000]

# populate data frame: each row corresponds to a word (non-unique) from the corpus
# word's document origin also included
for(i in 1:length(imdbcorpus)) {
  docstring <- strsplit(preProcessDocument(imdbcorpus[i], lemmatize = FALSE, stem = FALSE), " ")[[1]] 
  docstring <- docstring[which(nchar(docstring)>0)]
  docstring <- data.frame(matrix(docstring, ncol = 1), stringsAsFactors = FALSE)
  colnames(docstring) <- c("V1")
  doc <- docstring %>%
    unnest_tokens(word, V1) %>%
    cbind(data.frame(rep(i,nrow(.)))) %>%
    setNames(c("word","doc")) 
  tfidf = rbind(tfidf, doc)
}

# create tf-idf matrix for corpus
tfidf <- tfidf %>% 
  count(word, doc, sort = TRUE) %>% ## word count per document
  ungroup() %>%
  bind_tf_idf(word, doc, n)

########################## EXAMINING PRE-TRAINED VECTORS ############################

#Function that returns the cosine similarity of two vectors
CosSim = function(vector1, vector2)
{
  return((t(vector1) %*% (vector2))/sqrt(sum(vector1^2)*sum(vector2^2)))
}

## Returning cossine similarity for words
cossim_df = data.frame(apply(model, 1, CosSim, vector2 = as.vector(model[which(rownames(model) %in% rownames(model)[1]),], mode = "numeric")))
colnames(cossim_df) = rownames(model)[1]
for (i in rownames(model)[2:length(rownames(model))])
{
  cossim = data.frame(apply(model, 1, CosSim, vector2 = as.vector(model[which(rownames(model) %in% i),], mode = "numeric")))
  colnames(cossim) = i
  cossim_df = cbind(cossim_df, cossim)
}

## IMDb (4000 train) 

#movie
movie = t(cossim_df[which(rownames(cossim_df) %in% c("movie")),])
#top 10: movies, popcorn, segal, lungren, rgv, anytime, fingernails, commented, cringed, rainy
#for context: film is 0.2955446 (top 10 are 0.36 - 0.40)

#girl
girl = t(cossim_df[which(rownames(cossim_df) %in% c("girl")),])
#top 10: paulie, madly, meets, marie, boy, withdrawn, pauline, array, longed, salesman
#for context: brunette is 0.400378, blonde is 0.3895242, bride is 0.3645643, girls is 0.3542498 (top 10 are 0.40-0.54)

#positive
positive = t(cossim_df[which(rownames(cossim_df) %in% c("positive")),])
#top 10: message, revolting, reviews, comments, inaccuracies, critic, critique, glaring, criticised, hopefully
#for context: negative is 0.3832212, bravo is 0.3719596 (top 10 are 0.39 to 0.48)

#negative 
negative = t(cossim_df[which(rownames(cossim_df) %in% c("negative")),])
#top 10: coverage, imdb, inaccuracies, criticised, criticism, criticizing, proud, critique, reviews, user
#for context: positive is 0.3832212, (top 10 are 0.40 to 0.51)

#bad
bad = t(cossim_df[which(rownames(cossim_df) %in% c("bad")),])
#top 10: segal, criminally, horrible, tiresome, worst, snowman, terrible, awful, laughable, lousy
#for context: lots of other bad-synonyms in top 50 (top 10 are 0.41 - 0.47)

########################## PRE-LOADING FUNCTIONS ############################

#Function that returns the mean "paragraph vector"
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
      wordidf = bind_rows(wordidf, modelidf[which(rownames(modelidf) %in% i),c(1,ncol(modelidf))])
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
    if (i %in% rownames(modelidf))
    {
      totalvec = totalvec + modelidf[which(rownames(modelidf) %in% i),-c(1,ncol(modelidf))]
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  rownames(averagevec) = c()
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
      wordidf = bind_rows(wordidf, modelidf[which(rownames(modelidf) %in% i),c(1,ncol(modelidf))])
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
    if (i %in% rownames(modelidf))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(modelidf[which(rownames(modelidf) %in% i),-c(1,ncol(modelidf))],nrow=1)))
    }
  }
  totalvec = data.frame(matrix(unlist(totalvec), ncol=500, byrow=F))
  
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
      wordidf = bind_rows(wordidf, modelidf[which(rownames(modelidf) %in% i),c(1,ncol(modelidf))])
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
    if (i %in% rownames(modelidf))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(modelidf[which(rownames(modelidf) %in% i),-c(1,ncol(modelidf))],nrow=1)))
    }
  }
  totalvec = data.frame(matrix(unlist(totalvec), ncol=500, byrow=F))
  
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
  totalvec = rep(0, ncol(modelidf) - 2)
  
  #adding up each vector
  for (i in reviewWordVec)
  {
    if (i %in% rownames(modelidf))
    {
      totalvec = totalvec + (modelidf[which(rownames(modelidf) %in% i),-c(1,ncol(modelidf))] * modelidf[which(rownames(modelidf) %in% i),ncol(modelidf)])
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  rownames(averagevec) = c()
  return(averagevec)
}

################## VECTORIZING TEST AND TRAIN ################################
#definitions:
#min/max - concatenating min and max vectors to get a vector with 2x the original dimensionality
#top 30% idf - sorts the words in a text based on their idf values, take mean/max/min of top 30%
#idf-weighted - weigh each word with corresponding idf value, then take the mean


#converting distance matrix to dataframe and appending rownames column for left join
modelidf = data.frame(matrix(model, nrow = 10706, ncol = 500))
modelidf = cbind(data.frame(rownames(model), stringsAsFactors = FALSE), modelidf)
colnames(modelidf)[1] = c("rownames")
modelidf = left_join(modelidf, unique(tfidf[,c(1,5)]), by = c("rownames" = "word"))
rownames(modelidf) = rownames(model)
modelidf = modelidf[,-1]

#1) MEAN

#results:
#OANC, 4000 train, 1000 test - train 0.8076, test 0.7964847
#IMDb (4000 train), 4000 train, 1000 test - lasso max cv 0.9227, train 0.9424, test 0.904 

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
#IMDb (4000 train), 4000 train, 1000 test - lasso max cv 0.8756, AUC 0.9109, test 0.8498

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
#IMDb (4000 train), 4000 train, 1000 test - lasso max cv 0.8713, AUC 0.9041, test 0.8541

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
#IMDb (4000 train), 4000 train, 1000 test - lasso max cv 0.8826, AUC 0.9179, test 0.8621

#5) MEAN, TOP 30% IDF

#results:
#IMDb (4000 train), 4000 train, 1000 test - lasso max cv 0.8681, AUC 0.8938, test 0.78846

##Return min paragraph vector for 4000 reviews in train
meantop30trainvec = data.frame()
for (i in 1:4000)
{
  meantop30trainvec = bind_rows(meantop30trainvec, data.frame(matrix(AverageVecTop30(imdbtrain[i,2],modelidf),nrow=1)))
}
meantop30trainvec = data.frame(matrix(unlist(meantop30trainvec), nrow=4000, byrow=F))

#Return min paragraph vector for all reviews in test
meantop30testvec = data.frame()
for (i in 1:1000)
{
  meantop30testvec = bind_rows(meantop30testvec, data.frame(matrix(AverageVecTop30(imdbtest[i,2],modelidf),nrow=1)))
}
meantop30testvec = data.frame(matrix(unlist(meantop30testvec), nrow=1000, byrow=F))

#6) MAX, TOP 30% IDF

#results:
#IMDb (4000 train), 4000 train, 1000 test - lasso max cv 0.8289, AUC 0.8647, test 0.7728

#Return max top 30% idf paragraph vector for 4000 reviews in train
maxtop30trainvec = data.frame()
for (i in 1:4000)
{
  maxtop30trainvec = bind_rows(maxtop30trainvec, data.frame(matrix(MaxVecTop30(imdbtrain[i,2],modelidf),nrow=1)))
}
maxtop30trainvec = data.frame(matrix(unlist(maxtop30trainvec), nrow=4000, byrow=F))

#Return min paragraph vector for all reviews in test
maxtop30testvec = data.frame()
for (i in 1:1000)
{
  maxtop30testvec = bind_rows(maxtop30testvec, data.frame(matrix(MaxVecTop30(imdbtest[i,2],modelidf),nrow=1)))
}
maxtop30testvec = data.frame(matrix(unlist(maxtop30testvec), nrow=1000, byrow=F))

#7) MIN, TOP 30% IDF

#results:
#IMDB (4000 train), 4000 train, 1000 test - lasso max cv 0.8311, AUC 0.8657, test 0.7765

#Return min top 30% idf paragraph vector for 4000 reviews in train
mintop30trainvec = data.frame()
for (i in 1:4000)
{
  mintop30trainvec = bind_rows(mintop30trainvec, data.frame(matrix(MinVecTop30(imdbtrain[i,2],modelidf),nrow=1)))
}

#Return min top 30% idf paragraph vector for all reviews in test
mintop30testvec = data.frame()
for (i in 1:1000)
{
  mintop30testvec = bind_rows(mintop30testvec, data.frame(matrix(MinVecTop30(imdbtest[i,2],modelidf),nrow=1)))
}

#8) MIN/MAX, TOP 30% IDF

#results: 
#IMDB (4000 train), 4000 train, 1000 test - lasso max cv 0.8391, AUC 0.8679, test 0.7724

#9) MEAN, IDF-WEIGHTED

#results:
# IMDB (4000 train), 4000 train, 1000 test - lasso max cv 0.9140, AUC 0.9338, test 0.8893

#Return mean idf paragraph vector for 4000 reviews in train
meanidftrainvec = data.frame()
for (i in 1:4000)
{
  meanidftrainvec = bind_rows(meanidftrainvec, data.frame(matrix(MeanIdf(imdbtrain[i,2],modelidf),nrow=1)))
}
meanidftrainvec = data.frame(matrix(unlist(meanidftrainvec), nrow=4000, byrow=F))

#Return mean idf paragraph vector for all reviews in test
meanidftestvec = data.frame()
for (i in 1:1000)
{
  meanidftestvec = bind_rows(meanidftestvec, data.frame(matrix(MeanIdf(imdbtest[i,2],modelidf),nrow=1)))
}
meanidftestvec = data.frame(matrix(unlist(meanidftestvec), nrow=1000, byrow=F))

######################### LOGISTIC REGRESSION ################################

imdbtrainvec = meanidftrainvec
imdbtestvec = meanidftestvec

#10-fold CV to obtain optimal lambda for ridge)
cv_model = cv.glmnet(x = as.matrix(imdbtrainvec), y = imdbtrain$sentiment[1:4000], 
                     family = 'binomial',
                     #supplying sequence of lambdas from 10^-3 to 10^10
                     lambda = 10^seq(10,-3,length=300),
                     # L2 penalty (ridge), alpha = 0
                     alpha = 0, 
                     #auc used as loss measure for cross-validation
                     type.measure = "auc",
                     # 10-fold cross-validation
                     nfolds = 10)
plot(cv_model)
print(paste("max AUC for train =", max(cv_model$cvm)))
min_lambda = cv_model$lambda.min

#fit lasso log model with optimal lambda
logmodel = glmnet(x = as.matrix(imdbtrainvec), y = imdbtrain$sentiment[1:4000], 
                  family = 'binomial',
                  lambda = min_lambda,
                  alpha = 0)

preds = predict(logmodel, newx = as.matrix(imdbtrainvec), type = "response")[,1]
roccurve <- roc(imdbtrain$sentiment[1:4000] ~ preds)
plot(roccurve)
auc(roccurve)

#producing classification matrix and AUC for test
preds.test = predict(logmodel, newx = as.matrix(imdbtestvec), type = "response")[,1]
preds.test.labels = rep("0_predicted", length(preds.test))
preds.test.labels[preds.test > 0.5] = "1_predicted"
table(preds.test.labels, imdbtest$sentiment)
glmnet::auc(imdbtest$sentiment[1:1000],preds.test)
