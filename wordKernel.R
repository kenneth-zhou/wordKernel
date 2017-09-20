############################ LIBRARIES #####################################

require(signal) # Convolution
require(readr) # Read text file
require(utils) # txtProgressBar
require(SnowballC) #stemming
require(koRpus) #lemmatizing
library(dplyr)
library(tidytext)

############################## WORKING DIRECTORY ###########################

setwd("C:/Users/Kenneth/Desktop/wordKernel")

########################## PRE-LOADING FUNCTIONS ############################

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

# Process a document in the corpus.
processDocument = function(document, splitBySentences=TRUE, bscale=8, 
                           distanceMatrix, scalingMatrix, rareWordCorrection=FALSE)
{
  document = gsub("[\r\n]", " ", document) 
  
  # We have a single document, let's distance it
  
  # Pre-process document here
  wordVec = strsplit(preProcessDocument(document, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  # Strip out unique words to make loops more efficient
  uniqueWordVec = sort(unique(wordVec))
  # Remove empty words
  uniqueWordVec = uniqueWordVec[which(nchar(uniqueWordVec)>0)]
  
  #remove below: no longer neccesary because of minwordprevalence:
  # if(length(setdiff(uniqueWordVec, colnames(distanceMatrix))))
  # {
  #   stop("Word appears in individual document that did not appear in Corpus. Stopwording error?")
  # }
  
  print(paste0("Convolving document. Document length ", length(uniqueWordVec), " unique words and ", length(wordVec), " total words"))
  fullCorpusWordSet = colnames(distanceMatrix)
  # Base for stick representations of stuff we will convolve
  baseVector = rep(0,length(wordVec))
  smooth_gaussian=sapply(X = seq(-5,5), function(x) exp(-x^2/bscale))
  get_smooth_len = length(conv(rep(1,length(wordVec)), smooth_gaussian))
  # Stub 0-vector for anything that doesn't appear in the document
  stub = rep(0,get_smooth_len)
  
  initialSmoothing = matrix(NA, ncol=length(fullCorpusWordSet), nrow=get_smooth_len)
  pb = txtProgressBar(min=1,max=length(fullCorpusWordSet),initial=1, style=3)
  for(i in seq(1, length(fullCorpusWordSet)))
  {
    word = fullCorpusWordSet[i]
    if(!(word %in% uniqueWordVec)) { initialSmoothing[,i] = stub }
    else 
    { 
      stick = baseVector
      stick[which(wordVec==word)]=1
      initialSmoothing[,i] = conv(stick, smooth_gaussian) 
    }
    setTxtProgressBar(pb, i)
  }
  
  cat("\n")
  print("Done initial convolution, now adding document-level distance matrix")
  kTop = t(initialSmoothing) %*% initialSmoothing
  
  # Add the new distanceMatrix to the old one, element-wise
  distanceMatrix = distanceMatrix + kTop
  
  if(rareWordCorrection)
  {
    print("Rare word correction applied, calculating scaling factor matrix.")
    kBottom = sqrt(diag(kTop))
    pb = txtProgressBar(min=1,max=length(uniqueWordVec)-1,initial=1, style=3)
    for(i in seq(1, length(uniqueWordVec)-1))
    {
      setTxtProgressBar(pb, i)
      w1 = uniqueWordVec[i][1]
      for(j in seq(i, length(uniqueWordVec)))
      {
        w2 = uniqueWordVec[j][1]
        # Add scaling inner product to denominator.
        scalingMatrix[w1, w2] = scalingMatrix[w1, w2] + (kBottom[i] %*% kBottom[j])
      }
    }
    cat("\n")
  }
  
  # Return distance Matrix
  return(list("distanceMatrix"=distanceMatrix, "scalingMatrix"=scalingMatrix))
}

processCorpus = function(corpus, bscale=8, minwordprevalence=1, rareWordCorrection=FALSE)
{
  print("Prepping corpus...")
  
  # Pre-allocate matrix so we never need to resize it again
  # First, read all documents into a string
  corpusString = ""
  for(document in corpus) { corpusString = paste(corpusString, sep = " ", read_file(document)) }
  corpusString = gsub("[\r\n]", " ", corpusString)
  corpusString = gsub("\\. "," ", corpusString)
  corpusWordVec = strsplit(preProcessDocument(corpusString, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  rm(corpusString) #removes corpusString (no longer needed)
  corpusWordVec = names(table(corpusWordVec))[table(corpusWordVec) >= minwordprevalence]
  corpusUniqueWordVec = sort(unique(corpusWordVec))
  rm(corpusWordVec) #removes corpusWordVec (no longer needed)
  corpusUniqueWordVec = corpusUniqueWordVec[which(nchar(corpusUniqueWordVec)>0)] #removes empty words
  
  # Now, create the corpus distance matrix, and the corpus co-occurrence matrix
  distanceMatrix = matrix(0, nrow=length(corpusUniqueWordVec), ncol=length(corpusUniqueWordVec))
  
  
  colnames(distanceMatrix) = corpusUniqueWordVec 
  
  rownames(distanceMatrix) = corpusUniqueWordVec
  scalingMatrix = distanceMatrix
  
  print("Corpus Distance Matrix ready...")
  
  # Iterate through Corpus filename
  for(document in corpus)
  {
    print(paste0("Reading document ", document))
    # Need to robustify file read, but for now just assume it's a text and read using readr
    docString = read_file(document)
    result = processDocument(docString, bscale=bscale, 
                             distanceMatrix=distanceMatrix, scalingMatrix=scalingMatrix,
                             rareWordCorrection=rareWordCorrection)
    
    # Back-copy the results to feed forward to the next document
    distanceMatrix = result$distanceMatrix
    scalingMatrix = result$scalingMatrix
  }
  print("Read all corpus distances, now scaling...")
  
  if(rareWordCorrection) 
  {
    scalingMatrix = scalingMatrix + t(scalingMatrix)
    scalingMatrix[scalingMatrix==0] = 1
    distanceMatrix = distanceMatrix / scalingMatrix
  }
  else
  {
    scalingVec = unname(sqrt(diag(distanceMatrix)))
    newScalingMatrix = scalingVec %*% t(scalingVec)
    distanceMatrix = distanceMatrix / newScalingMatrix
  }
  
  # Return results
  return(list("distanceMatrix" = distanceMatrix, "scalingMatrix" = scalingMatrix, 
              "uniqueWords" = corpusUniqueWordVec, 
              "method" = ifelse(rareWordCorrection, "Rare Word Correction", "Standard")))
}

processCorpuslocal = function(corpus, bscale=8, minwordprevalence=1, rareWordCorrection=FALSE)
{
  print("Prepping corpus...")
  
  # Pre-allocate matrix so we never need to resize it again
  # First, read all documents into a string
  corpusString = ""
  for(document in corpus) { corpusString = paste(corpusString, sep = " ", document) }
  corpusString = gsub("[\r\n]", " ", corpusString)
  corpusString = gsub("\\. "," ", corpusString)
  corpusWordVec = strsplit(preProcessDocument(corpusString, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  rm(corpusString) #removes corpusString (no longer needed)
  corpusWordVec = names(table(corpusWordVec))[table(corpusWordVec) >= minwordprevalence]
  corpusUniqueWordVec = sort(unique(corpusWordVec))
  rm(corpusWordVec) #removes corpusWordVec (no longer needed)
  corpusUniqueWordVec = corpusUniqueWordVec[which(nchar(corpusUniqueWordVec)>0)] #removes empty words
  
  # Now, create the corpus distance matrix, and the corpus co-occurrence matrix
  distanceMatrix = matrix(0, nrow=length(corpusUniqueWordVec), ncol=length(corpusUniqueWordVec))
  
  
  colnames(distanceMatrix) = corpusUniqueWordVec 
  
  rownames(distanceMatrix) = corpusUniqueWordVec
  scalingMatrix = distanceMatrix
  
  print("Corpus Distance Matrix ready...")
  
  # Iterate through Corpus filename
  for(document in corpus)
  {
    docString = document
    result = processDocument(docString, bscale=bscale, 
                             distanceMatrix=distanceMatrix, scalingMatrix=scalingMatrix,
                             rareWordCorrection=rareWordCorrection)
    
    # Back-copy the results to feed forward to the next document
    distanceMatrix = result$distanceMatrix
    scalingMatrix = result$scalingMatrix
  }
  print("Read all corpus distances, now scaling...")
  
  if(rareWordCorrection) 
  {
    scalingMatrix = scalingMatrix + t(scalingMatrix)
    scalingMatrix[scalingMatrix==0] = 1
    distanceMatrix = distanceMatrix / scalingMatrix
  }
  else
  {
    scalingVec = unname(sqrt(diag(distanceMatrix)))
    newScalingMatrix = scalingVec %*% t(scalingVec)
    distanceMatrix = distanceMatrix / newScalingMatrix
  }
  
  # Return results
  return(list("distanceMatrix" = distanceMatrix, "scalingMatrix" = scalingMatrix, 
              "uniqueWords" = corpusUniqueWordVec, 
              "method" = ifelse(rareWordCorrection, "Rare Word Correction", "Standard")))
}

#Function that returns the average "paragraph vector"
AverageVec = function(review, distanceMatrix)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  #counter for number of words
  nwords = 0
  
  #empty vector
  totalvec = rep(0, length(colnames(distanceMatrix)))
  
  #adding up each vector
  for (i in reviewWordVec)
  {
    if (i %in% colnames(distanceMatrix))
    {
      totalvec = totalvec + distanceMatrix[,which(colnames(distanceMatrix) %in% i)]
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  
  return(averagevec)
}

#Function that returns the max "paragraph vector"
MaxVec = function(review, distanceMatrix)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  # create empty data frame
  totalvec = data.frame()
  
  #collecting each word vector into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% colnames(distanceMatrix))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(distanceMatrix[,which(colnames(distanceMatrix) %in% i),],nrow=1)))
    }
  }
  
  #taking the max across each column to get maxvec
  maxvec = apply(totalvec, 2, max, na.rm = TRUE)
  
  return(maxvec)
}

#Function that returns the min "paragraph vector"
MinVec = function(review, distanceMatrix)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  # create empty data frame
  totalvec = data.frame()
  
  #collecting each word vector into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% colnames(distanceMatrix))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(distanceMatrix[,which(colnames(distanceMatrix) %in% i),],nrow=1)))
    }
  }
  
  #taking the min across each column to get minvec
  minvec = apply(totalvec, 2, min, na.rm = TRUE)
  
  return(minvec)
}

#Function that returns the average "paragraph vector" for top 30% idf
AverageVecTop30 = function(review, distancematrixidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  ## 1) collect each word and corresponding idf value, keep top 30%
  
  #create empty data frame
  wordidf = data.frame()
  
  #collecting each word idf into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(distancematrixidf))
    {
      #keeping last 2 columns: word name and idf value
      wordidf = bind_rows(wordidf, distancematrixidf[which(rownames(distancematrixidf) %in% i),(ncol(distancematrixidf)-1):ncol(distancematrixidf)])
    }
  }
  
  wordidf = wordidf[order(wordidf$idf, decreasing = TRUE),]
  wordidf = wordidf[1:round((0.3*nrow(wordidf))),]
  
  ## 2) taking top 30 words and getting average vector
  
  #counter for number of words
  nwords = 0
  
  #empty vector
  totalvec = rep(0, length(colnames(distancematrixidf)) - 2)
  
  #adding up each vector
  for (i in wordidf$rownames)
  {
    if (i %in% colnames(distancematrixidf))
    {
      totalvec = totalvec + distancematrixidf[,which(colnames(distancematrixidf) %in% i)]
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  return(averagevec)
}

#Function that returns the max "paragraph vector" for top 30% idf
MaxVecTop30 = function(review, distancematrixidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  ## 1) collect each word and corresponding idf value, keep top 30%
  
  #create empty data frame
  wordidf = data.frame()
  
  #collecting each word idf into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(distancematrixidf))
    {
      #keeping last 2 columns: word name and idf value
      wordidf = bind_rows(wordidf, distancematrixidf[which(rownames(distancematrixidf) %in% i),(ncol(distancematrixidf)-1):ncol(distancematrixidf)])
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
    if (i %in% colnames(distancematrixidf))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(distancematrixidf[,which(colnames(distancematrixidf) %in% i),],nrow=1)))
    }
  }
  
  #taking the max across each column to get maxvec
  maxvec = apply(totalvec, 2, max, na.rm = TRUE)
  
  return(maxvec)
}

#Function that returns the min "paragraph vector" for top 30% idf
MinVecTop30 = function(review, distancematrixidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  ## adding each word vector row-wise to a dataframe
  
  ## 1) collect each word and corresponding idf value, keep top 30%
  
  #create empty data frame
  wordidf = data.frame()
  
  #collecting each word idf into a dataframe
  for (i in reviewWordVec)
  {
    if (i %in% rownames(distancematrixidf))
    {
      #keeping last 2 columns: word name and idf value
      wordidf = bind_rows(wordidf, distancematrixidf[which(rownames(distancematrixidf) %in% i),(ncol(distancematrixidf)-1):ncol(distancematrixidf)])
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
    if (i %in% colnames(distancematrixidf))
    {
      totalvec = bind_rows(totalvec, data.frame(matrix(distancematrixidf[,which(colnames(distancematrixidf) %in% i),],nrow=1)))
    }
  }
  
  #taking the min across each column to get minvec
  minvec = apply(totalvec, 2, min, na.rm = TRUE)
  
  return(minvec)
}

#Function that returns the average "paragraph vector" weighted by idf values
MeanIdf = function(review, distancematrixidf)
{
  #pre-process and convert to vector of words
  reviewWordVec = strsplit(preProcessDocument(review, lemmatize = FALSE, stem = FALSE), " ")[[1]]
  
  #counter for number of words
  nwords = 0
  
  #empty vector
  totalvec = rep(0, length(colnames(distancematrixidf)) - 2)
  
  #adding up each vector
  for (i in reviewWordVec)
  {
    if (i %in% colnames(distancematrixidf))
    {
      totalvec = totalvec + (distancematrixidf[,which(colnames(distancematrixidf) %in% i)] * distancematrixidf[which(rownames(distancematrixidf) %in% i),ncol(distancematrixidf)])
      nwords = nwords + 1 
    }
  }
  
  #average vector
  averagevec = totalvec/nwords
  
  return(averagevec)
}

###################### PRE-TRAIN WORD EMBEDDINGS ON OANC #################################

## PRE-TRAIN WORD EMBEDDINGS ON OANC

# obtain list of all corpus's file names
files <- list.files(path="Datasets/OANC_subset_310", pattern="*.txt", full.names=T, recursive=FALSE)
#Processing corpus
result = processCorpus(files, minwordprevalence =)

## GET IDF VALUES OF WORD EMBEDDINGS

# create empty data frame
tfidf = data.frame() 

# populate data frame: each row corresponds to a word (non-unique) from the corpus
# word's document origin also included
for(document in files) {
  docstring <- read_file(document) %>%
              gsub("[\r\n]", " ", .) %>%
              gsub("\\. "," ", .)
  docstring <- strsplit(preProcessDocument(docstring, lemmatize = FALSE, stem = FALSE), " ")[[1]] 
  docstring <- docstring[which(nchar(docstring)>0)]
  docstring <- data.frame(matrix(docstring, ncol = 1), stringsAsFactors = FALSE)
  colnames(docstring) <- c("V1")
  doc <- docstring %>%
      unnest_tokens(word, V1) %>%
      cbind(data.frame(rep(document,nrow(.)))) %>%
      setNames(c("word","doc")) 
  tfidf = rbind(tfidf, doc)
}

# create tf-idf matrix for corpus
tfidf <- tfidf %>% 
      count(word, doc, sort = TRUE) %>% ## word count per document
      ungroup() %>%
      bind_tf_idf(word, doc, n)

###################### PRE-TRAIN WORD EMBEDDINGS ON IMDB #################################

## PRE-TRAIN WORD EMBEDDINGS ON IMDB

# read all IMDb reviews
#all available data
#imdb1 = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")
#imdb2 = read_tsv("Datasets/IMDb/IMDBunlabeledtrain.tsv")
#imdb3 = read_tsv("Datasets/IMDb/IMDBtest.tsv")
#imdbcorpus = rbind(imdb1[,3],imdb2[,2],imdb3[,2])
#rm(imdb1)
#rm(imdb2)
#rm(imdb3)

#just pre-train on 4000 train
imdbcorpus = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")
imdbcorpus = imdbcorpus$review[1:4000]
result = processCorpuslocal(imdbcorpus, minwordprevalence = 10)

## GET IDF VALUES OF WORD EMBEDDINGS

# create empty data frame
tfidf = data.frame() 

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


################## TOKENIZE TEST AND TRAIN DATA ################################

#reading data
imdbdata = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")

#subsetting the first 4000
imdbtrain = imdbdata[1:4000,2:3]
imdbtest = imdbdata[20001:21000,2:3]

################## VECTORIZING TEST AND TRAIN ################################
#definitions:
#min/max - concatenating min and max vectors to get a vector with 2x the original dimensionality
#top 30% idf - sorts the words in a text based on their idf values, take mean/max/min of top 30%
#idf-weighted - weigh each word with corresponding idf value, then take the mean

#converting distance matrix to dataframe and appending rownames column for left join
distancematrix = data.frame(result$distanceMatrix)
distancematrixidf = cbind(distancematrix, data.frame(rownames(distancematrix), stringsAsFactors = FALSE))
colnames(distancematrixidf)[ncol(distancematrixidf)] = c("rownames")
distancematrixidf = left_join(distancematrixidf, unique(tfidf[,c(1,5)]), by = c("rownames" = "word"))
rownames(distancematrixidf) = colnames(distancematrixidf)[1:(ncol(distancematrixidf)-2)]

#1) MEAN

#results:
#OANC, 4000 train, 1000 test - train 0.8338, test 0.8315533
#IMDB (4000 train), 4000 train, 1000 test - train 0.9238, test 0.9102906

#Return mean paragraph vector for 4000 reviews in train
meantrainvec = data.frame()
for (i in 1:4000)
{
  meantrainvec = bind_rows(meantrainvec, data.frame(matrix(AverageVec(imdbtrain[i,2],data.frame(result$distanceMatrix)),nrow=1)))
}
colnames(meantrainvec) = colnames(result$distanceMatrix)

#Return mean paragraph vector for all reviews in test
meantestvec = data.frame()
for (i in 1:1000)
{
  meantestvec = bind_rows(meantestvec, data.frame(matrix(AverageVec(imdbtest[i,2],data.frame(result$distanceMatrix)),nrow=1)))
}
colnames(meantestvec) = colnames(result$distanceMatrix)

#2) MAX

#results:
#OANC, 4000 train, 1000 test - train 0.8447, test 0.8497296
#IMDB (4000 train), 4000 train, 1000 test - train 0.9208, test 0.9098346

#Return max paragraph vector for 4000 reviews in train
maxtrainvec = data.frame()
for (i in 1:4000)
{
  maxtrainvec = bind_rows(maxtrainvec, data.frame(matrix(MaxVec(imdbtrain[i,2],data.frame(result$distanceMatrix)),nrow=1)))
}
colnames(maxtrainvec) = colnames(result$distanceMatrix)

#Return max paragraph vector for all reviews in test
maxtestvec = data.frame()
for (i in 1:1000)
{
  maxtestvec = bind_rows(maxtestvec, data.frame(matrix(MaxVec(imdbtest[i,2],data.frame(result$distanceMatrix)),nrow=1)))
}
colnames(maxtestvec) = colnames(result$distanceMatrix)

#3) MIN

#results:
#OANC, 4000 train, 1000 test - train 0.8245, test 0.8270452

#Return min paragraph vector for 4000 reviews in train
mintrainvec = data.frame()
for (i in 1:4000)
{
  mintrainvec = bind_rows(mintrainvec, data.frame(matrix(MinVec(imdbtrain[i,2],data.frame(result$distanceMatrix)),nrow=1)))
}
colnames(mintrainvec) = colnames(result$distanceMatrix)

#Return min paragraph vector for all reviews in test
mintestvec = data.frame()
for (i in 1:1000)
{
  mintestvec = bind_rows(mintestvec, data.frame(matrix(MinVec(imdbtest[i,2],data.frame(result$distanceMatrix)),nrow=1)))
}
colnames(mintestvec) = colnames(result$distanceMatrix)

#4) MIN/MAX

#results:
#OANC, 4000 train, 1000 test - train 0.8386, test 0.8497296

#5) MEAN, TOP 30% IDF

#results:
#OANC, 4000 train, 1000 test - train 0.7136, test 0.7081833

#Return average top 30% idf paragraph vector for 4000 reviews in train
meantop30trainvec = data.frame()
for (i in 1:4000)
{
  meantop30trainvec = bind_rows(meantop30trainvec, data.frame(matrix(AverageVecTop30(imdbtrain[i,2],distancematrixidf),nrow=1)))
}
colnames(meantop30trainvec) = colnames(result$distanceMatrix)

#Return average top 30% idf paragraph vector for all reviews in test
meantop30testvec = data.frame()
for (i in 1:1000)
{
  meantop30testvec = bind_rows(meantop30testvec, data.frame(matrix(AverageVecTop30(imdbtest[i,2],distancematrixidf),nrow=1)))
}
colnames(meantop30testvec) = colnames(result$distanceMatrix)

#6) MAX, TOP 30% IDF

#results:
#OANC, 4000 train, 1000 test - train 0.7305, test 0.7147674

#Return max top 30% idf paragraph vector for 4000 reviews in train
maxtop30trainvec = data.frame()
for (i in 1:4000)
{
  maxtop30trainvec = bind_rows(maxtop30trainvec, data.frame(matrix(MaxVecTop30(imdbtrain[i,2],distancematrixidf),nrow=1)))
}
colnames(maxtop30trainvec) = colnames(result$distanceMatrix)

#Return max top 30% idf paragraph vector for all reviews in test
maxtop30testvec = data.frame()
for (i in 1:1000)
{
  maxtop30testvec = bind_rows(maxtop30testvec, data.frame(matrix(MaxVecTop30(imdbtest[i,2],distancematrixidf),nrow=1)))
}
colnames(maxtop30testvec) = colnames(result$distanceMatrix)

#7) MIN, TOP 30% IDF

#results:
#OANC, 4000 train, 1000 test - train 0.6852, test 0.6665787

#Return min top 30% idf paragraph vector for 4000 reviews in train
mintop30trainvec = data.frame()
for (i in 1:4000)
{
  mintop30trainvec = bind_rows(mintop30trainvec, data.frame(matrix(MinVecTop30(imdbtrain[i,2],distancematrixidf),nrow=1)))
}
colnames(mintop30trainvec) = colnames(result$distanceMatrix)

#Return min top 30% idf paragraph vector for all reviews in test
mintop30testvec = data.frame()
for (i in 1:1000)
{
  mintop30testvec = bind_rows(mintop30testvec, data.frame(matrix(MinVecTop30(imdbtest[i,2],distancematrixidf),nrow=1)))
}
colnames(mintop30testvec) = colnames(result$distanceMatrix)

#8) MIN/MAX, TOP 30% IDF

#results: 
#OANC, 4000 train, 1000 test - train 0.7287, test 0.7112514

#9) MEAN, IDF-WEIGHTED

#results:
#OANC, 4000 train, 1000 test - train 0.8335, test 0.8411735

#Return mean idf paragraph vector for 4000 reviews in train
meanidftrainvec = data.frame()
for (i in 1:4000)
{
  meanidftrainvec = bind_rows(meanidftrainvec, data.frame(matrix(MeanIdf(imdbtrain[i,2],distancematrixidf),nrow=1)))
}
colnames(meanidftrainvec) = colnames(result$distanceMatrix)

#Return mean idf paragraph vector for all reviews in test
meanidftestvec = data.frame()
for (i in 1:1000)
{
  meanidftestvec = bind_rows(meanidftestvec, data.frame(matrix(MeanIdf(imdbtest[i,2],distancematrixidf),nrow=1)))
}
colnames(meanidftestvec) = colnames(result$distanceMatrix)

######################### LOGISTIC REGRESSION ################################

imdbtrainvec = maxtrainvec
imdbtestvec = maxtestvec

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
print(difftime(Sys.time(), t1, units = 'sec'))6

plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

preds = predict(glmnet_classifier,as.matrix(imdbtestvec),type = 'response')[,1]
glmnet::auc(imdbtest$sentiment[1:1000], preds)
