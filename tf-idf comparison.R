library(text2vec)
library(data.table)
library(readr)
library(SnowballC)
library(koRpus)
library(glmnet)
library(glmnet)
library(pROC)
setwd("C:/Users/Kenneth/Desktop/wordKernel")

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
  if(lemmatize) {
    tagged.results <- treetag(document, treetagger="manual", format="obj",
                              TT.tknz=FALSE , lang="en",TT.options=list(path="C:/TreeTagger", 
                                                                        preset="en", no.unknown = TRUE))    #path argument needs to be set manually to local download of TreeTagger
    document = paste(tagged.results@TT.res$lemma, collapse = " ")
  }
  if (stem) document = paste(wordStem(strsplit(document, split = " ")[[1]]), collapse = " ")
  return(document)
}

#Reading in IMDB data
imdbdata = read_tsv("Datasets/IMDb/IMDBlabeledtrain.tsv")
imdbdata = imdbdata[,2:3] #subsetting just the first 4000
imdbtrain = imdbdata[1:4000,]
imdbtest = imdbdata[20001:21000,]

# Creating vocabulary
train_tokens = imdbtrain$review %>% 
  preProcessDocument %>% 
  word_tokenizer
it_train = itoken(train_tokens, 
                  ids = imdbtrain$id,
                  progressbar = FALSE)
vocab = create_vocabulary(it_train)

#Create document-term matrix for train
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train,vectorizer)
#sense-check:
#dim(dtm_train)
#identical(rownames(dtm_train), imdbtrain$id)

#Create document-term matrix for test
test_tokens = imdbtest$review %>% 
  preProcessDocument %>% 
  word_tokenizer
it_test = itoken(test_tokens, 
                  ids = imdbtest$id,
                  progressbar = FALSE)
dtm_test = create_dtm(it_test, vectorizer)
#sense-check:
#dim(dtm_test)
#identical(rownames(dtm_test),imdbtest$id)

#Apply TF-IDF transformation to train and test document-term matrices
tfidf = TfIdf$new() # define tfidf model
dtm_train = fit_transform(dtm_train, tfidf) # fit model to train data and transform train data with fitted model
# apply pre-trained tf-idf transformation to train data
dtm_test = dtm_test %>% transform(tfidf)

#Fit model
#10-fold CV to obtain optimal lambda for ridge)
cv_model = cv.glmnet(x = dtm_train, y = imdbtrain$sentiment[1:4000], 
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
logmodel = glmnet(x = as.matrix(dtm_train), y = imdbtrain$sentiment[1:4000], 
                  family = 'binomial',
                  lambda = min_lambda,
                  alpha = 0)

preds = predict(logmodel, newx = as.matrix(dtm_train), type = "response")[,1]
roccurve <- roc(imdbtrain$sentiment[1:4000] ~ preds)
plot(roccurve)
auc(roccurve)

#producing classification matrix and AUC for test
preds.test = predict(logmodel, newx = as.matrix(dtm_test), type = "response")[,1]
preds.test.labels = rep("0_predicted", length(preds.test))
preds.test.labels[preds.test > 0.5] = "1_predicted"
table(preds.test.labels, imdbtest$sentiment)
glmnet::auc(imdbtest$sentiment[1:1000],preds.test)

#max train 0.876, test 0.8597
