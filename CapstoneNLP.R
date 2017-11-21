## Capstone Project - Text Mining NLP ##

library(stringr)
library(tm)
library(SnowballC)
library(ggplot2)
library(shiny)
library(wordnet)
setDict("/usr/local/Cellar/wordnet/3.1/dict")
library(openNLP)
library(tidytext)
library(dplyr)
library(data.table)
library(tokenizers)
library(sqldf)

############### Function Definition Part #####################

baseDir <- "~/coursera/datascience/C-10-CapstoneProject/final"



## Function to create a random sampling of lines in a given file & number of lines to be extracted
## Accepts the file name , number of lines, locale to decide (future enhancement) on specific language files
## appnd will decide whether the repeated invocation of the function should append the file created or not
## skipLineNumber is the number of lines to be skipped from the begining upto the skipLineNumber
## A value greater than 0 will avoid random line picking and use nL as the block of lines to read

createSampleFile <- function(fileN,nL=5,locl="",appnd=FALSE,destDir = "", startLineNumber = -1) { 
  baseDir <- paste(baseDir,locl,sep="/") 
  name <- paste(baseDir,fileN,sep="/")
  con <- file(name, "r")
  
  i <- 0
  v <- NULL
  
  if(nL < 0){ # load full file
    v <- readLines(con)
    close(con)
    return(v)
  }
  
  if(startLineNumber >= 0) {
    # Skip startlineNumber - 1 times and start capturing lines
    if (startLineNumber > 0) {readLines(con, n = (startLineNumber - 1) )}
    v <- readLines(con, n=nL)
  } else if(startLineNumber < 0){ ## if startLineNumber is < 0 then create random sample file
    set.seed(2017-11-01)
    while(i < nL){
    ## find a way to get EOF and exit of this loop
      if(as.logical(rbinom(1,1,prob=.5))){
        v <- c(v,readLines(con, n=1))
        i <- i + 1
      } else { # skip
        readLines(con, n=1)
      }
    }
  }
  
  destF <- paste("sample_",fileN,sep = "") # prefix the filename
  write(v, file = paste(baseDir,destDir,destF,sep = "/"),append = appnd)
  close(con)
  return(v)
}

## Load and get the SwearWords from the file. Each word is in a new line
getSwearWords <- function(){
  con <- file(paste(baseDir,"swearWords.txt",sep = "/"), "r")
  v <- readLines(con)
  close(con)
  return(v)
}

# Using wordnet package a function to list synonyms
getSynonyms <- function(word,pos = "NOUN"){
  return(synonyms(word,pos))
}

# Tagging parts of speech (noun, pronound, verb etc with each word) using openNLP package

getTagPOS <- function(src){
  return(tagged_paras(src))
}

## Create a Summary for the given Corpus post performing cleanup - Remove whitespace, numbers, punctuation, swear & stop words
createSummary <- function(x, cleanup=FALSE, addTDM = FALSE){
  summry  <- vector("list",3) # initialize with length 2 with each being a list
  summry[[1]] <- summary(x)
  if(cleanup){
    print("Cleanup Started..")
    x <- tm_map(x, stripWhitespace)
    x <- tm_map(x, removeNumbers)
    x <- tm_map(x, removeWords,stopwords("en"))
    x <- tm_map(x, removePunctuation)
    # Profanity check
    gsw<- getSwearWords()
    x <- tm_map(x, removeWords,gsw)
    print("Cleanup Ended..")
  }
  tdm <- TermDocumentMatrix(x)
  wrdCnt <- paste("Number of Words in the document ", tdm$dimnames$Docs, " is:",format(tdm$nrow, big.mark = ","),sep = "")
  summry[[2]] <- wrdCnt
  if(addTDM) summry[[3]] <- tdm
  return(summry)
}

## Pass the n of ngram as a parameter to the function
createNgram <- function(ng){
  strt <- proc.time()
  
  ## Create Corpus using the tm package functions and perform basic pre-processing
  tdf <- paste(baseDir,"en_US/sample",sep="/")
  td <- Corpus(DirSource(tdf), readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
  ## Pre-processing 
  # Cleanup the data including removing profanity (used swear words list from http://www.bannedwordlist.com/lists/swearWords.txt)
  td <- tm_map(td, stripWhitespace)
  td <- tm_map(td, removeNumbers)
  td <- tm_map(td, removeWords,stopwords("en"))
  td <- tm_map(td, removePunctuation)
  
  # Profanity check
  gsw<- getSwearWords()
  td <- tm_map(td, removeWords,gsw)
  
  txt_cat <- c(td[[1]]$content , td[[2]]$content, td[[3]]$content)
  ngramDat <- data.frame(text = txt_cat) %>% unnest_tokens(ngram,text,token = "ngrams", n = ng) %>% group_by(ngram) %>% summarise(freq=n())
 
   end <- proc.time()
   print(paste("Elapsed time to create ngrams started @ ",Sys.time(), " --->",round((end-strt)[3],2), "secs", sep="") ) # Elapsed time is 3rd element in the output
   return(ngramDat)
}


## Create a WordCloud given a TermDocumentMatrix object

createWordCloud <- function(x) {
  mat <- as.matrix(tdm)
  aggr <- sort(rowSums(mat),decreasing=TRUE)
  df <- data.frame(trm = names(aggr), freq=aggr)
  set.seed(1234)
  wordcloud(words = df$trm, freq = df$freq, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.45, 
            colors=brewer.pal(8,"Accent"))
}

## Function to take in list of matched lines and a current matched token 
## and returns list of nexttokens (next to the matched string) across the list of matched lines

nextTokenList <- function(matchingLines,curToken){
  stopifnot(!is.null(curToken) | length(matchingLines) > 0 )
  nxTknList <- NULL
  
  ## Tokenize the matchingLines list into List of lists with each sublist having array of tokens 
  lst<-tokenize_words(matchingLines,lowercase = TRUE,simplify = TRUE) 
  
  if(length(lst) == 0) return(nxTknList)
  
  ## Loop through each list of lists (of tokens)
  for(i in 1:length(lst)){
    tknLst <- lst[[i]] ## list of tokens
    idxLst <- which( tolower(tknLst) == tolower(curToken)) ## indeces of current token matched against the list of tokens from the list of lists
    
    ## Loop through the indeces list and pull the next token which is next token in the list of tokens
    l <- length(tknLst)
    
    if (length(idxLst) == 0 ) next ## Remember a for loop with upper bound as 0 will enter the loop
    for(j in 1:length(idxLst)) {
      ## Check for array index: in case the last token is the matching token then there is no next token
      k <- idxLst[j] + 1
      if(k <= l) { 
        nxTknList <- c(nxTknList, tknLst[idxLst[j] + 1])
      }
    }
  }
  return(nxTknList)
}

## Find the last token in the given string. Used as an anchor / preceding token for nextTokenList function
findLastToken <- function(str){
  stopifnot(!is.null(str))
  str <- tolower(trimws(str, which="both"))
  stopifnot(nchar(str) > 0)
  lst <- tokenize_words(str)
  return(lst[[1]] [length(lst[[1]])])
}


## Function that executes system command(unix only) to get the line count of the file
getLineCount <- function(fullFilePath){
  sysCommand <- paste("wc -l ", fullFilePath, " | awk '{print $1}'", sep = "")
  ct<- as.numeric(system(sysCommand, intern = TRUE))
  print(ct)
  return(ct)
}

connectDB <- function(dbName="Capstone.sqlite"){
  db <- dbConnect(SQLite(), dbname=dbName)
  return(db)
}

closeDB <- function(dbConnection){
  db <- dbDisconnect(dbConnection)
  return(db)
}

setValueSQL <- function(con,key,val){
  upd <- dbSendQuery(con, "UPDATE keyval SET value = ? WHERE key = ?")
  dbBind(upd,list(val,key))
  if(dbGetRowsAffected(upd) == 0){ ## if the key doesn't exist insert a key val pair
    ins <- dbSendQuery(conn = con, "INSERT INTO keyval VALUES (?, ?)")
    dbBind(ins,list(key,val))
  }
}

getValueSQL <- function(con, key){
  sel <- dbSendQuery(conn = con, "SELECT value FROM keyval WHERE key = ?")
  rs <- dbBind(sel,list("FROM"))
  v <- dbFetch(rs)
  dbClearResult(rs)
  if (length(v$value) == 0) {return("1")}
  return(v$value[1]) # column name from the fetched row
}

deleteKeyValSQL <- function(con,key){
  del <- dbSendQuery(db, "DELETE FROM keyval WHERE key = ?")
  dbBind(del, list(key))
  dbClearResult(del)
}

flushNgramData <- function(con, ngramD){
  strt <- proc.time()
  dbWriteTable(con, "ngrams", ngramD, append = TRUE)
  end <- proc.time()
  print(paste("Elapsed time for ngrams flushed into DB @ ",Sys.time(), " --->",round((end-strt)[3],2), "secs", sep="") ) 
}

## Get the dataframe for the Entered string with results fetched from (DB/source) limited to setnumber of records 
getMatchingData <- function(con, str,limit){
  rs <- dbSendQuery(con, "SELECT * FROM  ngrams WHERE ngram  LIKE  ? ORDER BY freq DESC LIMIT ?")
  dbBind(rs, list(paste("%",str,"%", sep=""),limit))
  df <- dbFetch(rs)
  dbClearResult(rs)
  return(df)
}

## Fetches the final predicted list of words to display
predictedWList <- function(str){
  db <- dbConnect(SQLite(), dbname="Capstone.sqlite")
  df <- getMatchingData(db, str,10)
  lst <- unique(nextTokenList(df$ngram,findLastToken(str)))
  dbDisconnect(db)
  return(lst)
}

############### Function Calling Part #####################

## Create Sample files with nL = 1000 (number of lines randomly picked) for the three data set in a separate folder "sample". 
## When nL <0 then the vector will be assigned the whole text file without sampling

blogsLc <- getLineCount("~/coursera/datascience/C-10-CapstoneProject/final/en_US/en_US.blogs.txt")
newsLc <- getLineCount("~/coursera/datascience/C-10-CapstoneProject/final/en_US/en_US.news.txt")
twtrLc <- getLineCount("~/coursera/datascience/C-10-CapstoneProject/final/en_US/en_US.twitter.txt")

## seq function splits the range min and max by length.out. We round it off by floor
## diff function takes the vector and uses a lag of 1 which is how many indeces in the array
## to jump to take the number and subtract the current index number
## Each value in these arrays will give the number of lines to be read from the original file
## so that the sum of all values in the array will equate to total lc 
## after adding one mseq/lout to the vector that gets lost during the diff operation


lout = 1000
mseq = 1000

bSeqnL <- diff(floor(seq(mseq, blogsLc, length.out = lout )))
nSeqnL <- diff(floor(seq(mseq, newsLc, length.out = lout)))
tSeqnL <- diff(floor(seq(mseq, twtrLc, length.out = lout)))

r1 <- sum(bSeqnL) - blogsLc
r2 <- sum(nSeqnL) - newsLc
r3 <- sum(tSeqnL) - twtrLc

bSeqnL <- c(bSeqnL, r1)
nSeqnL <- c(nSeqnL, r2)
tSeqnL <- c(tSeqnL, r3)

db <- connectDB() # default is Capstone DB

from <- as.numeric(getValueSQL(db,"FROM"))

for (k in from:lout){
  if (from == 1){ offs <- 0} else{ offs <- 1} # to avoid skipping lines when we first start 
  
  blogs <- createSampleFile("en_US.blogs.txt",locl="en_US",nL=bSeqnL[k],destDir = "sample", startLineNumber = offs * (sum(bSeqnL[1:k]) + 1 ) )
  news  <- createSampleFile("en_US.news.txt",locl="en_US",nL=nSeqnL[k],destDir = "sample", startLineNumber = offs *  (sum(nSeqnL[1:k]) + 1 ) )
  twtr  <- createSampleFile("en_US.twitter.txt",locl="en_US",nL=tSeqnL[k],destDir = "sample",startLineNumber = offs * (sum(tSeqnL[1:k]) + 1) )
  
  ngramData <- createNgram(4) # quad gram
  flushNgramData(db,ngramData)
    
  setValueSQL(db,"FROM",as.character(k)) # record the position after successful execution.Recovery from where it is left
}    

print("Cleaning up the persisted pointer & closing DB <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
#deleteKeyValSQL(db,"FROM")
closeDB(db)









##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

blogs <- createSampleFile("en_US.blogs.txt",locl="en_US",nL=1000,destDir = "sample", startLineNumber = -1 )
news  <- createSampleFile("en_US.news.txt",locl="en_US",nL=1000,destDir = "sample", startLineNumber = -1 )
twtr  <- createSampleFile("en_US.twitter.txt",locl="en_US",nL=1000,destDir = "sample", startLineNumber = -1)

ngd <- createNgram(2)


# ## Create Corpus using the tm package functions and perform basic pre-processing
# tdf <- paste(baseDir,"en_US/sample",sep="/")
# td <- Corpus(DirSource(tdf), readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
# 
# ## Exploratory analysis 
# # Sanity check the Corpus created using summary function
# summary(td)
# 
# # Check the contents
# td[[1]]$content
# td[[2]]$content
# td[[3]]$content
# 


# # *Note:* getReaders() to get lists all readers 
# 
# ## Pre-processing 
# # Cleanup the data including removing profanity (used swear words list from http://www.bannedwordlist.com/lists/swearWords.txt)
# td <- tm_map(td, stripWhitespace)
# td <- tm_map(td, removeNumbers)
# td <- tm_map(td, removeWords,stopwords("en"))
# td <- tm_map(td, removePunctuation)
# 
# # Profanity check
# gsw<- getSwearWords()
# td <- tm_map(td, removeWords,gsw)
# 
# # Stemming
# 
# #td <- tm_map(td, stemDocument)
# 
# 
# ## Create TermDocumentMatrix (matrix of terms, frequency etc.) for the 3 text files in the Corpus
# tdm <- TermDocumentMatrix(td)
# #dtm <- DocumentTermMatrix(td)
# 
# 
# 
# ## Look at the *sparsity* and create sparse matrix
# 
# 
# # tdm$dimnames  ## gives all the terms
# # tdm$Docs ## to get the documents processed for creating TDM 
# 
# ## hierarchichal clustering between documents
# #hclust(dist(tdm),method = "ward.D2")

matchingList <- c("I am struck so struck find", "second struck in", "Third Struck struck" )
tkns <- nextTokenList(matchingList,"struck")
tkns

#gtxt <- grep(t, pattern = "( +[a-zA-Z] +)?struck +", value = TRUE) ## list of lines that have the matching word
#pred <- gsub("struck\\s([^ ]*).*$","\\1", gtxt )





