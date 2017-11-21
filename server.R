#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(stringr)
library(tm)
library(SnowballC)
library(ggplot2)
library(openNLP)
library(tidytext)
library(dplyr)
library(data.table)
library(tokenizers)
library(sqldf)

############### Function Definition Part #####################


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
  db <- dbConnect(SQLite(), dbname="./Capstone.sqlite")
  df <- getMatchingData(db, str,10)
  lst <- unique(nextTokenList(df$ngram,findLastToken(str)))
  dbDisconnect(db)
  return(lst)
}


library(shiny)
library(sqldf)
library(googlesheets)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  output$Note <- renderText({
    paste("Note: 1) Enter 3-4 words for better prediction and if not found reduce to couple of words",
          "2) ngrams (quadgrams) is created for the entire dataset given and searched through SQLLite. Hence, expect some delay for the prediction to work", sep ="\n" ) })
  
  ## observe Submit event
  observeEvent(input$submit,{
    
    lst <- NULL
     #  if(input$submit >= 1){
      str <- input$yourtext
      if(is.null(str) | nchar(str) <= 0) {
        showNotification("No words entered to predict the next word ..",type="error")
        return()
      }
      withProgress(message = 'Prediction in progress',
                   detail = 'This may take a while...', value = 0, {
                       #str <- input$yourtext
                       str <- tolower(trimws(str, which="both"))
                       incProgress(1/2)
                       lst <- predictedWList(tolower(str))
                       if(is.null(lst) | length(lst) <= 0) {
                         showNotification("No words predicted. Sorry !! I will learn soon..",type="error")
                         return()
                       }
                       incProgress(1)
                    })
      
      output$predicted <- renderText({
        #str <- input$yourtext
        predicted <- toString(lst)
        paste("Entered Text:", str, "Predicted Word(s) by order of higher frequencies:", predicted, sep="\n")
      })
      
  # }
    
  })
  
  
})
  
  

