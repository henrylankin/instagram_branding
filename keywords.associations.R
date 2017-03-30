# need to use install.packages for each of these packages before you begin using this function
# for example, type install.packages(tm) into the console first

require(tm)


## This function takes a commentsFile, keywordsFile and destination path. It outputs a csv
##  file that contains a new list of suggested keywords for each brand.
## commentsFile should contain, at minimum, the messages to be examined
## keywordsFile should have the brand keywords in one column for each brand
## destinationPath is the file path to create a csv file to
## assocLevel is the minimum level of association acceptable, defaults to 0.8, 
##  must be a number between 0 and 1.0
## return is a True or False value. If True it will return the resulting data frame and output the csv
##  file, if False (it defaults to False) it will only output the csv file

keywords.associations <- function(commentsFile, keywordsFile, destinationPath, assocLevel = 0.8, return = FALSE) {
  
  # read files
  commentsData <- read.csv(commentsFile)
  keywordsData <- read.csv(keywordsFile)
  
  
  # define function to clean up messages
  cleanUpStrings <- function(x) {
    y <- x
    # remove retweet entities
    y = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", y)
    # remove at people
    y = gsub("@\\wl+", "", y)
    # remove punctuation
    y = gsub("[[:punct:]]", "", y)
    # remove numbers
    y = gsub("[[:digit:]]", "", y)
    # remove html links
    y = gsub("http\\w+", "", y)
    
    #result
    return(y)
  }
  
  # define function to clean up messages
  cleanUpStrings.Spaces <- function(x) {
    y <- x
    # remove retweet entities
    y = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", y)
    # remove at people
    y = gsub("@\\wl+", "", y)
    # remove punctuation
    y = gsub("[[:punct:]]", "", y)
    # remove numbers
    y = gsub("[[:digit:]]", "", y)
    # remove html links
    y = gsub("http\\w+", "", y)
    
    # remove unnecessary spaces
    y = gsub("[ \t]{2,}", "", y)
    y = gsub("^\\s+|\\s+$", "", y)
    
    #result
    return(y)
  }
  
  # define "tolower error handling" function 
  tryTolower = function(x) {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  } 
  
  # define function to force everything into a character type
  toCharacter <- function(x) {
    y <- x
    for (i in y) {
      y[y == i] <- as.character(i)
    }
    return(y)
  }
  
  ## define function that looks through the comments to find associations of the words 
  # in the keywords list and output a new list of keywords 
  # x is a vector containing the comments to be examined
  # y is a vector string representing the brand keywords to find associations for
  # n is a the level of association acceptable
  associations.list <- function(x, y, n) {
    
    comments <- x
    keywords <- y
    assoc.level <- n
    
    # force to character type, clean up, and make everything lowercase on comments
    comments <- sapply(comments, toCharacter)
    comments <- cleanUpStrings.Spaces(comments)
    comments <- sapply(comments, tryTolower)
    names(comments) <- NULL
  
    
    # remove empty results (if any)
    comments <- comments[comments != ""]
    keywords <- keywords[keywords != ""]
    
    # create corpus
    corpus.comments <- Corpus(VectorSource(comments))
    
    # remove stopwords
    skipwords <- c(stopwords("english"), 'instagram')
    corpus.comments <- tm_map(corpus.comments, removeWords, skipwords)
    
    # term-document matrix
    tdm <- TermDocumentMatrix(corpus.comments)
    
    # inspect most popular words --- doesn't seem to be used, may delete
    comments.freq <- findFreqTerms(tdm, lowfreq = 10, highfreq = Inf)
    
    # vector containing associations with association score
    comments.assoc <- findAssocs(tdm, keywords, assoc.level)
    
    # initiate vector to hold associated words
    comments.assocWords <- c()
    
    # loop through each brand keyword and add the associated words to comments.assocWords 
    for (name in names(comments.assoc)) {
      temp <- names(comments.assoc[[name]])
      comments.assocWords <- c(comments.assocWords, temp)
    }
    
    # remove any duplicates
    comments.assocWords <- unique(comments.assocWords)
    
    return(comments.assocWords)
  }
  
  # extract messages from commentsFile
  comments <- commentsData$Message
  
  # get brand names in keywordsFile
  brandNames <- colnames(keywordsData)
  
  # initialize a blank list to hold the old and new keywords
  result.list <- vector("list", length = length(brandNames))
  
  # loop through each of the brands to generate a new list of keywords
  for (i in 1:length(keywordsData)) {
    
    # extract keywords
    brand.keywords <- levels(keywordsData[,i])
    
    # remove empty keywords (if any)
    brand.keywords <- brand.keywords[brand.keywords != ""]
    
    # force to character type, clean up, and make everything lowercase on keywords
    brand.keywords <- sapply(brand.keywords, toCharacter)
    brand.keywords <- cleanUpStrings.Spaces(brand.keywords)
    brand.keywords <- sapply(brand.keywords, tryTolower)
    
    # save orginal set of non-blank keywords
    brand.keywords.original <- brand.keywords
    
    # create new list of keywords that are associated with the keywords in the keywords list
    brand.newkeywords <- associations.list(comments, brand.keywords, assocLevel)

    # add the original keywords and new keywords to the keywords list
    result.list[[i]] <- c(brand.keywords.original, brand.newkeywords)
  }
  
  # find the max list length
  listLength.max <- 0
  for (i in 1:length(result.list)) {
    listLength <- length(result.list[[i]])
    if (listLength > listLength.max) {
      listLength.max <- listLength
    }
  }
  
  # add blanks to each list with length smaller than 
  # the max list length to make them all of equal length
  for (i in 1:length(result.list)) {
    listLength <- length(result.list[[i]])
    if (listLength < listLength.max) {
      result.list[[i]] <- c(result.list[[i]], rep("", listLength.max - listLength))
    }
  }
  
  # convert result.list to a data frame with the original column names
  result.dataframe <- as.data.frame(result.list, col.names = brandNames, row.names = 1:listLength.max)
  
  # write the new list of keywords in result.dataframe to a csv file
  write.csv(result.dataframe, file = destinationPath)
  
  # result returns result.dataframe if return = True
  if (return) {
    return(result.dataframe)
  }
}
























