

## This function takes a commentsFile, keywordsFile and destination path. It outputs a csv
##   file similar to the raw comments data but with additional columns for each brand stating 
##   whether the message mentions the brand or is unclassified. Also, has all cleaned up messages
## commentsFile should contain, at minimum, the estimated impressions, messages and countries
## keywordsFile should have the brand keywords in one column for each brand
## destination is the file path to create a csv file to

brand.mentions <- function(commentsFile, keywordsFile, destinationPath) {
  
  # read files
  commentsData <- read.csv(commentsFile)
  keywordsData <- read.csv(keywordsFile)
  
  # initiate result.dataframe to hold results
  result.dataframe <- commentsData
  
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
    
    # result
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
    
    # result
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
  
  
  # define function that adds a marker to each string in a vector to differentitae duplicates
  differentiateStringsInVector <- function(x) {
    y <- x
    for (i in 1:length(y)) {
      y[i] <- paste(y[i], i)
    }
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
  
  # extract messages and intiatiate the comments.classified vector 
  # to append 'classified' to all comments with brand keywords
  comments <- as.character(commentsData$Message)
  comments.classified <- comments
  
  # clean up messages, make everything lower case, differentiate duplicates in commentsData, 
  # and force character type on each message
  comments <- sapply(comments, toCharacter)
  comments <- sapply(comments, tryTolower)
  comments <- cleanUpStrings.Spaces(comments)
  comments <- differentiateStringsInVector(comments)
  names(comments) <- NULL
  
  # get brand names in keywords file and add an Unclassified brand
  brandNames <- c(colnames(keywordsData), 'Unclassified')
  
  for (i in 1:length(keywordsData)) {
    # set brand, force character type and remove blank keywords
    brand.keywords <- keywordsData[,i]
    brand.keywords <- brand.keywords[brand.keywords != ""]
    
    # clean up keywords and make everything lower case
    brand.keywords <- cleanUpStrings(as.character(brand.keywords))
    brand.keywords <- tryTolower(brand.keywords)
    
    # initate vector of comment indices that mention brand keywords
    brand.commentsVector <- c()
    
    # loop through all the keywords and add the index to the the brand.commentsVector
    for (word in c(brand.keywords) ) {
      temp <- grep(word, comments, perl = TRUE, useBytes = TRUE)
      brand.commentsVector <- c(brand.commentsVector, temp)
    }
    
    # remove duplicate countings of a brand in a comment
    brand.commentsVector <- unique(brand.commentsVector)
    
    # the brand column vector with yes if the brand is mentioned in the comment and blank otherwise
    brand.yesno <- vector(length = length(comments))
    brand.yesno[brand.commentsVector] <- 'yes'
    brand.yesno[-brand.commentsVector] <- ""
    
    # add the brand column to result.dataframe
    result.dataframe[brandNames[i]] <- brand.yesno
    
    # paste classified to the end of each message containing a keyword
    for (index in brand.commentsVector) {
      comments.classified[index] <- paste(comments.classified[index], " classified")
    }
  }
  
  # initiate vector to hold indices of the messages that have been classified with a brand keyword
  classified.indices <- c()
  
  # loop through each message in the comments to check if it contains the classified marker
  # if so, add the index to classified indices
  for (message in comments.classified) {
    temp <- grep('classified', comments.classified, perl = TRUE)
    classified.indices <- c(classified.indices, temp)
  }
  classified.indices <- unique(classified.indices)

  # unclassified column vector: unclassified for messages that did not have a keyword, blank otherwise
  unclassified.yesno <- vector(length = length(comments))
  unclassified.yesno[-classified.indices] <- 'unclassified'
  unclassified.yesno[classified.indices] <- ""
  
  # add the unclassified column to result.dataframe
  result.dataframe["Unclassified"] <- unclassified.yesno
  result.dataframe["Cleaned Messages"] <- comments
  
  # write result.dataframe to a csv file
  write.csv(result.dataframe, file = destinationPath)
  
  # result returns nothing since the csv file is the intended output
  return()
}






















