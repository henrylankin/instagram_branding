# need to use install.packages for each of these packages before you begin using this function
# for example, type install.packages(tm) into the console first

require(tm)

## This function takes a commentsFile, keywordsFile and destination path. It outputs a csv
## file that contains, for each month, brand and country, the number of comments, proportion of comments,
## number of impressions and proportion of impressions.
## commentsFile should contain, at minimum, the post data, estimated impressions, message and country
##  it does not account for a difference between months of different years, so the commentsFile should be over
##  a maximum of a one year period.
## keywordsFile should have the brand keywords in one column for each brand
## destination is the file path to create a csv file to

proportion.comments.months <- function(commentsFile, keywordsFile, destination) {

  #read files
  commentsData <- read.csv(commentsFile)
  keywordsData <- read.csv(keywordsFile)
  
  
  #define function to clean up messages
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
    
    
    # remove quotations
    
    # remove unnecessary spaces
    #y = gsub("[ \t]{2,}", "", y)
    #y = gsub("^\\s+|\\s+$", "", y)
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
  
  #create a months column vector and mark each message with the post month
  comments.month <- c()
  postDates <- as.character(commentsData$Post.Date)
  for (i in 1:length(postDates)) {
    if (nchar(postDates[i]) == 8) {
      temp.month <- substr(postDates[i], 3, 5)
      comments.month <- c(comments.month, temp.month)
    }
    if (nchar(postDates[i]) == 9) {
      temp.month <- substr(postDates[i], 4, 6)
      comments.month <- c(comments.month, temp.month)
    }
  }
  
  # vector to hold the months of each post date
  commentsData["Month"] <- comments.month
  
  #clean up messages, make everything lower case, differentiate duplicates in commentsData, 
  #and force character type on each message
  commentsData$Message <- cleanUpStrings(commentsData$Message)
  commentsData$Message <- sapply(commentsData$Message, tryTolower)
  commentsData$Message <- differentiateStringsInVector(commentsData$Message)
  commentsData$Message <- sapply(commentsData$Message, toCharacter)
  
  
  #get brand names in keywords file
  brandNames <- c(colnames(keywordsData), 'Unclassified')
  
  #extract countries from comments file
  countryVector <- unique(commentsData$Country)
  
  
  ##create data frame that will hold comment counts, comment proportions, 
  ##impression count, and impression proportion by month, country and brand
  repeat.months <- c()
  for (month in month.abb) {
    repeat.months <- c(repeat.months, rep(month, length(countryVector)*length(brandNames)))
  }
  repeat.countries <- c()
  for (country in countryVector) {
    repeat.countries <- c(repeat.countries, rep(country, length(brandNames)))
  }
  repeat.brands <- rep(brandNames, length(countryVector))
  repeat.commentCounts <- rep(0, length(repeat.countries))
  repeat.commentProportions <- rep(0, length(repeat.countries))
  repeat.impressionsCounts <- rep(0, length(repeat.countries))
  repeat.impressionsProportions <- rep(0, length(repeat.countries))
  
  result.dataframe <- data.frame(repeat.months, repeat.countries, repeat.brands, 
                                 repeat.commentCounts, repeat.commentProportions,
                                 repeat.impressionsCounts, repeat.impressionsProportions)
  colnames(result.dataframe) <- c('Month', 'Country', 'Brand', 'Comment_Counts', 
                                  'Comment_Proportions', 'Impressions_Counts', 'Impressions_Proportions')

  ##process and add data to the data frame by looping through each month, country and brand
  for (month in month.abb) {
    for (country in countryVector) {
      
      #extract comments vector for the country and intiatiate the comments.classified vector 
      #to append 'classified' to all comments with brand keywords
      comments <- commentsData$Message[commentsData$Country == country & commentsData$Month == month]
      comments.classified <- comments

      
      #total number of comments for the country
      totalComments <- length(comments)
      #total impressions for the country
      totalImpressions <- 0
      if (totalComments > 0) {
        totalImpressions <- sum(commentsData$Estimated.Impressions[commentsData$Country == country 
                                                                   & commentsData$Month == month])
      }

      
      #loop through each brand in the keywords file
      for (i in 1:length(keywordsData)) {
        
        #set brand, force character type and remove blank keywords
        brand.keywords <- keywordsData[,i]
        brand.keywords <- brand.keywords[brand.keywords != ""]
        
        #clean up keywords and make everything lower case
        brand.keywords <- cleanUpStrings(as.character(brand.keywords))
        brand.keywords <- tryTolower(brand.keywords)
        
        #initate vector of comment indices that mention brand keywords
        brand.commentsVector <- c()
        
        
        #loop through all the keywords and add the index to the the commentsVector
        for (word in brand.keywords) {
          temp <- grep(word, comments, perl = TRUE, useBytes = TRUE)
          brand.commentsVector <- c(brand.commentsVector, temp)
        }
        
        #remove duplicate countings of a brand in a comment
        brand.commentsVector <- unique(brand.commentsVector)
        
        #calculate the number and the proportion of comments that contain brand keywords
        #proportion is set to 0 if there are not comments for the month
        brand.commentCount <- length(brand.commentsVector)
        brand.commentProportion <- 0
        if (totalComments > 0) {
          brand.commentProportion <- brand.commentCount/totalComments
        }
        
        #calculate the impressions and proportion of impressoins of each brand according to the indices 
        # in brand.commentsVector, proportion is set to 0 if there are 
        brand.impressionsCount <- 0
        for (index in brand.commentsVector) {
          brand.impressionsCount <- brand.impressionsCount + commentsData$Estimated.Impressions[commentsData$Message == comments[index]]
          
          # paste classified to the end of each message containing a keyword if comments available
          if (totalComments > 0) {
            comments.classified[index] <- paste(comments.classified[index], " classified")
          }
        }
        brand.impressionsProportion <- 0
        if (totalComments > 0) {
          #calculate the proportion of impressions for the brand
          brand.impressionsProportion <- brand.impressionsCount/totalImpressions
        }
        
        #add brand comment count, brand comment proportion, 
        #brand impression count, and brand impression proportion to result.dataframe
        result.dataframe$Comment_Counts[result.dataframe$Country == country 
                                        & result.dataframe$Brand == brandNames[i]
                                        & result.dataframe$Month == month] <- brand.commentCount
        result.dataframe$Comment_Proportions[result.dataframe$Country == country 
                                        & result.dataframe$Brand == brandNames[i]
                                        & result.dataframe$Month == month] <- brand.commentProportion
        result.dataframe$Impressions_Counts[result.dataframe$Country == country
                                        & result.dataframe$Brand == brandNames[i]
                                        & result.dataframe$Month == month] <- brand.impressionsCount
        result.dataframe$Impressions_Proportions[result.dataframe$Country == country
                                        & result.dataframe$Brand == brandNames[i]
                                        & result.dataframe$Month == month] <- brand.impressionsProportion
      }
      
      # initiate vector to hold indices of the messages that have been classified with a brand keyword
      classified.indices <- c()
      
      # loop through each message in the comments of the country to check if it 
      # contains the classified marker; if so, add the index to classified indices
      for (message in comments.classified) {
        temp <- grep('classified', comments.classified, perl = TRUE)
        classified.indices <- c(classified.indices, temp)
      }
      classified.indices <- unique(classified.indices)
      
      #initiate vector to hold unclassified messages
      unclassified.messages <- c()

      
      if (totalComments > 0) {
        #loop through each index in the comments vector and add all the messages 
        # who's index is not in classified.indices to unclassified.messages
        for (i in 1:length(comments.classified)) {
          if (!is.element(i, classified.indices)) {
            unclassified.messages <- c(unclassified.messages, comments.classified[i])
          }
        }
      }
      
      #calculate the number and the proportion of comments that are unclassifed keywords
      unclassified.commentCount <- length(unclassified.messages)
      unclassified.commentProportion <- 0
      if (totalComments > 0) {
        unclassified.commentProportion <- unclassified.commentCount/totalComments
      }
      
      #calculate the total impressions of unclassified messages
      unclassified.impressionsCount <- 0
      for (message in unclassified.messages) {
        unclassified.impressionsCount <- unclassified.impressionsCount + commentsData$Estimated.Impressions[commentsData$Message == message]
        if (totalImpressions > 0) {
          commentsData$Message[commentsData$Message == message] <- paste(commentsData$Message[commentsData$Message == message], 
                                                                         ' unclassified')
        }
      }
      
      unclassified.impressionsProportion <- 0
      if (totalImpressions > 0) {
        #calculate the proportion of impressions that are unclassified
        unclassified.impressionsProportion <- unclassified.impressionsCount/totalImpressions
      }
      
      #add the unclassified comment count, comment proportion, 
      #impression count, and impression proportion to result.dataframe
      result.dataframe$Comment_Counts[result.dataframe$Country == country
                                      & result.dataframe$Brand == 'Unclassified'
                                      & result.dataframe$Month == month] <- unclassified.commentCount
      result.dataframe$Comment_Proportions[result.dataframe$Country == country
                                      & result.dataframe$Brand == 'Unclassified'
                                      & result.dataframe$Month == month] <- unclassified.commentProportion
      result.dataframe$Impressions_Counts[result.dataframe$Country == country
                                      & result.dataframe$Brand == 'Unclassified'
                                      & result.dataframe$Month == month] <- unclassified.impressionsCount
      result.dataframe$Impressions_Proportions[result.dataframe$Country == country
                                      & result.dataframe$Brand == 'Unclassified'
                                      & result.dataframe$Month == month] <- unclassified.impressionsProportion
    }
  }

  #write result.dataframe to a csv file
  write.csv(result.dataframe, file = destination)
    
  return()
}