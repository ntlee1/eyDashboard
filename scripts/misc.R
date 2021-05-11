#Misc
#This function returns a character vector of subsetted columns
naPos <- function(dataframe) {
  dfCheck <- is.data.frame(dataframe)
  #TODO Dataframe check
  colNum <- ncol(dataframe)
  colValues <- c(1:colNum)
  colResults <- numeric(length(colValues))
  
  dfColPos <- as.numeric(c(colValues))
  dfCat <- as.character(deparse(substitute(dataframe)), max(colNum))
  
  for (i in 1:length(colValues)) {
    colResults[[i]] <- paste0(dfCat[1], "[", ",",i, "]")
    
  }
  
  dfPos <- as.tibble(x = colResults)
  
  colResults
}


gee <- do.call("<-", list(hello[1], kikstrt[,1]))

#Figure out why
which(is.na(kikstrt[,2]))
which(is.na(kikstrt[,13]))




pledgePos <- filter(diffPledge, `Pledge Difference` > 0)
pledgePos <- flatten(pledgePos) %>%
  unlist
summary(pledgePos)
sd(pledgePos)


naRowCheck2 <- function(myDf, myCol) {
  if (sum(is.na(myDf[,myCol])) > 0) {
    naRows <- which(is.na(myDf[,myCol]))
    
  } else {
    paste0("No NAs in column ", colnames(myDf[,myCol]))
  }
}

testy <- naRowCheck2(kikstrt, 2)





test <- ggplot(charAnalysis[1:200,1:2], aes(x = value, y = value, colour = state)) +
  geom_point(size = 2,
             position = "jitter") +
  facet_grid(~state)
charAnalysis[1:10,1:2]
