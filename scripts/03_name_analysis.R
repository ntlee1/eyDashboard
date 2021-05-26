#Determine the optimal name for a client campaign.
#For this analysis we ignore state == c(live, suspended, canceled).
#These ambiguous outcomes not insightful for this use case

#Do Successful Project Names Differ in Their Character Count? ------------------
charAnalysis <- nchar(kikstrt$name)
charAnalysis <- as.tibble(charAnalysis)
charAnalysis$state <- kikstrt$state

#t-test used for simple mean comparison between two sample groups
charSuccess <- filter(charAnalysis, state == "successful")
charFailed <- filter(charAnalysis, state == "failed")
ttestChar <- t.test(charSuccess$value, charFailed$value)
#Successful Campaign Names Average ~36.2 characters. Failed Campaign Names
#Average ~32.8 Characters
charAvgSuccess <- ttestChar$estimate[1]
charAvgFail <- ttestChar$estimate[2]

#Do Successful Project Names Differ in Their Word Count?------------------------
#Regex to count number of words in a name
wordLenAnalysis <- stringr::str_count(kikstrt$name, "\\S+")
wordLenAnalysis <- as.tibble(wordLenAnalysis)
wordLenAnalysis$state <- as.factor(kikstrt$state)

#t-test used for simple mean comparision between two sample groups
wordLenSuccess <- filter(wordLenAnalysis, state == "successful")
wordLenFail <- filter(wordLenAnalysis, state == "failed")
ttestWord <- t.test(wordLenSuccess$value, wordLenFail$value)
#Successful Campaign Names Average ~6 words. Failed Campaigns Average ~5.3
wordLenAvgSuccess <- ttestWord$estimate[1]
wordLenAvgFail <- ttestWord$estimate[2]

#Compare name, character/word length, and state by size of campaign
#By number of characters
charKikstrt <- kikstrt
charKikstrt$CharCount <- nchar(charKikstrt$name)
charKikstrt <- filter(charKikstrt, state == "successful" | state == "failed")

#Inputs: Small, Mid, Prem, Large
charPlotFun <- function(kikstrtSize) {
  mySize <- deparse(substitute(kikstrtSize))
  charPlot <- filter(charKikstrt, size == mySize) %>%
    ggplot2::ggplot(., aes(x = CharCount, color = state)) +
    geom_histogram(fill = "white", alpha = 0.7, position = "identity") +
    xlab("Kickstarter Name Length (characters)") +
    ylab("Number of Kickstarter Projects") +
    labs(title = paste("Length of Kickstarter Project Names: ", mySize, "Projects")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(charPlot)
}

#By number of words 
wordKikstrt <- kikstrt
wordKikstrt$wordCount <-  stringr::str_count(kikstrt$name, "\\S+")
wordKikstrt <- filter(wordKikstrt, state == "successful" | state == "failed")

#Inputs: Small, Mid, Prem, Large
wordPlotFun <- function(kikstrtSize) {
  mySize <- deparse(substitute(kikstrtSize))
  WordPlot <- filter(wordKikstrt, size == mySize) %>%
    ggplot2::ggplot(., aes(x = `wordCount`, color = state)) +
    geom_histogram(fill = "white", alpha = 0.7, position = "identity",
                   breaks = seq(from = 0.5, to = 20, by = 1)) +
    xlab("Kickstarter Name Length (words)") +
    ylab("Number of Kickstarter Projects") +
    labs(title = paste("Length of Kickstarter Project Names:",
                       mySize, "Projects")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(WordPlot)
}


#Name length recommender -------------------------------------------------------
#Purpose: Gives client the optimal length for their project name based on 
#campaign goal
#Appropriate linear model selected based on goal size
optimalNmLen <- function (goalAmount) {
  if(5 < goalAmount & goalAmount < 1302) {
    lmCharSmall <- lm(CharCount~usd_goal_real,
                      data = filter(charKikstrt,
                                    size == "Small",
                                    state == "successful"))
    goalSmallMe <-  as.numeric(lmCharSmall$coefficients[1] +
                                 lmCharSmall$coefficients[2]*goalAmount) %>%
      round(digits = 0)
    paste("The optimal number of characters for your project name is",
          goalSmallMe)
  } else {
    if(1302 <= goalAmount & goalAmount < 3838) {
      lmCharMid <- lm(CharCount~usd_goal_real,
                      data = filter(charKikstrt,
                                    size == "Mid",
                                    state == "successful"))
      goalMidMe <-  as.numeric(lmCharMid$coefficients[1] +
                                 lmCharMid$coefficients[2]*goalAmount) %>%
        round(digits = 0)
      paste("The optimal number of characters for your project name is",
            goalMidMe)
    } else {
      if(3838 <= goalAmount & goalAmount < 10000) {
        lmCharLarge <- lm(CharCount~usd_goal_real,
                          data = filter(charKikstrt,
                                        size == "Large",
                                        state == "successful"))
        goalLargeMe <-  as.numeric(lmCharLarge$coefficients[1] +
                                     lmCharLarge$coefficients[2]*goalAmount) %>%
          round(digits = 0)
        paste("The optimal number of characters for your project name is",
              goalLargeMe)
      } else {
        if(10000 <= goalAmount & goalAmount < 775000) {
          lmCharPrem <- lm(CharCount~usd_goal_real,
                           data = filter(charKikstrt,
                                         size == "Prem",
                                         state == "successful"))
          goalPremMe <-  as.numeric(lmCharPrem$coefficients[1] +
                                      lmCharPrem$coefficients[2]*goalAmount) %>%
            round(digits = 0)
          paste("The optimal number of characters for your project name is", goalPremMe)
        } else {
          if(goalAmount < 5 | 775000 < goalAmount) {
            print("Campaign goal size out of range. Recommender valid for campaign sizes $5 to $775,000")
          }
        }
      }
    }  
  }    
}



