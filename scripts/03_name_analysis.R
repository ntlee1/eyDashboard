#What is the best type of name for a client campaign?
#For this analysis we ignore state == c(live, suspended, canceled).
#These ambiguous outcomes not insightful for this use case

#Do Successful Project Names Differ in Their Character Count? ------------------
charAnalysis <- nchar(kikstrt$name)
charAnalysis <- as.tibble(charAnalysis)
charAnalysis$state <- as.factor(kikstrt$state)

#t-test used for simple mean comparison between two sample groups
charSuccess <- filter(charAnalysis, state == "successful")
charFailed <- filter(charAnalysis, state == "failed")

ttestChar <- t.test(charSuccess$value, charFailed$value)
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
wordLenAvgSuccess <- ttestWord$estimate[1]
wordLenAvgFail <- ttestWord$estimate[2]

#Name Length Analysis by Campaign Size--------------------------------------
#Bin analysis by successful project quantiles to create 4 rough categories of project size
#Success Quantiles 1302, 3838, 10000
#Small, Mid, Large, Premium
projSuccess <- filter(kikstrt, state == "successful")
summary(projSuccess$usd_goal_real)

#Compare name, character/word length, and state by size of campaign
#By number of characters
charKikstrt <- kikstrt[,c("name", "usd_goal_real", "state")]
charKikstrt$name <- nchar(charKikstrt$name)
charKikstrt$state <- as.factor(charKikstrt$state)
charKikstrt <- filter(charKikstrt, state == "successful" | state == "failed")

goalCharSmall <- filter(charKikstrt, usd_goal_real < 1302) 
goalCharMid <- filter(charKikstrt, 1302 <= usd_goal_real & usd_goal_real < 3838) 
goalCharLarge <- filter(charKikstrt, 3838 <= usd_goal_real & usd_goal_real < 10000) 
goalCharPrem <- filter(charKikstrt, 10000 <= usd_goal_real)

goalCharSmallPlot <- goalCharSmall %>%
  ggplot2::ggplot(., aes(x = name, color = state)) +
  geom_histogram(fill = "white", alpha = 0.7, position = "identity") +
  xlab("Kickstarter Name Length (characters)") +
  ylab("Number of Kickstarter Projects") +
  labs(title = "Length of Kickstarter Project Names: Small Projects") 

goalCharMidPlot <- goalCharMid %>%
  ggplot2::ggplot(., aes(x = name, color = state)) +
  geom_histogram(fill = "white", alpha = 0.7, position = "identity") +
  xlab("Kickstarter Name Length (characters)") +
  ylab("Number of Kickstarter Projects") +
  labs(title = "Length of Kickstarter Project Names: Mid Projects") 

goalCharLargePlot <- goalCharLarge %>%
  ggplot2::ggplot(., aes(x = name, color = state)) +
  geom_histogram(fill = "white", alpha = 0.7, position = "identity") +
  xlab("Kickstarter Name Length (characters)") +
  ylab("Number of Kickstarter Projects") +
  labs(title = "Length of Kickstarter Project Names: Large Projects") 

goalCharPremPlot <- goalCharPrem %>%
  ggplot2::ggplot(., aes(x = name, color = state)) +
  geom_histogram(fill = "white", alpha = 0.7, position = "identity") +
  xlab("Kickstarter Name Length (characters)") +
  ylab("Number of Kickstarter Projects") +
  labs(title = "Length of Kickstarter Project Names: Premium Projects") 

#By number of words 
wordKikstrt <- kikstrt[,c("name", "usd_goal_real", "state")]
wordKikstrt$name <-  stringr::str_count(kikstrt$name, "\\S+")
wordKikstrt$state <- as.factor(wordKikstrt$state)
wordKikstrt <- filter(wordKikstrt, state == "successful" | state == "failed")
colnames(wordKikstrt)[1] <- c("Number of Words")

goalWordSmall <- filter(wordKikstrt, usd_goal_real < 1302) 
goalWordMid <- filter(wordKikstrt, 1302 <= usd_goal_real & usd_goal_real < 3838) 
goalWordLarge <- filter(wordKikstrt, 3838 <= usd_goal_real & usd_goal_real < 10000) 
goalWordPrem <- filter(wordKikstrt, 10000 <= usd_goal_real)

#TODO Beautify and plot other sizes
goalWordSmallPlot <- goalWordSmall %>%
  ggplot2::ggplot(., aes(x = `Number of Words`, color = state)) +
  geom_histogram(fill = "white", alpha = 0.7, position = "identity",
                 breaks = seq(from = 0.5, to = 20, by = 1)) +
  xlab("Kickstarter Name Length (words)") +
  ylab("Number of Kickstarter Projects") +
  labs(title = "Length of Kickstarter Project Names: Small Projects") 


#Name length recommender based on project size----------------------------------
#Use a linear model to recommend the optimal name length for a client

#Filter out projects with a goal of less than $5 (~11% of the small data)
#Projects set at this amount typically use the low goal amount as a marketing
#ploy, not a true representation of the money they seek.
#This tactic can be seen throughout the small project subset but a large majority
#can be filtered out with the $5 limit. 
lmCharSmall <- lm(name~usd_goal_real, data = filter(goalCharSmall, state == "successful",
                                                    5 < usd_goal_real))
lmCharMid <- lm(name~usd_goal_real, data = filter(goalCharMid, state == "successful"))
lmCharLarge <- lm(name~usd_goal_real, data = filter(goalCharLarge, state == "successful"))
#Filter the top 1% of data out for premium. 
#Excessively large campaigns skew model toward suggesting a name length
#that only matches the excessively large campaigns in question.
lmCharPremSkew <- arrange(goalCharPrem, -usd_goal_real)[-c(1:1228),]
lmCharPremSkew <- filter(lmCharPremSkew, state == "successful")
lmCharPrem <- lm(name~usd_goal_real, data = lmCharPremSkew)
#Name length recommender
#Gives client the optimal length for their project name based on campaign goal
#Appropriate lm selected based on goal size
#TODO
#Add ideal number of words 
#TODO
#Set error message for
#negative input
#alphabet input
#non conforming numeric format
optimalNmLen <- function (goalAmount) {
  if(5 < goalAmount & goalAmount < 1302) {
   goalSmallMe <-  as.numeric(lmCharSmall$coefficients[1] +
                                lmCharSmall$coefficients[2]*goalAmount) %>%
      round(digits = 0)
   paste("The optimal number of characters for your project name is", goalSmallMe)
    
  } else {
    if(1302 <= goalAmount & goalAmount < 3838) {
      goalMidMe <-  as.numeric(lmCharMid$coefficients[1] +
                                 lmCharMid$coefficients[2]*goalAmount) %>%
        round(digits = 0)
      paste("The optimal number of characters for your project name is", goalMidMe)
    } else {
      if(3838 <= goalAmount & goalAmount < 10000) {
        goalLargeMe <-  as.numeric(lmCharLarge$coefficients[1] +
                                     lmCharLarge$coefficients[2]*goalAmount) %>%
          round(digits = 0)
        paste("The optimal number of characters for your project name is", goalLargeMe)
      } else {
        if(10000 <= goalAmount & goalAmount < 775000) {
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

