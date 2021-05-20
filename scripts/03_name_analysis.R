#What type of fundraiser names predict success?
#For this analysis we ignore state == c(live, suspended, canceled).
#These ambiguous outcomes not comparable
#Additionally, what is the best fitting name for a campaign? 

#General Character Count vs. Success ------------------------------------------------------
charAnalysis <- nchar(kikstrt$name)
charAnalysis <- as.tibble(charAnalysis)
charAnalysis$state <- as.factor(kikstrt$state)
#t-test used for simple mean comparision between two sample groups
charSuccess <- filter(charAnalysis, state == "successful")
charFailed <- filter(charAnalysis, state == "failed")
#TODO Output
ttestChar <- t.test(charSuccess$value, charFailed$value)
charAvgSuccess <- ttestChar$estimate[1]
charAvgFail <- ttestChar$estimate[2]

#General Word Count vs. Success------------------------------------------------------------
#Determine if there is a statistical difference between word count and state
wordLenAnalysis <- stringr::str_count(kikstrt$name, "\\S+")
wordLenAnalysis <- as.tibble(wordLenAnalysis)
wordLenAnalysis$state <- as.factor(kikstrt$state)
#t-test used for simple mean comparision between two sample groups
wordLenSuccess <- filter(wordLenAnalysis, state == "successful")
wordLenFail <- filter(wordLenAnalysis, state == "failed")
#TODO OUtput
ttestWord <- t.test(wordLenSuccess$value, wordLenFail$value)
wordLenAvgSuccess <- ttestWord$estimate[1]
wordLenAvgFail <- ttestWord$estimate[2]

#Name Length vs Campaign Size--------------------------------------
#Bin analysis by successful project quantiles
projSuccess <- filter(kikstrt, state == "successful")
summary(projSuccess$goal)

goalSmall <- filter(projSuccess, goal < 1250)
goalMid <- filter(projSuccess, 1250 <= goal & goal < 3923)
goalLarge <- filter(projSuccess, 3923 <= goal & goal < 10000)
goalPrem <- filter(projSuccess, 10000 <= goal)

projFail <- filter(kikstrt, state == "failed")
goalSmallFail <- filter(projFail, goal < 1250)
goalMidFail <- filter(projFail, 1250 <= goal & goal < 3923)
goalLargeFail <- filter(projFail, 3923 <= goal & goal < 10000)
goalPremFail <- filter(projFail, 10000 <= goal)

#Determine how much longer successful project name lengths are from failed projects by project size
#Step 1: Character
#Create bins that suit both success and fail

goalCharSmall <- nchar(goalSmall$name) 
goalCharSmall <- data.frame(goalCharSmall)
goalCharSmall$State <- "Success"
colnames(goalCharSmall) <- c("CharCount", "State")


goalCharSmallFail <- nchar(goalSmallFail$name)
goalCharSmallFail <- data.frame(goalCharSmallFail)
goalCharSmallFail$State <- "Fail"
colnames(goalCharSmallFail) <- c("CharCount", "State")

goalCharSmallJoin <- rbind(goalCharSmall, goalCharSmallFail)
class(goalCharSmallJoin)

ggplot2::ggplot(goalCharSmallJoin, aes(x = CharCount, colour = State)) +
  geom_histogram(fill = "white", alpha = 0.7, position = "identity")






goalCharMid <- nchar(goalMid$name)
goalCharLarge <- nchar(goalLarge$name)
goalCharPrem <- nchar(goalPrem$name)

goalCharSmallFail <- nchar(goalSmallFail$name) 
goalCharMidFail <- nchar(goalMidFail$name)
goalCharLargeFail <- nchar(goalLargeFail$name)
goalCharPremFail <- nchar(goalPremFail$name)

#T-test
#t.test(goalCharSmall, goalCharSmallFail)
#t.test(goalCharMid, goalCharMidFail)
#t.test(goalCharLarge, goalCharLargeFail)
#t.test(goalCharPrem, goalCharPremFail)

#Plot Quantile Comparision Charts






#Step 2: Word
#Compare character length of each bin ----
#TODO Include these quantile results as quantile dividers in distribution plot
#There appears to be a correlation between character length and size of project










