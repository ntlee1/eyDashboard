#Expected Pledge Amount Per Backer Based On Project Size And Category ----------

#Step 0: Given a goal fundraising amount in a particular category,
#output the expected number of backers and expected donation amount per backer

#Step 1: Find and filter out breakout success kickstarter campaigns 
#Reason: We want to model realistic campaigns, not the breakout successes
#Define breakout success as median of goalExceedRatio
#Only analyze state == success
breakoutFilter <- kikstrt
breakoutFilter <- filter(breakoutFilter, state == "successful")
breakoutFilter$goalExceedRatio <- breakoutFilter$usd_pledged_real/breakoutFilter$usd_goal_real
breakoutFilterSummary <- summary(breakoutFilter$goalExceedRatio)
#view(breakoutFilter)

#Step 2: Find median for each project size classification
#Success Quantiles 1302, 3838, 10000
#Larger campaigns will naturally have lower goalExceedRatio multipliers 
#which brings down the median and what can be considered as a breakout success
breakoutSmall <- filter(breakoutFilter, breakoutFilter$usd_goal_real < 1302)
breakoutMid <- filter(breakoutFilter, 1302 <= breakoutFilter$usd_goal_real,
                      breakoutFilter$usd_goal_real < 3838)
breakoutLarge <- filter(breakoutFilter, 3838 <= breakoutFilter$usd_goal_real,
                        breakoutFilter$usd_goal_real < 10000)
breakoutPrem <- filter(breakoutFilter, 10000 <= usd_goal_real)

breakoutSmallMedian <- summary(breakoutSmall$goalExceedRatio)[3]  %>%
  as.numeric
breakoutMidMedian <- summary(breakoutMid$goalExceedRatio)[3]  %>%
  as.numeric
breakoutLargeMedian <- summary(breakoutLarge$goalExceedRatio)[3]  %>%
  as.numeric
breakoutPremMedian <- summary(breakoutPrem$goalExceedRatio)[3]  %>%
  as.numeric

breakoutSmall <- filter(breakoutSmall, goalExceedRatio <= breakoutSmallMedian)
breakoutSmall$Size <- "Small"
breakoutMid <- filter(breakoutMid, goalExceedRatio <= breakoutMidMedian)
breakoutMid$Size <- "Mid"
breakoutLarge <- filter(breakoutLarge, goalExceedRatio <= breakoutLargeMedian)
breakoutLarge$Size <- "Large"
breakoutPrem <- filter(breakoutPrem, goalExceedRatio <= breakoutPremMedian)
breakoutPrem$Size <- "Prem"

breakoutFilterDf <- rbind(breakoutSmall,
                          breakoutMid,
                          breakoutLarge,
                          breakoutPrem) %>%
  as.data.frame

#Step 3: Estimate the average number of backers, and their estimated contribution
#per backer for a given goal amount
#Goal amount will be used to classify project size and select appropriate
#goalExceedRatio to define breakout success
#If goal range exceeds limits of size classification, then the additional points =
#will be used and not excluded
#Range will be defined as +- 20% goalExceedRatio

#Inputs: myGoalAmount
goalAmountFunction <- function(myGoalAmount) {
  if(myGoalAmount < 1302) {
    myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
    backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
      round(., digits = 2)
    backerAvgCount <- mean(myRange$backers) %>%
      round(., digits = 0)
    paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","),
          "the average number of backers is", backerAvgCount,
          "and the average contribution per backer is",
          paste0("$", backerAvgPledge, "."))
  } else {
    if(1302 <= myGoalAmount & myGoalAmount < 3838) {
      myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
  
      backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
        round(., digits = 2)
      backerAvgCount <- mean(myRange$backers) %>%
        round(., digits = 0)
      paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","),
            "the average number of backers is", backerAvgCount,
            "and the average contribution per backer is",
            paste0("$", backerAvgPledge, "."))
    } else {
      if(3838 <= myGoalAmount & myGoalAmount < 10000) {
        myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
       
        backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
          round(., digits = 2)
        backerAvgCount <- mean(myRange$backers) %>%
          round(., digits = 0)
        paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","),
              "the average number of backers is",
              backerAvgCount, "and the average contribution per backer is",
              paste0("$", backerAvgPledge, ".")) 
      } else {
        if(10000 <= myGoalAmount) {
          myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
          
          backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
            round(., digits = 2)
          backerAvgCount <- mean(myRange$backers) %>%
            round(., digits = 0)
          paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","),
                "the average number of backers is", backerAvgCount,
                "and the average contribution per backer is",
                paste0("$", backerAvgPledge, ".")) 
        }
      }
    }
  }
}

#Plot myGoalAmount ----------
#Enable function to give me outputs needed to create plot
goalAmountFunctionPlot <- function(myGoalAmount) {
  if(myGoalAmount < 1302) {
    myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
    backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
      round(., digits = 2)
    backerAvgCount <- mean(myRange$backers) %>%
      round(., digits = 0)
    myDf <- data.frame(x = backerAvgPledge,
                       x1 = backerAvgCount)
    colnames(myDf) <- c("backerAvgPledge", "backerAvgCount")
    myDf
  } else {
    if(1302 <= myGoalAmount & myGoalAmount < 3838) {
      myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
      backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
        round(., digits = 2)
      backerAvgCount <- mean(myRange$backers) %>%
        round(., digits = 0)
      myDf <- data.frame(x = backerAvgPledge,
                         x1 = backerAvgCount)
      colnames(myDf) <- c("backerAvgPledge", "backerAvgCount")
      myDf
    } else {
      if(3838 <= myGoalAmount & myGoalAmount < 10000) {
        myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
        backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
          round(., digits = 2)
        backerAvgCount <- mean(myRange$backers) %>%
          round(., digits = 0)
        myDf <- data.frame(x = backerAvgPledge,
                           x1 = backerAvgCount)
        colnames(myDf) <- c("backerAvgPledge", "backerAvgCount")
        myDf
      } else {
        if(10000 <= myGoalAmount) {
          myRange <-  filter(breakoutFilterDf, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
          backerAvgPledge <- (mean(myRange$usd_pledged_real)/mean(myRange$backers)) %>%
            round(., digits = 2)
          backerAvgCount <- mean(myRange$backers) %>%
            round(., digits = 0)
          myDf <- data.frame(x = backerAvgPledge,
                             x1 = backerAvgCount)
          colnames(myDf) <- c("backerAvgPledge", "backerAvgCount")
          myDf
        }
      }
    }
  }
}


#Create four different plots depending on the size of the project
#Plot a subset of points then connect
#Small seq(0,1302, by 100)
#TODO Cut by an even number of points
#TODO Fix variable names
goalFunSeq <- seq(1, 700000, by = 100) %>%
  data.frame
colnames(goalFunSeq) <- c("myGoalAmount")
goalFunMap <- pmap(goalFunSeq, goalAmountFunctionPlot) %>%
  unlist %>%
  as.numeric

length(goalFunMap)
#Average Pledge

goalFunSeqData <- data.frame(x = goalFunMap[seq(1, 14000, by = 2)],
                           x1 = goalFunMap[seq(0, 14000, by = 2)[-1]])
goalFunSeqData$GoalAmount <- seq(1, 700000, by = 100)
colnames(goalFunSeqData) <- c("BackerAvgPledge", "BackerAvgCount", "GoalAmount")


#The more backers a project has, the more money they contribute??
#This could make sense because successful projects need to meet 100% of their goal
#in order to be funded
#So large projects need more backers who contribute increasing amounts of more money
#One would think that per contribution numbers would be more stagnant

ggplot2::ggplot(goalFunSeqData, aes(x = BackerAvgCount, y = BackerAvgPledge)) +
  geom_point()

ggplot2::ggplot(goalFunSeqData, aes(y = BackerAvgCount, x = GoalAmount)) +
  geom_point()

ggplot2::ggplot(goalFunSeqData, aes(y = BackerAvgPledge, x = GoalAmount)) +
  geom_point()


##################
#ITS WRONG BECAUSE I HAVENT FILTERED OUT THE EXTREME SMALL AND LARGE PROJECTS






















