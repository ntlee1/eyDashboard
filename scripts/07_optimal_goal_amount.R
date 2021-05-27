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
view(breakoutFilter)

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





#Step 3: Estimate the average number of backers, and their estimated contribution
#per backer for a given goal amount
#Goal amount will be used to classify project size and select appropriate
#goalExceedRatio to define breakout success
#If goal range exceeds limits of size classification, then the additional points 
#will be used and not excluded
#Range will be defined as +- 20% goalExceedRatio

#Inputs: myGoalAmount
test <- function(myGoalAmount) {
  if(myGoalAmount < 1302)
   myFilter <-  filter(breakoutFilter, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
  backerAvgPledge <- (mean(myFilter$usd_pledged_real)/mean(myFilter$backers)) %>%
    round(., digits = 2)
  backerAvgCount <- mean(myFilter$backers) %>%
    round(., digits = 0)
  paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","), "the average number of backers is", backerAvgCount, "and the average contribution per backer is", paste0("$", backerAvgPledge, "."))
}




test(300)






breakoutFilterSmall <- projSmall
breakoutFilterSmall <- filter(breakoutFilterSmall, state == "successful")
breakoutFilterSmall$goalExceedRatio <- breakoutFilterSmall$usd_pledged_real/breakoutFilterSmall$usd_goal_real

breakoutFilterSmallSummary<- summary(breakoutFilterSmall$goalExceedRatio)
breakoutFilterSmallIQR <- (breakoutFilterSmallSummary[5] -  breakoutFilterSmallSummary[2]) %>%
  as.numeric

breakoutFilterSmall <- filter(breakoutFilterSmall, goalExceedRatio <=  breakoutFilterSmallIQR)


#Create function 
#Inputs: Goal Range, Category
#Outputs: Estimated number of backers, estimated dollars per backer
#Success Quantiles 1302, 3838, 10000
catNmsBreakout <- unique(categorySummaryCount$main_category) %>%
  data.frame
colnames(catNmsBreakout) <- c("Category")


test <- filter(breakoutFilterSmall, 800 <= usd_goal_real, usd_goal_real <= 1000,
       category == catNmsBreakout[1,])
testMean <- mean(test$backers)
testRaiseAvg <- mean(test$usd_pledged_real)
testDollarsPerBacker <- testRaiseAvg/testMean


function(goalAmount, catIndex) {
  if(goalAmount < 1302) {
    mySmallBreakoutFilter <- filter(breakoutFilterSmall,
                                    goalAmount*0.8 <= usd_goal_real,
                                    usd_goal_real <= goalAmount*1.2,
                   category == catNmsBreakout[catIndex,])
    myMean <- mean(test$backers)
    testRaiseAvg <- mean(test$usd_pledged_real)
    testDollarsPerBacker <- testRaiseAvg/myMean
  }

  
}