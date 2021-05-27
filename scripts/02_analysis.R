#TOPIC: NAME ANALYSIS #################################################################
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
wordLenAnalysis <- as_tibble(wordLenAnalysis)
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






#TOPIC: CATEGORY ANALYSIS ######################################################
#Which main_category has the highest ratio of funds pledged vs goal -----------
#Super successful projects included
catRatio <- filter(kikstrt, state == "successful")
catRatio$`pledged/goal` <- catRatio$pledged/catRatio$usd_goal_real

mainCatRatio <- function(mainCategory) {
  myCat <- mainCategory
  filterMe <- filter(catRatio, main_category == myCat)
  filterMe <- sum(filterMe$usd_pledged_real)/sum(filterMe$usd_goal_real)
  filterMe <- round(filterMe, digits = 2)
  cbind(myCat, filterMe)
  
}

mainCatRatioInput <- data.frame(x = unique(kikstrt$main_category))
colnames(mainCatRatioInput) <- "mainCategory"
mainCatRatioInput$mainCategory <- as.character(mainCatRatioInput$mainCategory)
catRatioResults <- pmap(mainCatRatioInput, mainCatRatio) 
catRatioResults <- as.data.frame(matrix(unlist(catRatioResults), nrow = 15, byrow = TRUE))
colnames(catRatioResults) <- c("Category", "pledged/goal")
#Design has the highest ratio of funds pledged per goal. 400% of goal reached
catRatioResults <- arrange(catRatioResults, desc(`pledged/goal`))









#TOPIC: SUMMARY STATS #################################################################
#Create yearly timeline of new projects ----------------------------------------
timelineKik <- kikstrt
#Set each launch day to first of month for easy visual analysis
timelineKik$launchYear <- lubridate::year(timelineKik$launched) %>%
  as.factor()

timelineStateCount <- dplyr::filter(timelineKik, state == c("successful", "failed")) %>%
  dplyr::group_by(state, launchYear) %>%
  summarise(n())

timelineStateCountPlot <-  timelineStateCount %>%
  ggplot2::ggplot(., aes(x = launchYear, y = `n()`, fill = state)) +
  geom_col(position = "dodge") +
  scale_fill_discrete(name = "State", labels = c("Successful", "Failed")) +
  xlab("Year") +
  ylab("Number of Kickstarter Campaigns") +
  labs(title = "Kickstarter Campaigns Launched by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 25000, by = 2500))

timelineStateCountPlot <- plotly::ggplotly(timelineStateCountPlot)

#Breakdown of Number of Projects per Category ----------------------------------
categorySummary <- filter(kikstrt, state == c("successful", "failed")) 
categorySummary$category <- as.factor(categorySummary$category)
categorySummary$main_category <- as.factor(categorySummary$main_category)
categorySummaryCount <- categorySummary %>%
  dplyr::group_by(main_category, category) %>%
  summarise(n())

#Plot all subcategories
subcatPlots <- function(mainCategory) {
  myMainCategory <- mainCategory
  mySubcatPlot <- dplyr::filter(categorySummaryCount,
                                main_category == myMainCategory & !category == myMainCategory) %>%
    ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`, fill = category)) +
    geom_col(position = "stack") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    xlab(paste(myMainCategory, "Subcategory")) +
    ylab("Count") +
    labs(title = paste("Number of Projects in", myMainCategory, "Subcategories 2009-2018")) +
    scale_fill_discrete()
  
  mySubcatPlot
}

subCatPlotInput <- data.frame(mainCategory = unique(kikstrt$main_category))
#as.Character for filter
subCatPlotInput$mainCategory <- as.character(subCatPlotInput$mainCategory)
subCatPlotOutput <- pmap(subCatPlotInput, subcatPlots)



#Average length of a campaign  -------------------------------------------------
daysLengthMedian <- dplyr::filter(kikstrt, state == "successful")
#Median successful campaign length is 30 days. Knowing this helps client
#plan anticipated timeline for project fundraising
daysLengthMedian <- stats::median(daysLengthMedian$projDays)





















#TOPIC: CAMPAIGN FAIL BENCHMARK ################################################
#How close do different size failed campaigns come to reaching their goal amount? ----
kikPartialFail <- rbind(kikSmall, kikMid, kikLarge, kikPrem)
kikPartialFail <- dplyr::filter(kikPartialFail, state == "failed")
kikPartialFail$partialFail <- kikPartialFail$usd_pledged_real/kikPartialFail$usd_goal_real

#Overall 159063 projects raised some money. 38656 small projects raised no money
count(filter(kikPartialFail, !partialFail == 0))
count(filter(kikPartialFail, partialFail == 0))

#Small
kikPartialFailSmall <- filter(kikPartialFail, size == "Small")
kikPartialFailSmall <-dplyr::filter(kikPartialFailSmall, !partialFail == 0)
kikPartialFailSmallZero <- dplyr::filter(kikPartialFail, partialFail == 0)
#20919 Small projects raised some money. 8046 Small projects raised no money
dplyr::count(kikPartialFailSmall)
dplyr::count(kikPartialFailSmallZero)
#Small failed projects only raise about (Average ~16.7%, Median ~10%) of 
#their required capital
mean(kikPartialFailSmall$partialFail)
stats::median(kikPartialFailSmall$partialFail)

#Mid
kikPartialFailMid <- filter(kikPartialFail, size == "Mid")
kikPartialFailMid <-dplyr::filter(kikPartialFailMid, !partialFail == 0)
kikPartialFailMidZero <- dplyr::filter(kikPartialFail, partialFail == 0)
#28682 Mid projects raised some money. 38656 Mid projects raised no money
dplyr::count(kikPartialFailMid)
dplyr::count(kikPartialFailMidZero)
#Mid failed projects only raise about (Average ~13%, Median ~6.2%) of 
#their required capital
mean(kikPartialFailMid$partialFail)
stats::median(kikPartialFailMid$partialFail)

#Large
kikPartialFailLarge <- filter(kikPartialFail, size == "Large")
kikPartialFailLarge <-dplyr::filter(kikPartialFailLarge, !partialFail == 0)
kikPartialFailLargeZero <- dplyr::filter(kikPartialFail, partialFail == 0)
#36909 Large projects raised some money. 38656 Large projects raised no money
dplyr::count(kikPartialFailLarge)
dplyr::count(kikPartialFailLargeZero)
#Large failed projects only raise about (Average ~11%, Median ~4.3%) of 
#their required capital
mean(kikPartialFailLarge$partialFail)
stats::median(kikPartialFailLarge$partialFail)

#Plot Distribution of Partially Raised Funds -----------------------------------
partialFailPlot <- function(size) {
  mySize <- size
  myCut <- filter(kikPartialFail, size == mySize)
  myCut <- cut(myCut$partialFail, breaks = seq(0,1, by = 0.1)) %>%
    table %>%
    data.frame
  colnames(myCut) <- c("Breaks", "Count")
  
  myPlot <- ggplot2::ggplot(myCut, aes(x = Breaks, y = Count, fill = Breaks)) +
    geom_histogram(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete() +
    xlab("Percent Breaks") +
    ylab("Number of Kickstarter Campaigns") +
    labs(title = paste("Percent of Goal Amount Raised for", mySize, "Failed Projects"))
  return(myPlot)
}

partialFailPlotInputs <- data.frame(size = c("Small", "Mid", "Large", "Prem"))
pmap(partialFailPlotInputs, partialFailPlot)





#TOPIC: OPTIMAL GOAL AMOUNT ####################################################
#Expected Pledge Amount Per Backer Based On Project Size And Category ----------
#Given a goal fundraising amount in a particular category,
#output the expected number of backers and expected donation amount per backer


#Step 3: Estimate the average number of backers, and their estimated contribution
#per backer for a given goal amount
#Goal amount will be used to classify project size and select appropriate
#goalExceedRatio to define breakout success
#If goal range exceeds limits of size classification, then the additional points =
#will be used and not excluded
#Range will be defined as +- 20% goalExceedRatio

#Inputs: myGoalAmount
backerStatsFun <- function(myGoalAmount) {
  if(myGoalAmount < 1302) {
    myRange <-  filter(breakoutFilter,
                       myGoalAmount*0.8 <= usd_goal_real,
                       usd_goal_real <= myGoalAmount*1.2) 
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
      myRange <-  filter(breakoutFilter,
                         myGoalAmount*0.8 <= usd_goal_real,
                         usd_goal_real <= myGoalAmount*1.2) 
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
        myRange <-  filter(breakoutFilter,
                           myGoalAmount*0.8 <= usd_goal_real,
                           usd_goal_real <= myGoalAmount*1.2) 
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
          myRange <-  filter(breakoutFilter,
                             myGoalAmount*0.8 <= usd_goal_real,
                             usd_goal_real <= myGoalAmount*1.2) 
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
    myRange <-  filter(breakoutFilter, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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
      myRange <-  filter(breakoutFilter, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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
        myRange <-  filter(breakoutFilter, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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
          myRange <-  filter(breakoutFilter, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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

#We see a log decreasing (?) function
#On average, more backers actually mean a higher average contribution
#This implies that the strategy for successful projects is getting higher donations per backer
#Not a whole bunch of backers who donate small amounts
#Is this accurate? How can I validate???
#Wait i need to plot this against goal size
#The number of backers does not follow a linear model when GoalAmount goes up!!
#This is the key insight. On average you dont attract more backers, you attract
#backers with more money
#Why arent really big projects just getting funded by 10000 people with $1?
#Why why why???? Maybe this has to do with kickerstarter tiers encouraging 
#people to give more money EDIT:investigate this
#So naturally if its not a 1 to 1 then less people need to increase their average
#contribution size in order for a project to be successful
#ohhh so if its a linear model then 1 (2500,45) (5000, 75)























