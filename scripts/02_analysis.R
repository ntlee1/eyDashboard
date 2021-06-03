#TOPIC: NAME ANALYSIS ##########################################################
#Determine the optimal name characteristics for a campaign.
#For this analysis we ignore state == c(live, suspended, canceled).
#These ambiguous outcomes not insightful for this use case

#Do Successful Project Names Differ in Their Character Count? ------------------
Kik$char <- nchar(Kik$kiksrt$name)
Kik$char <- dplyr::as_tibble(Kik$char)
Kik$char$state <- Kik$kiksrt$state

#t-test used for simple mean comparison between two sample groups
Kik$charSucc <- dplyr::filter(Kik$char, state == "successful")
Kik$charFail <- dplyr::filter(Kik$char, state == "failed")
Kik$ttestChar <- stats::t.test(Kik$charSucc$value, Kik$charFail$value)
#Successful campaign names average ~36 characters. Failed campaign names
#average ~33 characters
Kik$CharAvgSucc <- Kik$ttestChar$estimate[1] %>%
  round(., digits = 0)
Kik$CharAvgFail <- Kik$ttestChar$estimate[2] %>%
  round(., digits = 0)

#Compare name, character/word length, and state by size of campaign
#By number of characters
Kik$charKik$kiksrt <- Kik$kiksrt
Kik$charKik$kiksrt$charCt <- nchar(Kik$charKik$kiksrt$name)
Kik$charKik$kiksrt <- dplyr::filter(Kik$charKik$kiksrt,
                                    state == "successful" | state == "failed")

#Small projects tend to have shorter names, large and premium projects tend to 
#have longer names, especially premium
Kik$charPlot <-
  ggplot2::ggplot() +
  geom_line(data = dplyr::filter(Kik$charKik$kiksrt, 
                                 size == "Small", state == "successful"), 
            aes(x = charCt, colour = size),
            stat = "bin", size = 1) +
  geom_line(data = dplyr::filter(Kik$charKik$kiksrt,
                                 size == "Mid", state == "successful"),
            aes(x = charCt, colour = size),
            stat = "bin", size = 1) +
  geom_line(data = dplyr::filter(Kik$charKik$kiksrt, 
                                 size == "Large", state == "successful"),
            aes(x = charCt, colour = size),
            stat = "bin", size = 1) +
  geom_line(data = dplyr::filter(Kik$charKik$kiksrt, 
                                 size == "Prem", state == "successful"),
            aes(x = charCt, colour = size),
            stat = "bin", size = 1) +
  xlab("Kickstarter Name Length (characters)") +
  ylab("Number of Kickstarter Projects") +
  labs(title = "Length of Kickstarter Project Names by Project Size") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete()
Kik$charPlot

#Name length recommender -------------------------------------------------------
#Purpose: Give optimal project name length based on campaign goal
#Appropriate linear model selected based on goal size
Kik$optNmLen <- function (goalAmount) {
  if(5 < goalAmount & goalAmount < 1302) {
    lmCharSmall <- stats::lm(charCt~usd_goal_real,
                      data = dplyr::filter(Kik$charKik$kiksrt,
                                    size == "Small",
                                    state == "successful"))
    goalSmallMe <-  as.numeric(lmCharSmall$coefficients[1] +
                                 lmCharSmall$coefficients[2]*goalAmount) %>%
      round(digits = 0)
    paste("The optimal number of characters for your project name is",
          goalSmallMe)
  } else {
    if(1302 <= goalAmount & goalAmount < 3838) {
      lmCharMid <- stats::lm(charCt~usd_goal_real,
                      data = dplyr::filter(Kik$charKik$kiksrt,
                                    size == "Mid",
                                    state == "successful"))
      goalMidMe <-  as.numeric(lmCharMid$coefficients[1] +
                                 lmCharMid$coefficients[2]*goalAmount) %>%
        round(digits = 0)
      paste("The optimal number of characters for your project name is",
            goalMidMe)
    } else {
      if(3838 <= goalAmount & goalAmount < 10000) {
        lmCharLarge <- stats::lm(charCt~usd_goal_real,
                          data = dplyr::filter(Kik$charKik$kiksrt,
                                        size == "Large",
                                        state == "successful"))
        goalLargeMe <-  as.numeric(lmCharLarge$coefficients[1] +
                                     lmCharLarge$coefficients[2]*goalAmount) %>%
          round(digits = 0)
        paste("The optimal number of characters for your project name is",
              goalLargeMe)
      } else {
        if(10000 <= goalAmount & goalAmount < 775000) {
          lmCharPrem <- stats::lm(charCt~usd_goal_real,
                           data = dplyr::filter(Kik$charKik$kiksrt,
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

#What words are associated with each category? ---------------------------------
Kik$kikNm <- dplyr::tibble(name = Kik$kiksrt$name)
Kik$kikNmTkn <- tidytext::unnest_tokens(Kik$kikNm, word, name) 

Kik$kikNmTkn <- Kik$kikNmTkn %>%
  dplyr::anti_join(stop_words)
Kik$kikNmTkn %>%
dplyr::count(word, sort = TRUE)
#Canceled projects have word "canceled in name
Kik$customStopWords <- bind_rows(tibble(word = c("canceled"),
                             lexicon = c("custom")),
                      stop_words)
Kik$kikNmTkn <- Kik$kikNmTkn %>%
  dplyr::anti_join(Kik$customStopWords)
#Top words overall include album, film, project
Kik$kikNmTkn %>%
  dplyr::count(word, sort = TRUE)

#Custom category #TODO
Kik$kikNmTknFood <- dplyr::filter(Kik$kiksrt,
                                  main_category == "Food", state == "successful")
Kik$kikNmTknFood <- dplyr::tibble(name = Kik$kikNmTknFood$name)
Kik$kikNmTknFood <- tidytext::unnest_tokens(Kik$kikNmTknFood, word, name)
Kik$kikNmTknFood <- Kik$kikNmTknFood %>%
  dplyr::anti_join(Kik$customStopWords)
#Top word is food. It's good to be literal with your name
Kik$kikNmTknFood %>%
  dplyr::count(word, sort = TRUE) %>%
  view

Kik$kikNmTknFood %>%
  count(word) %>%
  with(wordcloud::wordcloud(word, n, max.words = 50))

testy <- tidytext::stop_words 

#TOPIC: CATEGORY ANALYSIS ######################################################
#Which main_category has the highest ratio of funds pledged vs goal ------------
#Super successful projects included
Kik$catRatio <- dplyr::filter(Kik$kiksrt, state == "successful")
Kik$catRatio$`pledged/goal` <- Kik$catRatio$pledged/Kik$catRatio$usd_goal_real

Kik$mainCatRatio <- function(mainCategory) {
  myCat <- mainCategory
  filterMe <- dplyr::filter(Kik$catRatio, main_category == myCat)
  filterMe <- sum(filterMe$usd_pledged_real)/sum(filterMe$usd_goal_real)
  filterMe <- round(filterMe, digits = 2)
  cbind(myCat, filterMe)
  
}

Kik$mainCatRatioInput <- data.frame(x = unique(Kik$kiksrt$main_category))
colnames(Kik$mainCatRatioInput) <- "mainCategory"
Kik$mainCatRatioInput$mainCategory <- as.character(Kik$mainCatRatioInput$mainCategory)
Kik$catRatioResults <- pmap(Kik$mainCatRatioInput, Kik$mainCatRatio) 
Kik$catRatioResults <- as.data.frame(matrix(unlist(Kik$catRatioResults),
                                            nrow = 15, byrow = TRUE))
colnames(Kik$catRatioResults) <- c("Category", "pledged/goal")
#Design has the highest ratio of funds pledged per goal. 400% of goal reached
Kik$catRatioResults <- arrange(Kik$catRatioResults, desc(`pledged/goal`))

#TOPIC: SUMMARY STATS ##########################################################
#Create yearly timeline of new projects ----------------------------------------
Kik$timeline <- Kik$kiksrt
#Set each launch day to first of month for easy visual analysis
Kik$timeline$launchYear <- lubridate::year(Kik$timeline$launched) %>%
  as.factor()

Kik$timelineStCt <- dplyr::filter(Kik$timeline, state == c("successful", "failed")) %>%
  dplyr::group_by(state, launchYear) %>%
  summarise(n())

Kik$timelineStCtPlot <-  Kik$timelineStCt %>%
  ggplot2::ggplot(., aes(x = launchYear, y = `n()`, fill = state)) +
  geom_col(position = "dodge") +
  scale_fill_discrete(name = "State", labels = c("Successful", "Failed")) +
  xlab("Year") +
  ylab("Number of Kickstarter Campaigns") +
  labs(title = "Kickstarter Campaigns Launched by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 25000, by = 2500))

Kik$timelineStCtPlot <- plotly::ggplotly(Kik$timelineStCtPlot)

#Breakdown of Number of Projects per Category ----------------------------------
Kik$catSmry <- dplyr::filter(Kik$kiksrt, state == "successful" | state == "failed") 
Kik$catSmry$category <- as.factor(Kik$catSmry$category)
Kik$catSmry$main_category <- as.factor(Kik$catSmry$main_category)
Kik$catSmryCount <- Kik$catSmry %>%
  dplyr::group_by(main_category, category) %>%
  summarise(n()) 

#Plot all subcategories
Kik$subcatPlot <- function(mainCategory) {
  myMainCategory <- mainCategory
  mySubcatPlot <- dplyr::filter(Kik$catSmryCount,
                                main_category == myMainCategory &
                                  !category == myMainCategory) %>%
    ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`,
                           fill = category)) +
    geom_col(position = "stack") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    xlab(paste(myMainCategory, "Subcategory")) +
    ylab("Count") +
    labs(title = paste("Number of Projects in", myMainCategory,
                       "Subcategories 2009-2018")) +
    scale_fill_discrete()
  
  mySubcatPlot
}

Kik$subCatPlotInput <- data.frame(mainCategory = unique(Kik$kiksrt$main_category))
#as.Character for dplyr::filter
Kik$subCatPlotInput$mainCategory <- as.character(Kik$subCatPlotInput$mainCategory)
Kik$subCatPlotOutput <- pmap(Kik$subCatPlotInput, Kik$subcatPlot)



#Average length of a campaign  -------------------------------------------------
Kik$projLenMedian <- dplyr::filter(Kik$kiksrt, state == "successful")
#Median successful campaign length is 30 days. Knowing this helps client
#plan anticipated timeline for project fundraising
Kik$projLenMedian <- stats::median(Kik$projLenMedian$projLen)


#TOPIC: CAMPAIGN FAIL BENCHMARK ################################################
#How close do different size failed campaigns come to reaching their goal amount? ----
Kik$kikPartialFail <- Kik$kiksrt
Kik$kikPartialFail <- dplyr::filter(Kik$kikPartialFail, state == "failed")
Kik$kikPartialFail$partialFail <- Kik$kikPartialFail$usd_pledged_real/Kik$kikPartialFail$usd_goal_real

#Overall ~159k projects raised some money. ~40k small projects raised no money
count(dplyr::filter(Kik$kikPartialFail, !partialFail == 0))
count(dplyr::filter(Kik$kikPartialFail, partialFail == 0))

#Small
Kik$kikPartialFailSmall <- dplyr::filter(Kik$kikPartialFail, size == "Small",
                                         !partialFail == 0)
Kik$kikPartialFailSmallZero <- dplyr::filter(Kik$kikPartialFail, size == "Small",
                                             partialFail == 0)
#~21k Small projects raised some money. ~8k Small projects raised no money
dplyr::count(Kik$kikPartialFailSmall)
dplyr::count(Kik$kikPartialFailSmallZero)
#Small failed projects only raise about (Average ~16.7%, Median ~10%) of 
#their required capital
stats::mean(Kik$kikPartialFailSmall$partialFail)
stats::median(Kik$kikPartialFailSmall$partialFail)

#Mid
Kik$kikPartialFailMid <- dplyr::filter(Kik$kikPartialFail, size == "Mid",
                                       !partialFail == 0)
Kik$kikPartialFailMidZero <- dplyr::filter(Kik$kikPartialFail, size == "Mid", 
                                           partialFail == 0)
#~28k Mid projects raised some money. ~7.3k Mid projects raised no money
dplyr::count(Kik$kikPartialFailMid)
dplyr::count(Kik$kikPartialFailMidZero)
#Mid failed projects only raise about (Average ~13%, Median ~6.2%) of 
#their required capital
stats::mean(Kik$kikPartialFailMid$partialFail)
stats::median(Kik$kikPartialFailMid$partialFail)

#Large
Kik$kikPartialFailLarge <- dplyr::filter(Kik$kikPartialFail, size == "Large",
                                         !partialFail == 0)
Kik$kikPartialFailLargeZero <- dplyr::filter(Kik$kikPartialFail, size == "Large", 
                                             partialFail == 0)
#~36k Large projects raised some money. ~8.5k Large projects raised no money
dplyr::count(Kik$kikPartialFailLarge)
dplyr::count(Kik$kikPartialFailLargeZero)
#Large failed projects only raise about (Average ~11%, Median ~4.3%) of 
#their required capital
stats::mean(Kik$kikPartialFailLarge$partialFail)
stats::median(Kik$kikPartialFailLarge$partialFail)

#Prem
Kik$kikPartialFailPrem <- dplyr::filter(Kik$kikPartialFail, size == "Prem",
                                         !partialFail == 0)
Kik$kikPartialFailPremZero <- dplyr::filter(Kik$kikPartialFail, size == "Prem", 
                                             partialFail == 0)
#~72k Prem projects raised some money. ~15k Prem projects raised no money
dplyr::count(Kik$kikPartialFailPrem)
dplyr::count(Kik$kikPartialFailPremZero)
#Prem failed projects only raise about (Average ~9%, Median ~2%) of 
#their required capital
stats::mean(Kik$kikPartialFailPrem$partialFail)
stats::median(Kik$kikPartialFailPrem$partialFail)


#Plot Distribution of Partially Raised Funds -----------------------------------
Kik$partialFailPlot <- function(size) {
  mySize <- size
  myCut <- dplyr::filter(Kik$kikPartialFail, size == mySize)
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
    labs(title = paste("Percent of Goal Amount Raised for", mySize,
                       "Failed Projects"))
  return(myPlot)
}

Kik$partialFailPlotInputs <- data.frame(size = c("Small", "Mid", "Large", "Prem"))
purrr::pmap(Kik$partialFailPlotInputs, Kik$partialFailPlot)


#TOPIC: OPTIMAL GOAL AMOUNT ####################################################
#Expected Pledge Amount Per Backer Based On Project Size And Category ----------
#Given a goal fundraising amount in a particular category,
#output the expected number of backers and expected donation amount per backer


#Step 3: Estimate the average number of backers, and their estimated contribution
#per backer for a given goal amount
#Goal amount will be used to classify project size and select appropriate
#goalExceedRatio to define breakout success
#If goal range exceeds limits of size classification, then the additional 
#points will be used and not excluded
#Range will be defined as +- 20% goalExceedRatio in order to include 
#enough observations

#Inputs: myGoalAmount
Kik$bkrStatsFun <- function(myGoalAmount) {
  if(myGoalAmount < 1302) {
    myRange <-  dplyr::filter(Kik$brkout,
                       myGoalAmount*0.8 <= usd_goal_real,
                       usd_goal_real <= myGoalAmount*1.2) 
    backerAvgPledge <- (stats::mean(myRange$usd_pledged_real)/stats::mean(myRange$backers)) %>%
      round(., digits = 2)
    backerAvgCount <- stats::mean(myRange$backers) %>%
      round(., digits = 0)
    paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","),
          "the average number of backers is", backerAvgCount,
          "and the average contribution per backer is",
          paste0("$", backerAvgPledge, "."))
  } else {
    if(1302 <= myGoalAmount & myGoalAmount < 3838) {
      myRange <-  dplyr::filter(Kik$brkout,
                         myGoalAmount*0.8 <= usd_goal_real,
                         usd_goal_real <= myGoalAmount*1.2) 
      backerAvgPledge <- (stats::mean(myRange$usd_pledged_real)/stats::mean(myRange$backers)) %>%
        round(., digits = 2)
      backerAvgCount <- stats::mean(myRange$backers) %>%
        round(., digits = 0)
      paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","),
            "the average number of backers is", backerAvgCount,
            "and the average contribution per backer is",
            paste0("$", backerAvgPledge, "."))
    } else {
      if(3838 <= myGoalAmount & myGoalAmount < 10000) {
        myRange <-  dplyr::filter(Kik$brkout,
                           myGoalAmount*0.8 <= usd_goal_real,
                           usd_goal_real <= myGoalAmount*1.2) 
        backerAvgPledge <- (stats::mean(myRange$usd_pledged_real)/stats::mean(myRange$backers)) %>%
          round(., digits = 2)
        backerAvgCount <- stats::mean(myRange$backers) %>%
          round(., digits = 0)
        paste("For a kickstarter goal of", paste0("$", myGoalAmount, ","),
              "the average number of backers is",
              backerAvgCount, "and the average contribution per backer is",
              paste0("$", backerAvgPledge, ".")) 
      } else {
        if(10000 <= myGoalAmount) {
          myRange <-  dplyr::filter(Kik$brkout,
                             myGoalAmount*0.8 <= usd_goal_real,
                             usd_goal_real <= myGoalAmount*1.2) 
          backerAvgPledge <- (stats::mean(myRange$usd_pledged_real)/stats::mean(myRange$backers)) %>%
            round(., digits = 2)
          backerAvgCount <- stats::mean(myRange$backers) %>%
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
Kik$goalAmtFunPlot <- function(myGoalAmount) {
  if(myGoalAmount < 1302) {
    myRange <-  dplyr::filter(Kik$brkout, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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
      myRange <-  dplyr::filter(Kik$brkout, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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
        myRange <-  dplyr::filter(Kik$brkout, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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
          myRange <-  dplyr::filter(Kik$brkout, myGoalAmount*0.8 <= usd_goal_real, usd_goal_real <= myGoalAmount*1.2) 
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


#Create four different plots depending on the size of the project #EDIT
#Plot a subset of points then connect
#Small seq(0,1302, by 100)
#TODO Cut by an even number of points
#TODO Fix variable names
Kik$goalFunSeq <- seq(1, 700000, by = 100) %>%
  data.frame
colnames(Kik$goalFunSeq) <- c("myGoalAmount")
Kik$goalFunMap <- pmap(Kik$goalFunSeq, Kik$goalAmtFunPlot) %>%
  unlist %>%
  as.numeric

length(Kik$goalFunMap)
#Average Pledge

Kik$goalFunSeqData <- data.frame(x = Kik$goalFunMap[seq(1, 14000, by = 2)],
                             x1 = Kik$goalFunMap[seq(0, 14000, by = 2)[-1]])
Kik$goalFunSeqData$GoalAmount <- seq(1, 700000, by = 100)
colnames(Kik$goalFunSeqData) <- c("BackerAvgPledge", "BackerAvgCount", "GoalAmount")


#The more backers a project has, the more money they contribute??
#This could make sense because successful projects need to meet 100% of their goal
#in order to be funded
#So large projects need more backers who contribute increasing amounts of more money
#One would think that per contribution numbers would be more stagnant





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























