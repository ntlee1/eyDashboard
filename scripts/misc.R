#Code I may use. Please ignore--------------------------------------------------
#Not having to use literal deparse substitute
myCatNms <- data.frame(x = catNms)
myCatNms$x <- as.character(myCatNms$x)
myCatNms[1,]

hee <- "Art"

heyo <- myCatNms[myCatNms$x == hee,]
eval(substitute(heyo))

catNms


subcatPlots <- function(subcatName) {
  
  mySubcat <- deparse(substitute(subcatName))
  
  filter(categorySummaryCount, main_category == mySubcat & !category == mySubcat) %>%
    ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`, fill = category)) +
    geom_col(position = "stack") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    xlab(paste(mySubcat, "Subcategory")) +
    ylab("Count") +
    labs(title = paste("Number of Projects in", mySubcat, "Subcategories 2009-2018")) +
    scale_fill_discrete()
}


#Copy to notes
mainCatRatio <- function(mainCategory) {
  catRatio <- filter(kikstrt, state == "successful")
  catRatio$`pledged/goal` <- catRatio$pledged/catRatio$usd_goal_real
  
  mainCatRatioInput <- data.frame(x = unique(kikstrt$main_category))
  colnames(mainCatRatioInput) <- "mainCategory"
  
  #Convert input name to character to find matching index in possible arguments
  #Input is seen as a variable, convert that variable to character
  myCatIndex <- which(mainCatRatioInput$mainCategory == deparse(substitute(mainCategory)))
  myCat <-mainCatRatioInput$mainCategory[myCatIndex]
  
  filterMe <- filter(catRatio, main_category == myCat)
  filterMe <- sum(filterMe$usd_pledged_real)/sum(filterMe$usd_goal_real)
  return(list(filterMe, myCat))
}




Kik$duplicateIdKik <- dplyr::filter(Kik$kiksrt, ID %in% Kik$duplicateId$Var1)
#Remove duplicate data
Kik$kiksrt <- Kik$kiksrt[!duplicateinstd(Kik$kiksrt[,c("ID")]),]
#TRUE
length(Kik$kiksrt$ID) == length(unique(Kik$kiksrt$ID))


#Name length recommender
#Purpose: Give optimal project name length based on campaign goal
#Appropriate linear model selected based on goal size
#This function is may need to refine its small goal filter
Kik$optNmLen <- function (goalAmount, data = Kik$charKik$kiksrt) {
  if(5 < goalAmount & goalAmount < 1302) {
    lmCharSmall <- stats::lm(charCt~usd_goal_real,
                             data = dplyr::filter(data,
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
                             data = dplyr::filter(data,
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
                                 data = dplyr::filter(data,
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
                                  data = dplyr::filter(data,
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
      myRange <-  dplyr::filter(Kik$brkout,
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
        myRange <-  dplyr::filter(Kik$brkout,
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
          myRange <-  dplyr::filter(Kik$brkout,
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









0

test <- function(mainCat) {
  myMainCat <- mainCat
  
  myTkn <- dplyr::filter(Kik$kiksrt,
                         main_category == myMainCat, state == "successful",
                         currency == "SGD")
  myTkn <- dplyr::tibble(name = myTkn$name) %>%
    tidytext::unnest_tokens(., word, name) 
  myTkn <- myTkn %>%
    dplyr::anti_join(Kik$customStopWords)
  
  myTknTable <- myTkn %>%
    dplyr::count(word, sort = TRUE)
  myTknTable <- myTknTable[1:100,]
  return(myTknTable)
  
}
test("Food")

Kik$tknFxRank("Art", "SGD", "NOK")
















#Global vars will be reused often throughout scripts ---------------------------
#Useful vars stored here for easy reference and reuse throughout project
#Var: Filters
Kik$kikSuccess <- dplyr::filter(Kik$kiksrt, state == "successful")
















Kik$NmTknFun("Food")







layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Title of my first plot")


Kik$NmTknMainPlotOutput[[1]]

Kik$subCatPlotOutput[[1]]





#Maybe a direct call is eaiser in shiny
Kik$NmTknMainPlot("Food")




kikSize <- "All Projects"
myHeight <- 
  myWidth <- 100


Kik$timelineStCt <- dplyr::filter(Kik$timeline, state == c("successful", "failed")) %>%
  dplyr::group_by(state, launchYear) %>%
  summarise(n())




myPlot <-  Kik$timelineStCt %>%
  ggplot2::ggplot(., aes(x = launchYear, y = `n()`, fill = state)) +
  geom_col(position = "dodge", color = "#222A35") +
  Kik$ggAutoTheme +
  Shy$plotColsEy +
  scale_fill_manual(values = c(failed = "red", successful = "green"),
                    labels = c("Failed", "Successful"),
                    name = "State") +
  xlab("Year") +
  ylab(stringr::str_wrap("Number of Kickstarter Campaigns", width = 25)) +
  labs(title = stringr::str_wrap("All Kickstarter Campaigns Launched by Year",
                                 width = 60)) +
  theme(plot.margin = unit(c(2,2,2,2), "cm"),
        axis.text.x = element_text(size = 4)) 

##################


myPlot <-  Kik$timelineStCt %>%
  ggplot2::ggplot(., aes(x = launchYear, y = `n()`, fill = state)) +
  geom_col(position = "dodge", color = "#222A35") +
  theme(axis.text.x = element_text(size = ))

myPlot


myPlot$labels$x

ggplot_build(myPlot)

Kik$ggAutoTheme 







<h3 style = "text-align: center;">This Dashboard Delivers Actionable Insights Including:</h3>
  <ul>
  <li>Compare and contrast popular trends between foreign currencies</li>
  <li>Find out which subcategories are the most popular</li>
  <li>See how close failed projects come to reaching their goal</li>
  <li>and more!</li>
  </ul>
  <hr />
  
  
  
  
  
  
  
  
  
  
  









