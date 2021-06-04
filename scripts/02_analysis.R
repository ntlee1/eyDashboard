#Environments start with uppercase
#For kikstarter data
Kik <- new.env()

#TOPIC: WRANGLE ################################################################
#Import Data -------------------------------------------------------------------
Kik$kiksrt <- readr::read_csv(here::here("data", "Kickstarter Data",
                                         "ksprojects.csv")) %>%
  as_tibble

colnames(Kik$kiksrt)
lapply(Kik$kiksrt, class)
#Convert select cols to factor for easier manipulation
Kik$colFct <- c("category",
                "main_category",
                "currency",
                "state",
                "country")
Kik$kiksrt[, Kik$colFct] <- lapply(Kik$kiksrt[, Kik$colFct],
                                   as.factor)

#Cleanse and Check Each Column for Errors --------------------------------------
#NAs ---------------------------------------------------------------------------
#name, usd_pledged have NAs.
purrr::map(Kik$kiksrt, ~sum(is.na(.)))

#Find row position of NAs 
Kik$naVal <- 1:ncol(Kik$kiksrt)
Kik$naOut <- vector("list", length(Kik$naVal))
Kik$naCols <- vector("list", length(Kik$naVal))
for (i in seq_along(Kik$naVal)) {
  Kik$naOut[[i]] <- which(is.na(Kik$kiksrt[,i]))
  Kik$naCols[[i]] <-  print(colnames(Kik$kiksrt[,i]))
}
names(Kik$naOut) <- Kik$naCols
Kik$naOut

#4 projects have no name. Assign "NONAME"
#view(Kik$kiksrt[Kik$naOut$name,])
Kik$kiksrt[Kik$naOut$name,]$name <- "NONAME"
#7400+ Projects have no value for usd pledged. No action taken now until needed
#view(Kik$kiksrt[Kik$naOut$`usd pledged`,])
Kik$plgNa <- Kik$naOut$`usd pledged`

#Fix misclassified state values ------------------------------------------------
#When state == undefined, I noticed usd pledged is NA in every observation 
#It appears the API relies on state being defined to give usd pledged a value 
#We can still infer campaign success by usd_pledged_real >= usd_goal_real
unique(Kik$kiksrt$state)
Kik$stUnd <- (filter(Kik$kiksrt, state == "undefined"))
sum(is.na(Kik$stUnd$`usd pledged`)) == nrow(Kik$stUnd)
Kik$stUndSucc <- (filter(Kik$kiksrt, state == "undefined" &
                           usd_pledged_real >= usd_goal_real))
Kik$stUndSucc$state <- "successful"
Kik$stUndFail <- (filter(Kik$kiksrt, state == "undefined" &
                           usd_pledged_real <= usd_goal_real))
Kik$stUndFail$state <- "failed"
Kik$kiksrt <- filter(Kik$kiksrt, !state == "undefined") %>%
  rbind(Kik$stateNoUndef, Kik$stUndSucc, Kik$stUndFail)

#Eliminate projects with funds raised but number of backers is missing ---------
#Can't infer conclusions if there are funds raised but by an unknown number 
#of backers. Only ~3000 cases, trivial amount of cases that wont impact overall 
#analysis
#Don't know why API did not include backers in these cases
Kik$noBkrNum <- dplyr::filter(Kik$kiksrt, backers == 0 &
                                0 < usd_pledged_real)
Kik$kiksrt <- dplyr::filter(Kik$kiksrt, !(ID %in% Kik$noBkrNum$ID))

#Check for duplicate rows. 0 found --------------------------------------------
length(Kik$kiksrt$ID) == length(unique(Kik$kiksrt$ID))
Kik$dupId <- table(Kik$kiksrt$ID) %>%
  data.frame
Kik$dupId <- dplyr::filter(Kik$dupId, 1 < Kik$dupId$Freq)

#Check category, main_category, currency for case sensitive duplication --------
#Visual inspection, no issues found
unique(Kik$kiksrt$category) 
unique(Kik$kiksrt$main_category) 
unique(Kik$kiksrt$currency)

#Fix incorrect deadline dates --------------------------------------------------
#~7 launch dates are misformatted as 1970-01-01
#This is not a valid. Kickstarter was founded in 2009
#It appears the web scraping API defaulted to the POSIX start date for an
#unknown reason
#For practical purposes I have set the launch date 1 month before deadline

#Remove timestamp from launched for easy manipulation
Kik$kiksrt$launched <- as.Date(Kik$kiksrt$launched, format = "%Y-%m-%d")
Kik$mod1970 <- (dplyr::filter(Kik$kiksrt, launched < "1971-01-01"))

# %m-% adds one month. Imported from lubridate
Kik$mod1970$launched <- Kik$mod1970$deadline %m-% months(1)
Kik$kiksrt[Kik$kiksrt$launched == "1970-01-01",
           "launched"] <- Kik$mod1970$launched

#Kickstarter DOES NOT provide accurate usd pledged data. Fixer.io does. -------- 
#Reasoning:
#I suspect data scraping errors in fx conversion columns usd_pledged
#and usd_pledged_real
#Logically, if a project is in base currency USD,
#columns "pledged", "usd_pledged", and "usd_pledged_real" should be equivalent.
#They all measure the same metric, total USD dollars pledged
#This is not always the case. There are major discrepancies. 
#For simplicity, lets analyze USD projects as the FX conversion is 1:1
#TODO MAYBE check NAs for other currencies and import external data for use

#Pledge Accuracy: Kickstarter vs Fixer.io
#Filter all projects with base USD currency
#~293k USD Projects
Kik$usd <- dplyr::filter(Kik$kiksrt, `currency` == "USD")
Kik$usdNum <- nrow(Kik$usd)

#Create accuracy ratio for usd_pledged. Compare against correct amount 
#in pledged
Kik$usdKik <- Kik$usd$pledged == Kik$usd$`usd pledged`
#Filter out NAs for accurate ratio, ~500 NAs detected
Kik$usdKikNa <- sum(is.na(Kik$usdKik))
#Kickstarter provides the correct USD FX conversion 85% of the time for 
#USD projects
Kik$usdKikAcc <- (sum(Kik$usdKik, na.rm = TRUE)/(Kik$usdNum - Kik$usdKikNa)) %>%
  round(., digits = 3)

#Create fixer.io accuracy ratio for usd_pledged_real
Kik$usdFixer <- Kik$usd$pledged == Kik$usd$usd_pledged_real
#No NAs detected from fixer.io
Kik$usdFixerNa <- sum(is.na(Kik$usdFixer))
#Fixer.io provides the correct USD FX conversion 100% of the time for 
#USD projects
#Added benefit of zero NAs
Kik$usdFixerAcc <- sum(Kik$usdFixer)/Kik$usdNum

#Can we rely on Fixer.io to provide 100% accuracy for other denominations?
#Validation Process: 
#1. Import external USD FX conversion data for time period 
#2. Convert pledged column manually and compare to fixer.io 
#3. If relatively similar then fixer.io is assumed accurate 
unique(Kik$kiksrt$currency)

#Step1: 
#Import FX data. https://www.ofx.com/en-us/forex-news/historical-exchange-rates/
#Note that FX data will differ because exact time of day isn't taken 
#into account
#YMD 
Ofx <- new.env()
#Date range I need to import
range(Kik$kiksrt$deadline)

#Test fixer.io accuracy for USDAUD to verify accuracy with a non-USD denomination
Ofx$ofxUsdAud <- readxl::read_xlsx(here::here("data", "ofx_fx_data",
                                              "USDAUD_ofx.xlsx"))
Kik$aud <- dplyr::filter(Kik$kiksrt, `currency` == "AUD")
#Forex rates
Kik$aud$fixer_rate <- Kik$aud$pledged/Kik$aud$usd_pledged_real
Kik$aud$kik_rate <- Kik$aud$pledged/Kik$aud$`usd pledged`
#~9400 AUD projects
nrow(Kik$aud)
Ofx$ofxUsdAud$Date <- as.Date(Ofx$ofxUsdAud$Date, format = "%Y-%m-%d")
colnames(Ofx$ofxUsdAud) <- c("deadline", "ofx_rate")
Kik$aud <- right_join(Kik$aud, Ofx$ofxUsdAud, by = "deadline")

#Fixer.io forex conversion is highly accurate. 
#Accuracy ranges from factor of 0.95 to 1.15, variance 0.00012
#Reasonable numbers. Low spread, low variance
Kik$audOfxFixer <- Kik$aud$fixer_rate/Kik$aud$ofx_rate

#1715 NAs detected. These are projects with zero money raised. Thus 0/0 forex
sum(is.na(Kik$aud$usd_pledged_real))
Kik$audOfxFixer <- na.omit(Kik$audOfxFixer)
range(Kik$audOfxFixer)
var(Kik$audOfxFixer)

#Kickstarter forex conversion is not accurate at all
#FX factor ranges from 0 to 1025 compared to ofx
Kik$audOfxKik <- Kik$aud$kik_rate/Kik$aud$ofx_rate
Kik$audOfxKik <- Kik$audOfxKik[is.finite(Kik$audOfxKik)]
range(Kik$audOfxKik)

#Drop usd pledged for inaccuracy
Kik$kiksrt <- dplyr::select(Kik$kiksrt, -c(`usd pledged`))

#Create 4 categories of project size using quantiles ---------------------------
#of usd_goal_real, state = success
#Quantiles = 1300, 3840, 10000
#Project Sizes: Small, Mid, Large, Premium
dplyr::filter(Kik$kiksrt, state == "successful")["usd_goal_real"] %>%
  summary

Kik$kiksrt$size <- "empty"
Kik$kiksrt[Kik$kiksrt$usd_goal_real < 1300, "size"] <- "Small"
Kik$kiksrt[1300 <= Kik$kiksrt$usd_goal_real &
             Kik$kiksrt$usd_goal_real < 3838, "size"] <- "Mid"
Kik$kiksrt[3838 <= Kik$kiksrt$usd_goal_real &
             Kik$kiksrt$usd_goal_real < 10000, "size"] <- "Large"
Kik$kiksrt[10000 <= Kik$kiksrt$usd_goal_real, "size"] <- "Prem"
dplyr::filter(Kik$kiksrt, size == "empty")

#Add Length of Campaign --------------------------------------------------------
Kik$kiksrt$projLen <- (Kik$kiksrt$deadline - Kik$kiksrt$launched) %>%
  as.numeric

#Create optional filter: Filter out super successful kickstarter campaigns -----
#Reason: Useful for modeling realistic fundraisers where pledge amount is 
#similar, to goal amount
#Analysis will say if I apply this breakout filter or not
#Super successful = Breakout Success
#Define breakout success as ratio of pledge dollars to goal amount and 
#only campaigns below the median ratio for their size classification

#Step 1: Find and filter out breakout success kickstarter campaigns 
#Interested only in successful campaigns for success modeling
Kik$brkout <- Kik$kiksrt
Kik$brkout <- dplyr::filter(Kik$brkout, state == "successful")
#Example: excess_ratio of 1.4 means pledge funds matched 140% of the goal amount
Kik$brkout$excess_ratio <- Kik$brkout$usd_pledged_real/Kik$brkout$usd_goal_real
Kik$brkoutSummary <- summary(Kik$brkout$excess_ratio)

#Step 2: Find median for each project size classification
#Each size category will have different median ratio. Larger projects will
#naturally have a smaller multiplier that signifies excessive pledge funds raised 
Kik$brkoutSm <- dplyr::filter(Kik$brkout, size == "Small")
Kik$brkoutSm <- dplyr::filter(Kik$brkoutSm,
                              excess_ratio <= stats::median(Kik$brkoutSm$excess_ratio,))

Kik$brkoutMd <- dplyr::filter(Kik$brkout, size == "Mid")
Kik$brkoutMd <- dplyr::filter(Kik$brkoutMd,
                              excess_ratio <= stats::median(Kik$brkoutMd$excess_ratio))

Kik$brkoutLg <- dplyr::filter(Kik$brkout, size == "Large")
Kik$brkoutLg <- dplyr::filter(Kik$brkoutLg,
                              excess_ratio <= stats::median(Kik$brkoutLg$excess_ratio))

Kik$brkoutPrem <- dplyr::filter(Kik$brkout, size == "Prem")
Kik$brkoutPrem <- dplyr::filter(Kik$brkoutPrem,
                                excess_ratio <= stats::median(Kik$brkoutPrem$excess_ratio))


Kik$brkout <- rbind(Kik$brkoutSm,
                    Kik$brkoutMd,
                    Kik$brkoutLg,
                    Kik$brkoutPrem) %>%
  as.data.frame

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
Kik$charAvgSucc <- Kik$ttestChar$estimate[1] %>%
  round(., digits = 0)
Kik$charAvgFail <- Kik$ttestChar$estimate[2] %>%
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
#Overall
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

#Main Category Freq Table
Kik$tknRankMain <- function(mainCat) {
  myMainCat <- mainCat
  
  myTkn <- dplyr::filter(Kik$kiksrt,
                         main_category == myMainCat, state == "successful")
  myTkn <- dplyr::tibble(name = myTkn$name) %>%
    tidytext::unnest_tokens(., word, name) 
  myTkn <- myTkn %>%
    dplyr::anti_join(Kik$customStopWords)
  
  myTknTable <- myTkn %>%
    dplyr::count(word, sort = TRUE)
  return(myTknTable)
  
}

#Sub Category Freq Table
Kik$tknRankSub <- function(subCat) {
  mySubCat <- subCat
  
  myTkn <- dplyr::filter(Kik$kiksrt,
                         category == mySubCat, state == "successful")
  myTkn <- dplyr::tibble(name = myTkn$name) %>%
    tidytext::unnest_tokens(., word, name) 
  myTkn <- myTkn %>%
    dplyr::anti_join(Kik$customStopWords)
  
  myTknTable <- myTkn %>%
    dplyr::count(word, sort = TRUE)
  return(myTknTable)
  
}

#Main category word cloud
Kik$NmTknMainPlot <- function(mainCat) {
  myMainCat <- mainCat
  
  myTkn <- dplyr::filter(Kik$kiksrt,
                         main_category == myMainCat, state == "successful")
  myTkn <- dplyr::tibble(name = myTkn$name) %>%
    tidytext::unnest_tokens(., word, name) 
  myTkn <- myTkn %>%
    dplyr::anti_join(Kik$customStopWords)
  
  myTknTable <- myTkn %>%
    dplyr::count(word, sort = TRUE)
  
  #Base R title
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, cex = 1.50,
       paste("Top 60 Most Popular Words in", mainCat, "Category"))
  myTknCld <- myTkn %>%
    count(word) %>%
    with(wordcloud::wordcloud(word, n, max.words = 60,
                              color = brewer.pal(8, "Dark2"),
                              scale = c(3,1.5)))
  
}

#Kik$NmTknSubPlotInput <- data.frame(subCat = unique(Kik$kiksrt$category))
#Kik$NmTknSubPlotOutput <- purrr::pmap(Kik$NmTknSubPlotInput, Kik$NmTknSubPlot)

#Subcategory Word Cloud
Kik$NmTknSubPlot <- function(subCat) {
  mySubCat <- subCat
  
  myTkn <- dplyr::filter(Kik$kiksrt,
                         category == mySubCat, state == "successful")
  myTkn <- dplyr::tibble(name = myTkn$name) %>%
    tidytext::unnest_tokens(., word, name) 
  myTkn <- myTkn %>%
    dplyr::anti_join(Kik$customStopWords)
  
  myTknTable <- myTkn %>%
    dplyr::count(word, sort = TRUE)
  
  #Base R title
  layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, cex = 1.50,
       paste("Top 60 =Most Popular Words in", subCat, "Category"))
  myTknCld <- myTkn %>%
    count(word) %>%
    with(wordcloud::wordcloud(word, n, max.words = 60,
                              color = brewer.pal(8, "Dark2"),
                              scale = c(3,1.5)))
  
  
}

#Kik$NmTknSubPlotInput <- data.frame(subCat = unique(Kik$kiksrt$category))
#Kik$NmTknSubPlotOutput <- purrr::pmap(Kik$NmTknSubPlotInput, Kik$NmTknSubPlot)

#Are projects in different denominations interested in different categories?----
#Process
#1. Create a distribution of word frequency from entire kikstarter dataset
#2. Compare against word frequency distributions for different currencies
#3. Make note of words with drastically different rank position to highlight
#significant differences in the types of products a currency is interested in

#Top 100 words overall
Kik$kikNmTknPlot <- Kik$kikNmTkn %>%
  dplyr::count(word, sort = TRUE)

Kik$tknRank100 <- cbind(Kik$kikNmTknPlot[1:100,], rank = 1:100)


#Main Category Freq Table FX
Kik$tknRankMain <- function(mainCat, currency) {
  myMainCat <- mainCat
  myCurrency <- currency
  
  myTkn <- dplyr::filter(Kik$kiksrt,
                         main_category == myMainCat, state == "successful",
                         currency == myCurrency)
  myTkn <- dplyr::tibble(name = myTkn$name) %>%
    tidytext::unnest_tokens(., word, name) 
  myTkn <- myTkn %>%
    dplyr::anti_join(Kik$customStopWords)
  
  myTknTable <- myTkn %>%
    dplyr::count(word, sort = TRUE)
  return(myTknTable)
  
}

#Find which words are found in both datasets
test1 <- Kik$tknRankMain("Food", "GBP")[1:100,1]
test2 <- Kik$tknRankMain("Food", "USD")[1:100,1]

c(test1) %in% c(test2)
















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
mean(Kik$kikPartialFailSmall$partialFail)
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
mean(Kik$kikPartialFailMid$partialFail)
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
mean(Kik$kikPartialFailLarge$partialFail)
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
mean(Kik$kikPartialFailPrem$partialFail)
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























