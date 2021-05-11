# import ------------------------------------------------------------------
kikstrt <- readr::read_csv(here::here("data", "Kickstarter Data", "ksprojects.csv"))
print(kikstrt, tibble.width = "inf")
kikstrt <- as_tibble(kikstrt)
colnames(kikstrt)

# State Analysis ---------------------------------------------------------------------
#when state == undefined, i noticed usd pledged is NA in every observation 
#Maybe the API relies on this value to give a state?
#we can still infer success or failure by comparing usd_pledged_real to usd_goal_real
unique(kikstrt$state)
stateUndef <- (filter(kikstrt, state == "undefined"))
sum(is.na(stateUndef$`usd pledged`)) == nrow(stateUndef)

stateUndefSuccess <- (filter(kikstrt, state == "undefined" & usd_pledged_real >= usd_goal_real))
stateUndefFail <- (filter(kikstrt, state == "undefined" & usd_pledged_real <= usd_goal_real))

#Add undefined successes and failures to full dataset
kikstrt <- rbind(kikstrt, stateUndefSuccess, stateUndefFail)

# Cleanse NAs -----------------------------------------------------------------
#Separates NAs by column and returns their row position
naVals <- 1:ncol(kikstrt)
naOut <- vector("list", length(naVals))
naColNms <- vector("list", length(naVals))
for (i in seq_along(naVals)) {
  naOut[[i]] <- which(is.na(kikstrt[,i]))
  naColNms[[i]] <-  print(colnames(kikstrt[,i]))
}
names(naOut) <- naColNms
naOut

#Explore NA data on a case by case basis and decide how to handle
#4 projects have no name. Assign "NONAME"
view(kikstrt[naOut$name,])
kikstrt[naOut$name,]$name <- "NONAME"

#7431 Projects have no value for usd pledged. No action taken now until needed
view(kikstrt[naOut$`usd pledged`,])
usdPlgNaRows <- naOut$`usd pledged`


#usd pledged vs usd_pledged_real data scraping accuracy check------------------
#Logically, if a project is in base currency USD,
#columns "pledged", "usd_pledged", and "usd_pledged_real" should be equivalent.
#They all measure the same metric, total USD dollars pledged
#This is not always the case. There are major discrepancies. 
#I suspect data scraping errors. 
#TODO check NAs for other currencies and import external data for use

#For simplicity, lets analyze USD projects as the FX conversion is 1:1

#Filter all projects with base USD currency
#297,990 USD Projects
baseUsd <- filter(kikstrt, `currency` == "USD")
numberUSDProjects <- nrow(baseUsd)

#Create Kickstarter accuracy ratio for usd_pledged
pledgeUSDKik <- baseUsd$pledged == baseUsd$`usd pledged`
#There are 2738 projects with no value for usd pledged.
#Could indicate data scraping issues from kickstarter
naUSDKik <- sum(is.na(pledgeUSDKik))
#Kickstarter provides the correct USD FX conversion 85% of the time for USD projects
pledgeUSDKikAccRatio <- sum(pledgeUSDKik, na.rm = TRUE)/(numberUSDProjects - naUSDKik)

#Create fixer.io accuracy ratio for usd_pledged_real
pledgeUSDFixer <- baseUsd$pledged == baseUsd$usd_pledged_real
#No NAs detected from fixer.io
naUSDFixer <- sum(is.na(pledgeUSDFixer))
#Fixer.io provides the correct USD FX conversion 100% of the time for USD projects
pledgeUSDFixerAccRatio <- sum(pledgeUSDFixer)/numberUSDProjects

#Can we rely on Fixer.io to provide 100% accuracy for other denominations?
#TODO
#1. Import external USD FX conversion data for time period 
#2. Convert pledged column manually and compare to fixer.io 
#3. If relatively similar then fixer.io is accurate
unique(kikstrt$currency)
view(table(kikstrt$currency))

#Step1: 
#Import FX data from https://www.ofx.com/en-us/forex-news/historical-exchange-rates/
#USD FX data needed from 2009-05-03 to 2018-03-03. YMD 
range(kikstrt$deadline)

#Test fixer.io accuracy for USDAUD to verify accuracy with a non-USD denomination
ofxUsdAud <- readxl::read_xlsx(here::here("data", "ofx_fx_data", "USDAUD_ofx.xlsx"))
baseAud <- filter(kikstrt, `currency` == "AUD")
#9581 AUD projects
nrow(baseAud)

#Match dates of USDAUD external FX rate and multiply pledged by currency value
ofxUsdAud$deadline <- ofxUsdAud$Date
ofxUsdAud$deadline <- as.Date(ofxUsdAud$deadline, format = "Y-m-d")
class(ofxUsdAud$deadline)
baseAud <- right_join(baseAud, ofxUsdAud, by = "deadline")
baseAud$ofx_usd_pledged <- baseAud$`pledged`/baseAud$USDAUD

#External USDAUD data factor range 0.86 to 1.04, variance 0.00012
#Reasonable numbers. Low spread, low variance
#Fixer.io likely has an accurate ofx_usd_pledged column unlike kickstarter usd_pledged
#Also means my external data is likely accurate
#TODO Also correct usd_goal_real
ofxPledgeCompare <- baseAud$usd_pledged_real/baseAud$ofx_usd_pledged
#1715 NAs detected. These are projects with zero money raised. Remove
sum(is.na(baseAud$usd_pledged_real))
ofxPledgeCompare <- na.omit(ofxPledgeCompare)
range(ofxPledgeCompare)
var(ofxPledgeCompare)
view(baseAud)



# Project Name Length vs. Success ----------------------------------------------

#Project Name Length vs. Success
#Characters #Spaces Excluded
charAnalysis <- nchar(kikstrt$name[1:nrow(kikstrt)])
charAnalysis <- as.tibble(charAnalysis)
charAnalysis$state <- as.factor(kikstrt$state)

#TODO Summary stats for each factor variable
#1. Determine if there is a statistical difference between length and state
#2. If there is determine the optimal title length name
#Ignore state == c(live, suspended, canceled)
#Cancellation reason could be anything

#t-test for project name character length vs state
#TODO learn how to interpret t-test results
charSuccess <- filter(charAnalysis, state == "successful")
charFailed <- filter(charAnalysis, state == "failed")
charTTest <- t.test(charSuccess$value, charFailed$value)

#Number of words analysis
wordLenAnalysis <- stringr::str_count(kikstrt$name[1:nrow(kikstrt)], "\\S+")
wordLenAnalysis <- as.tibble(wordLenAnalysis)
wordLenAnalysis$state <- as.factor(kikstrt$state)

#t-test for project name word count vs state
wordLenSuccess <- filter(wordLenAnalysis, state == "successful")
wordLenFail <- filter(wordLenAnalysis, state == "failed")
#TODO learn how to interpret t-test results
wordLenTTest <- t.test(wordLenSuccess$value, wordLenFail$value)






























