#Personal preference for no scientific notation
options(scipen = 999)

#Required Packages --------------------------------------------------------
pkgReq <- c("tidyverse",
            "colorspace",
            "crosstalk",
            "here",
            "lubridate",
            "plotly",
            "RColorBrewer",
            "readxl",
            "scales",
            "ggthemes",
            "magrittr",
            "rlist",
            "readxl",
            "stringr",
            "tidytext",
            "stats")
lapply(pkgReq, require, character.only = TRUE)

#Import Data -------------------------------------------------------------------
kikstrt <- readr::read_csv(here::here("data", "Kickstarter Data", "ksprojects.csv"))
kikstrt <- as_tibble(kikstrt)
colnames(kikstrt)
lapply(kikstrt, class)
#Convert select cols to factor for easier manipulation
kikstrt$category <- as.factor(kikstrt$category)
kikstrt$main_category <- as.factor(kikstrt$main_category)
kikstrt$currency <- as.factor(kikstrt$currency)
kikstrt$state <- as.factor(kikstrt$state)
kikstrt$country <- as.factor(kikstrt$country)

#Cleanse and Check Each Column for Errors --------------------------------------
#NAs ---------------------------------------------------------------------------
#name, usd_pledged have NAs.
map(kikstrt, ~sum(is.na(.)))

#Find row position of NAs 
naVals <- 1:ncol(kikstrt)
naOut <- vector("list", length(naVals))
naColNms <- vector("list", length(naVals))
for (i in seq_along(naVals)) {
  naOut[[i]] <- which(is.na(kikstrt[,i]))
  naColNms[[i]] <-  print(colnames(kikstrt[,i]))
}
names(naOut) <- naColNms
naOut

#4 projects have no name. Assign "NONAME"
#view(kikstrt[naOut$name,])
kikstrt[naOut$name,]$name <- "NONAME"
#7400+ Projects have no value for usd pledged. No action taken now until needed
#view(kikstrt[naOut$`usd pledged`,])
usdPlgNaRows <- naOut$`usd pledged`

#Fix misclassified state values ------------------------------------------------
#When state == undefined, I noticed usd pledged is NA in every observation 
#It appears the API relies on state being defined to give usd pledged a value 
#We can still infer campaign success by usd_pledged_real >= usd_goal_real
unique(kikstrt$state)
stateUndef <- (filter(kikstrt, state == "undefined"))
sum(is.na(stateUndef$`usd pledged`)) == nrow(stateUndef)
stateUndefSuccess <- (filter(kikstrt, state == "undefined" & usd_pledged_real >= usd_goal_real))
stateUndefFail <- (filter(kikstrt, state == "undefined" & usd_pledged_real <= usd_goal_real))
kikstrt <- rbind(kikstrt, stateUndefSuccess, stateUndefFail)

#Remove duplicate rows. ~3400 found --------------------------------------------
length(kikstrt$ID) == length(unique(kikstrt$ID))
duplicateId <- table(kikstrt$ID) %>%
  data.frame
duplicateId <- filter(duplicateId, 1 < duplicateId$Freq)
duplicateIdKik <- filter(kikstrt, ID %in% duplicateId$Var1)
table(duplicateId$Freq)
#Remove duplicate data
kikstrt <- kikstrt %>% 
  distinct
#TRUE
length(kikstrt$ID) == length(unique(kikstrt$ID))

#Check category, main_category, currency for case sensitive duplication --------
#Visual inspection, no issues found
unique(kikstrt$category) 
unique(kikstrt$main_category) 
unique(kikstrt$currency)

#Fix incorrect deadline dates --------------------------------------------------
#~15 launch dates are misformatted as 1970-01-01
#This is not a valid. Kickstarter was founded in 2009
#It appears the web scraping API defaulted to the POSIX start date for an unknown reason
#For practical purposes I have set the launch date 1 month before deadline
mod1970 <- (filter(kikstrt, launched < "1971-01-01"))
# %m+% adds one month. Imported from lubridate
kikstrt <- full_join(mod1970, kikstrt)
#Append modifications then filter out erroneous data
kikstrt <- filter(kikstrt, launched > "1971-01-01")

#Remove timestamp from launched for easy manipulation
kikstrt$launched <- as.Date(kikstrt$launched, format = "%Y-%m-%d")


#Kickstarter DOES NOT provide accurate usd pledged data. Fixer.io does. -------- 
#Reasoning:
#I suspect data scraping errors in fx conversion columns usd_pledged and usd_pledged_real
#Logically, if a project is in base currency USD,
#columns "pledged", "usd_pledged", and "usd_pledged_real" should be equivalent.
#They all measure the same metric, total USD dollars pledged
#This is not always the case. There are major discrepancies. 
#For simplicity, lets analyze USD projects as the FX conversion is 1:1
#TODO MAYBE check NAs for other currencies and import external data for use

#Pledge Accuracy: Kickstarter vs Fixer.io
#Filter all projects with base USD currency
#297,990 USD Projects
baseUsd <- filter(kikstrt, `currency` == "USD")
numberUSDProjects <- nrow(baseUsd)

#Create Kickstarter accuracy ratio for usd_pledged
pledgeUSDKik <- baseUsd$pledged == baseUsd$`usd pledged`
#5363 NAs detected
naUSDKik <- sum(is.na(pledgeUSDKik))
#Kickstarter provides the correct USD FX conversion 85% of the time for USD projects
pledgeUSDKikAccRatio <- sum(pledgeUSDKik, na.rm = TRUE)/(numberUSDProjects - naUSDKik)

#Create fixer.io accuracy ratio for usd_pledged_real
pledgeUSDFixer <- baseUsd$pledged == baseUsd$usd_pledged_real
#No NAs detected from fixer.io
naUSDFixer <- sum(is.na(pledgeUSDFixer))
#Fixer.io provides the correct USD FX conversion 100% of the time for USD projects
#Added benefit of zero NAs
pledgeUSDFixerAccRatio <- sum(pledgeUSDFixer)/numberUSDProjects

#Can we rely on Fixer.io to provide 100% accuracy for other denominations?
#Validation Process: 
#1. Import external USD FX conversion data for time period 
#2. Convert pledged column manually and compare to fixer.io 
#3. If relatively similar then fixer.io is assumed accurate 
unique(kikstrt$currency)

#Step1: 
#Import FX data from https://www.ofx.com/en-us/forex-news/historical-exchange-rates/
#Note that FX data will differ because exact time of day is taken into account
#YMD 
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

#Accuracy ranges from factor of 0.86 to 1.04, variance 0.00012
#Reasonable numbers. Low spread, low variance
#Fixer.io is highly accurate compared to ofx_usd_pledged column.
#Kickstarter is not even close
ofxPledgeCompare <- baseAud$usd_pledged_real/baseAud$ofx_usd_pledged
#1715 NAs detected. These are projects with zero money raised. Remove
sum(is.na(baseAud$usd_pledged_real))
ofxPledgeCompare <- na.omit(ofxPledgeCompare)
range(ofxPledgeCompare)
var(ofxPledgeCompare)

#Drop usd pledged for inaccuracy
kikstrt <- select(kikstrt, -c(`usd pledged`))

<<<<<<< HEAD
#Create 4 categories of project size using quantiles ---------------------------
#of usd_goal_real, state = success
#Quantiles = 1302, 3838, 10000
#Project Sizes: Small, Mid, Large, Premium
kikSmall <- filter(kikstrt, usd_goal_real < 1302) 
kikSmall$size <- "Small"
kikMid <- filter(kikstrt, 1302 <= usd_goal_real & usd_goal_real < 3838)
kikMid$size <- "Mid"
kikLarge <- filter(kikstrt, 3838 <= usd_goal_real & usd_goal_real < 10000) 
kikLarge$size <- "Large"
kikPrem <- filter(kikstrt, 10000 <= usd_goal_real)
kikPrem$size <- "Prem"
#Add size classification
kikstrt <- rbind(kikSmall, kikMid, kikLarge, kikPrem) %>%
  data.frame

#Add Length of Campaign --------------------------------------------------------
kikstrt$projDays <- (kikstrt$deadline - kikstrt$launched) %>%
  as.numeric

#Global vars will be reused often throughout scripts ---------------------------
#Useful vars stored here for easy reference and reuse throughout project
#Var: Filters
kikSuccess <- filter(kikstrt, state == "successful")
summary(kikSuccess$usd_goal_real)

#Var: Filter out super successful kickstarter campaigns 
#Reason: Useful for modeling realistic fundraisers where pledge amount is 
#similar, to goal amount
#Define breakout success as ratio of pledge dollars to goal amount and keep
#only campaigns below median ratio 

#Step 1: Find and filter out breakout success kickstarter campaigns 
#Interested only in successful campaigns for success modeling
breakoutFilter <- kikstrt
breakoutFilter <- filter(breakoutFilter, state == "successful")
#Example: goalExceedRatio of 1.4 means pledge funds matched 140% of the goal amount
breakoutFilter$goalExceedRatio <- breakoutFilter$usd_pledged_real/breakoutFilter$usd_goal_real
breakoutFilterSummary <- summary(breakoutFilter$goalExceedRatio)

#Step 2: Find median for each project size classification
#Each size category will have different median ratio. Larger projects will naturally
#have a smaller multiplier that signifies excessive pledge funds raised 
breakoutSmall <- dplyr::filter(breakoutFilter, size == "Small")
breakoutSmall <- dplyr::filter(breakoutSmall,
                               goalExceedRatio <= stats::median(breakoutSmall$goalExceedRatio))

breakoutMid <- dplyr::filter(breakoutFilter, size == "Mid")
breakoutMid <- dplyr::filter(breakoutMid,
                             goalExceedRatio <= stats::median(breakoutMid$goalExceedRatio))

breakoutLarge <- dplyr::filter(breakoutFilter, size == "Mid")
breakoutLarge <- dplyr::filter(breakoutLarge,
                               goalExceedRatio <= stats::median(breakoutLarge$goalExceedRatio))

breakoutPrem <- dplyr::filter(breakoutFilter, size == "Mid")
breakoutPrem <- dplyr::filter(breakoutPrem,
                              goalExceedRatio <= stats::median(breakoutPrem$goalExceedRatio))


breakoutFilter <- rbind(breakoutSmall,
                        breakoutMid,
                        breakoutLarge,
                        breakoutPrem) %>%
  as.data.frame









=======
>>>>>>> 8713d66891ab160acbae6899b431ad7e24fc562e





















#Create 4 categories of project size using quantiles ---------------------------
#of usd_goal_real, state = success
#Quantiles = 1302, 3838, 10000
#Project Sizes: Small, Mid, Large, Premium
kikSmall <- filter(kikstrt, usd_goal_real < 1302) 
kikSmall$size <- "Small"
kikMid <- filter(kikstrt, 1302 <= usd_goal_real & usd_goal_real < 3838)
kikMid$size <- "Mid"
kikLarge <- filter(kikstrt, 3838 <= usd_goal_real & usd_goal_real < 10000) 
kikLarge$size <- "Large"
kikPrem <- filter(kikstrt, 10000 <= usd_goal_real)
kikPrem$size <- "Prem"
#Add size classification
kikstrt <- rbind(kikSmall, kikMid, kikLarge, kikPrem) %>%
  data.frame

#Add Length of Campaign --------------------------------------------------------
kikstrt$projDays <- (kikstrt$deadline - kikstrt$launched) %>%
  as.numeric
