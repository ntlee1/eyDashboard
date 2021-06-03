#Personal preference for no scientific notation
options(scipen = 999)

#Environments start with uppercase
#For initialization
Init <- new.env()
#For kikstarter data
Kik <- new.env()

#Required Packages -------------------------------------------------------------
Init$pkgReq <- c("tidyverse",
                 "colorspace",
                 "crosstalk",
                 "here",
                 "lubridate",
                 "plotly",
                 "RColorBrewer",
                 "scales",
                 "ggthemes",
                 "magrittr",
                 "rlist", 
                 "readxl",
                 "stringr",
                 "tidytext",
                 "stats",
                 "wordcloud",
                 "rlang")
lapply(Init$pkgReq, require, character.only = TRUE)

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

#Global vars will be reused often throughout scripts ---------------------------
#Useful vars stored here for easy reference and reuse throughout project
#Var: Filters
Kik$kikSuccess <- dplyr::filter(Kik$kiksrt, state == "successful")

#Var: Filter out super successful kickstarter campaigns 
#Reason: Useful for modeling realistic fundraisers where pledge amount is 
#similar, to goal amount
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































