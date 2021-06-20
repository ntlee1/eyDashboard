#Environments start with uppercase
#For kikstarter data
Kik <- new.env()
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#For Shiny 
Shy <- new.env()
#Shiny Palette
Shy$palBkGround <- "#1C2134"

#TOPIC: WRANGLE ################################################################
#Import Data -------------------------------------------------------------------
Kik$pathKikData <- here::here("data", "Kickstarter Data","ksprojects.csv")
Kik$kiksrt <- readr::read_csv(Kik$pathKikData) %>%
  as_tibble

<<<<<<< HEAD
=======
=======

#TOPIC: WRANGLE ################################################################
#Import Data -------------------------------------------------------------------
Kik$kiksrt <- readr::read_csv(here::here("data", "Kickstarter Data",
                                         "ksprojects.csv")) %>%
  as_tibble

colnames(Kik$kiksrt)
lapply(Kik$kiksrt, class)
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#Convert select cols to factor for easier manipulation
Kik$colFct <- c("category",
                "main_category",
                "currency",
                "state",
                "country")
Kik$kiksrt[, Kik$colFct] <- lapply(Kik$kiksrt[, Kik$colFct],
                                   as.factor)

<<<<<<< HEAD
#Common ggplot specifications to save time
#Must always place these specifications immediately after the geom
#That way I can partially overwrite them with another call to theme if needed

=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#Shiny UI Plot Modifications 
#Weird white line shows up on bottom of plot. 
#Plot border specified as background color
Shy$plotColsEy <- list(theme(plot.background = element_rect(fill = Shy$palBkGround, 
                                                            colour = Shy$palBkGround,
                                                            size = 0),
                             legend.background = element_rect(fill = Shy$palBkGround),
                             text = element_text(color = "white"),
                             axis.text = element_text(color = "white"),
                             legend.text = element_text(color = "white")))

#ggplot mods I use often
Kik$ggAutoTheme <- list(theme(plot.title = element_text(hjust = 0.5,
                                                        size = 24,
                                                        margin = margin(t = 16,
                                                                        b = 16)),
                              axis.title.y = element_text(margin = margin(r = 16,
                                                                          l = 16)),
                              axis.title.x = element_text(margin = margin(t = 16,
                                                                          b = 16)),
                              text = element_text(size = 20),
                              axis.text.x = element_text(angle = 90)))


#Cleanse and Check Each Column for Errors --------------------------------------
#NAs ---------------------------------------------------------------------------
#Fix NAs in cols: name, usd_pledged
<<<<<<< HEAD
=======
=======
#Cleanse and Check Each Column for Errors --------------------------------------
#NAs ---------------------------------------------------------------------------
#name, usd_pledged have NAs.
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
purrr::map(Kik$kiksrt, ~sum(is.na(.)))

#Find row position of NAs 
Kik$naVal <- 1:ncol(Kik$kiksrt)
Kik$naOut <- vector("list", length(Kik$naVal))
Kik$naCols <- vector("list", length(Kik$naVal))
<<<<<<< HEAD

=======
<<<<<<< HEAD

=======
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
for (i in seq_along(Kik$naVal)) {
  Kik$naOut[[i]] <- which(is.na(Kik$kiksrt[,i]))
  Kik$naCols[[i]] <-  print(colnames(Kik$kiksrt[,i]))
}
names(Kik$naOut) <- Kik$naCols
Kik$naOut

#4 projects have no name. Assign "NONAME"
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$kiksrt[Kik$naOut$name,]$name <- "NONAME"

#7400+ Projects have no value for usd pledged. No action taken now until needed
#This does get fixed later in the script
Kik$plgNa <- Kik$naOut$`usd pledged`

#Fix misclassified state values ------------------------------------------------
#When state == undefined, I noticed usd pledged is NA in every observation. 
#It appears the API relies on state being defined to give usd pledged a value. 
#We can still infer campaign success by usd_pledged_real >= usd_goal_real.
<<<<<<< HEAD
=======
=======
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
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$stUnd <- (filter(Kik$kiksrt, state == "undefined"))
sum(is.na(Kik$stUnd$`usd pledged`)) == nrow(Kik$stUnd)
Kik$stUndSucc <- (filter(Kik$kiksrt, state == "undefined" &
                           usd_pledged_real >= usd_goal_real))
Kik$stUndSucc$state <- "successful"
<<<<<<< HEAD

=======
<<<<<<< HEAD

=======
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$stUndFail <- (filter(Kik$kiksrt, state == "undefined" &
                           usd_pledged_real <= usd_goal_real))
Kik$stUndFail$state <- "failed"
Kik$kiksrt <- filter(Kik$kiksrt, !state == "undefined") %>%
  rbind(Kik$stateNoUndef, Kik$stUndSucc, Kik$stUndFail)

#Eliminate projects with funds raised but number of backers is missing ---------
#Can't infer conclusions if there are funds raised but by an unknown number 
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#of backers.
#Only ~3000 cases, trivial amount that wont impact overall analysis
#Don't know why API did not include backers in these cases
Kik$noBkr <- dplyr::filter(Kik$kiksrt, backers == 0 &
                             0 < usd_pledged_real)
Kik$kiksrt <- dplyr::filter(Kik$kiksrt, !(ID %in% Kik$noBkr$ID))
<<<<<<< HEAD
=======
=======
#of backers. Only ~3000 cases, trivial amount of cases that wont impact overall 
#analysis
#Don't know why API did not include backers in these cases
Kik$noBkrNum <- dplyr::filter(Kik$kiksrt, backers == 0 &
                                0 < usd_pledged_real)
Kik$kiksrt <- dplyr::filter(Kik$kiksrt, !(ID %in% Kik$noBkrNum$ID))
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886

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
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#~7 launch dates are misformatted as 1970-01-01.
#This is not a valid. Kickstarter was founded in 2009.
#It appears the API defaulted to the POSIX start date for an unknown reason.
#For practical purposes I have set the launch date 1 month before deadline.
<<<<<<< HEAD
=======
=======
#~7 launch dates are misformatted as 1970-01-01
#This is not a valid. Kickstarter was founded in 2009
#It appears the web scraping API defaulted to the POSIX start date for an
#unknown reason
#For practical purposes I have set the launch date 1 month before deadline
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886

#Remove timestamp from launched for easy manipulation
Kik$kiksrt$launched <- as.Date(Kik$kiksrt$launched, format = "%Y-%m-%d")
Kik$mod1970 <- (dplyr::filter(Kik$kiksrt, launched < "1971-01-01"))

# %m-% adds one month. Imported from lubridate
Kik$mod1970$launched <- Kik$mod1970$deadline %m-% months(1)
Kik$kiksrt[Kik$kiksrt$launched == "1970-01-01",
           "launched"] <- Kik$mod1970$launched

<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#Kickstarter DOES NOT provide accurate foreign conversion. Fixer.io does. ------
#Reasoning:
#Logically, if you examine projects in base currency USD,
#cols "pledged", "usd_pledged", and "usd_pledged_real" should be equivalent.
#The FX conversion should simply be 1:1 
#This is not always the case. There are major discrepancies. 

#Lets examine USD projects to determine which cols provide correct fx conversion
#Kickstarter = `usd pledged`
#Fixer io = `usd_pledged_real`
<<<<<<< HEAD
=======
=======
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
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#~293k USD Projects
Kik$usd <- dplyr::filter(Kik$kiksrt, `currency` == "USD")
Kik$usdNum <- nrow(Kik$usd)

<<<<<<< HEAD
#Kickstarter
#Compare fx conversion accuracy as ratio against correct value in col `pledged`
=======
<<<<<<< HEAD
#Kickstarter
#Compare fx conversion accuracy as ratio against correct value in col `pledged`
Kik$usdKik <- Kik$usd$pledged == Kik$usd$`usd pledged`
#Filter out NAs for accurate ratio, ~500 NAs detected
Kik$usdKikNa <- sum(is.na(Kik$usdKik))
#Kickstarter has 85% fx conversion accuracy for USD projects
Kik$usdKikAcc <- (sum(Kik$usdKik, na.rm = TRUE)/(Kik$usdNum - Kik$usdKikNa)) %>%
  round(., digits = 3)

#Fixer.io
#Compare fx conversion accuracy as ratio against correct value in col `pledged`
Kik$usdFixer <- Kik$usd$pledged == Kik$usd$usd_pledged_real
#No NAs detected from fixer.io
Kik$usdFixerNa <- sum(is.na(Kik$usdFixer))
#Fixer.io has 100% fx conversion accuracy for USD projects
#Added benefit of zero NAs
Kik$usdFixerAcc <- sum(Kik$usdFixer)/Kik$usdNum

#Fixer.io also provides 100% fx conversion accuracy for other denominations ----
#I test one currency pair, USD/AUD, and assume all other pairs are correct. 
#Validation Process: 
#1. Import external USD/AUD FX conversion data for time period as benchmark
#2. Compare to fixer.io fx conversion values
#3. If relatively similar then fixer.io is assumed accurate 

#Step1: Import
#Data source: https://www.ofx.com/en-us/forex-news/historical-exchange-rates/
#Note that exact values will differ. Ofx takes into account time of day.
#YMD 
Ofx <- new.env()

Ofx$ofxUsdAud <- readxl::read_xlsx(here::here("data", "ofx_fx_data",
                                              "USDAUD_ofx.xlsx"))
Kik$aud <- dplyr::filter(Kik$kiksrt, `currency` == "AUD")

#Derive fx conversion rates, kikstarter and fixer.io
Kik$aud$fixer_rate <- Kik$aud$pledged/Kik$aud$usd_pledged_real
Kik$aud$kik_rate <- Kik$aud$pledged/Kik$aud$`usd pledged`

Ofx$ofxUsdAud$Date <- as.Date(Ofx$ofxUsdAud$Date, format = "%Y-%m-%d")
colnames(Ofx$ofxUsdAud) <- c("deadline", "ofx_rate")
Kik$aud <- right_join(Kik$aud, Ofx$ofxUsdAud, by = "deadline")
Kik$audOfxFixer <- Kik$aud$fixer_rate/Kik$aud$ofx_rate

#Fixer.io forex conversion is highly accurate. 
#Accuracy spread multiplier is 0.95-1.15, variance 0.00012
#Reasonable numbers. Low spread, low variance
#NAs are removed but do not impact accuracy ratio. 
#These are simply projects with zero money raised. Thus 0/0
=======
#Create accuracy ratio for usd_pledged. Compare against correct amount 
#in pledged
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
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
<<<<<<< HEAD
#NAs are removed but do not impact accuracy ratio. 
#These are simply projects with zero money raised. Thus 0/0
=======
Kik$audOfxFixer <- Kik$aud$fixer_rate/Kik$aud$ofx_rate

#1715 NAs detected. These are projects with zero money raised. Thus 0/0 forex
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
sum(is.na(Kik$aud$usd_pledged_real))
Kik$audOfxFixer <- na.omit(Kik$audOfxFixer)
range(Kik$audOfxFixer)
var(Kik$audOfxFixer)

#Kickstarter forex conversion is not accurate at all
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#Accuracy spread multiplier is 0-1025. Very unreliable. 
#Drop usd pledged for inaccuracy
Kik$audOfxKik <- Kik$aud$kik_rate/Kik$aud$ofx_rate
Kik$audOfxKik <- Kik$audOfxKik[is.finite(Kik$audOfxKik)]
range(Kik$audOfxKik)
Kik$kiksrt <- dplyr::select(Kik$kiksrt, -c(`usd pledged`))

#Create 4 categories of campaign size ------------------------------------------
#Based on quantiles of usd_goal_real, state = success
#Quantiles ($) = 1300, 3840, 10000
<<<<<<< HEAD
#Project Sizes: Small, Med, Large, Premium
=======
=======
#FX factor ranges from 0 to 1025 compared to ofx
Kik$audOfxKik <- Kik$aud$kik_rate/Kik$aud$ofx_rate
Kik$audOfxKik <- Kik$audOfxKik[is.finite(Kik$audOfxKik)]
range(Kik$audOfxKik)

#Drop usd pledged for inaccuracy
Kik$kiksrt <- dplyr::select(Kik$kiksrt, -c(`usd pledged`))

#Create 4 categories of project size using quantiles ---------------------------
#of usd_goal_real, state = success
#Quantiles = 1300, 3840, 10000
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
#Project Sizes: Small, Mid, Large, Premium
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
dplyr::filter(Kik$kiksrt, state == "successful")["usd_goal_real"] %>%
  summary

Kik$kiksrt$size <- "empty"
Kik$kiksrt[Kik$kiksrt$usd_goal_real < 1300, "size"] <- "Small"
Kik$kiksrt[1300 <= Kik$kiksrt$usd_goal_real &
             Kik$kiksrt$usd_goal_real < 3838, "size"] <- "Med"
Kik$kiksrt[3838 <= Kik$kiksrt$usd_goal_real &
             Kik$kiksrt$usd_goal_real < 10000, "size"] <- "Large"
Kik$kiksrt[10000 <= Kik$kiksrt$usd_goal_real, "size"] <- "Prem"
dplyr::filter(Kik$kiksrt, size == "empty")
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$kiksrt$size <- factor(Kik$kiksrt$size,
                          levels = c("Small", "Med", "Large", "Prem"))
Kik$sizeExplain <- data.frame(comment = c("Small projects goal range is $0-1299",
                                          "Med projects goal range is $1300-3839",
                                          "Large projects goal range is $3840-9999",
                                          "Prem projects goal range is $10000+"))
Kik$sizeExplain$size <- levels(Kik$kiksrt$size)

#Add Time Length of Campaign ---------------------------------------------------
Kik$kiksrt$projLen <- (Kik$kiksrt$deadline - Kik$kiksrt$launched) %>%
  as.numeric

#Filter created to identify super successful kickstarter campaigns -------------
#Breakout Success = Super Successful
#This is a filter that is used occasionally during analysis. 
#I will specify when I use this filter. 
#Motive: Useful for filtering for realistic fundraisers where pledge amount is
#similar to goal amount. 
#That means the campaign did not raise abnormally excess funds.
#Campaigns falling below median ratio of `pledge dollars/goal amount` are defined as
#a realistic campaign. 

#Filter for realistic campaigns
<<<<<<< HEAD
=======
=======

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
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$brkout <- Kik$kiksrt
Kik$brkout <- dplyr::filter(Kik$brkout, state == "successful")
#Example: excess_ratio of 1.4 means pledge funds matched 140% of the goal amount
Kik$brkout$excess_ratio <- Kik$brkout$usd_pledged_real/Kik$brkout$usd_goal_real
Kik$brkoutSummary <- summary(Kik$brkout$excess_ratio)

<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#Find median of `Kik$brkout$excess_ratio` for each project size classification
#Motive: Larger projects will naturally have a smaller multiplier 
#that signifies excessive pledge funds raised.
#Its a lot harder to raise multiples of a goal amount of $1M compared to $1000 

<<<<<<< HEAD
=======
=======
#Step 2: Find median for each project size classification
#Each size category will have different median ratio. Larger projects will
#naturally have a smaller multiplier that signifies excessive pledge funds raised 
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$brkoutSm <- dplyr::filter(Kik$brkout, size == "Small")
Kik$brkoutSm <- dplyr::filter(Kik$brkoutSm,
                              excess_ratio <= stats::median(Kik$brkoutSm$excess_ratio,))

Kik$brkoutMd <- dplyr::filter(Kik$brkout, size == "Med")
Kik$brkoutMd <- dplyr::filter(Kik$brkoutMd,
                              excess_ratio <= stats::median(Kik$brkoutMd$excess_ratio))

Kik$brkoutLg <- dplyr::filter(Kik$brkout, size == "Large")
Kik$brkoutLg <- dplyr::filter(Kik$brkoutLg,
                              excess_ratio <= stats::median(Kik$brkoutLg$excess_ratio))

Kik$brkoutPrem <- dplyr::filter(Kik$brkout, size == "Prem")
Kik$brkoutPrem <- dplyr::filter(Kik$brkoutPrem,
                                excess_ratio <= stats::median(Kik$brkoutPrem$excess_ratio))

<<<<<<< HEAD
=======
<<<<<<< HEAD
=======

>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$brkout <- rbind(Kik$brkoutSm,
                    Kik$brkoutMd,
                    Kik$brkoutLg,
                    Kik$brkoutPrem) %>%
  as.data.frame

<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#Useful vars for analysis ------------------------------------------------------
#Note: state == c(live, suspended, canceled) are mostly ignored in this analysis. 
#These ambiguous outcomes are generally not insightful
Kik$kiksrtSuccFail <- dplyr::filter(Kik$kiksrt, state == "successful" | state == "failed")

<<<<<<< HEAD
=======
=======
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#TOPIC: NAME ANALYSIS ##########################################################
#Determine the optimal name characteristics for a campaign.
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

#Compare character/word length of name to size of campaign
#By number of characters
Kik$charKik$kiksrt <- Kik$kiksrt
Kik$charKik$kiksrt$charCt <- nchar(Kik$charKik$kiksrt$name)
Kik$charKik$kiksrt <- dplyr::filter(Kik$charKik$kiksrt,
                                    state == "successful" | state == "failed")
#Small projects tend to have shorter names, large and premium projects tend to 
#have longer names, especially premium.
#Could indicate asking for more money requires more detailed descriptions
#to attract backers. 
Kik$charPlot <-
  ggplot2::ggplot() +
  geom_line(data = dplyr::filter(Kik$charKik$kiksrt, 
                                 size == "Small", state == "successful"), 
            aes(x = charCt, colour = size),
            stat = "bin", size = 1) +
  geom_line(data = dplyr::filter(Kik$charKik$kiksrt,
                                 size == "Med", state == "successful"),
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
  Shy$plotColsEy +
  xlab("Campaign Name Length (characters)") +
  ylab("Number of Projects") +
  labs(title = "Name Length of Successful Campaigns") +
  Kik$ggAutoTheme +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name = "Campaign Size")

<<<<<<< HEAD
#TOPIC: WORD ANALYSIS ----------------------------------------------------------
#Determine which words are associated with each category -----------------------
#Overall Top Words 
=======
<<<<<<< HEAD
#TOPIC: WORD ANALYSIS ----------------------------------------------------------
#Determine which words are associated with each category -----------------------
#Overall Top Words 
=======
#What words are associated with each category? ---------------------------------
#Overall
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
Kik$kikNm <- dplyr::tibble(name = Kik$kiksrt$name)
Kik$kikNmTkn <- tidytext::unnest_tokens(Kik$kikNm, word, name) 
Kik$kikNmTkn <- Kik$kikNmTkn %>%
  dplyr::anti_join(stop_words)
Kik$kikNmTkn %>%
  dplyr::count(word, sort = TRUE)

#Canceled projects have word "canceled in campaign name. Filtered out for redundancy. 
Kik$customStopWords <- bind_rows(tibble(word = c("canceled"),
                                        lexicon = c("custom")),
                                 stop_words)
Kik$kikNmTkn <- Kik$kikNmTkn %>%
  dplyr::anti_join(Kik$customStopWords)
#Top 100 words overall include album, film, project
Kik$kikNmAll <- Kik$kikNmTkn %>%
  dplyr::count(word, sort = TRUE)
Kik$kikNmAll <- Kik$kikNmAll[1:100,]
colnames(Kik$kikNmAll) <- c("Word", "Use Frequency")

#Main Category Word Freq Table Top 100
Kik$tknRankMain <- function(mainCat) {
  if(mainCat == "All Categories"){
    myTkn <- dplyr::filter(Kik$kiksrt, state == "successful")
    myTkn <- dplyr::tibble(name = myTkn$name) %>%
      tidytext::unnest_tokens(., word, name) 
    myTkn <- myTkn %>%
      dplyr::anti_join(Kik$customStopWords)
    
    myTknTable <- myTkn %>%
      dplyr::count(word, sort = TRUE)
    myTknTable <- myTknTable[1:100,]
    colnames(myTknTable) <- c("Word", "Use Frequency")
    myTknTable <- DT::datatable(myTknTable,
                                caption = "Table 1: Top 100 Words by Frequency for All Categories")
    
    return(myTknTable)
    
  } else {
    myMainCat <- mainCat
    
    myTkn <- dplyr::filter(Kik$kiksrt,
                           main_category == myMainCat, state == "successful")
    myTkn <- dplyr::tibble(name = myTkn$name) %>%
      tidytext::unnest_tokens(., word, name) 
    myTkn <- myTkn %>%
      dplyr::anti_join(Kik$customStopWords)
    
    myTknTable <- myTkn %>%
      dplyr::count(word, sort = TRUE)
    myTknTable <- myTknTable[1:100,]
    colnames(myTknTable) <- c("Word", "Use Frequency")
    myTknTable <- DT::datatable(myTknTable,
                                caption = paste("Table 1: Top 100 Words by Frequency For Category", myMainCat))
    
    return(myTknTable)
  } 
}

<<<<<<< HEAD
#Sub Category Word Freq Table Top 100 
=======
<<<<<<< HEAD
#Sub Category Word Freq Table Top 100 
=======
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
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
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
<<<<<<< HEAD
  myTknTable <- myTknTable[1:100,]
=======
<<<<<<< HEAD
  myTknTable <- myTknTable[1:100,]
=======
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
  return(myTknTable)
  
}

<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
#Main category word cloud Top 60
Kik$nmTknMainPlot <- function(mainCat) {
  if(mainCat == "All Categories") {
    myTkn <- dplyr::filter(Kik$kiksrt, state == "successful")
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
         paste("Top 60 Most Popular Words in All Categories"))
    myTknCld <- myTkn %>%
      count(word) %>%
      with(wordcloud::wordcloud(word, n, max.words = 60,
                                color = brewer.pal(8, "Dark2"),
                                scale = c(3,1.5)))
  }else{
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
}


#Alphabetical Category for Shiny
Kik$tknCatInput <- data.frame(mainCategory = unique(Kik$kiksrt$main_category))
Kik$tknCatInput$mainCategory <- as.character(Kik$tknCatInput$mainCategory)
Kik$tknCatInput[16,1] <- "All Categories"
Kik$tknCatInput <- arrange(Kik$tknCatInput, -desc(mainCategory))

#Kik$nmTknSubPlotInput <- data.frame(subCat = unique(Kik$kiksrt$category))
#Kik$nmTknSubPlotOutput <- purrr::pmap(Kik$nmTknSubPlotInput, Kik$nmTknSubPlot)

#Subcategory Word Cloud Top 60 
Kik$nmTknSubPlot <- function(subCat) {
<<<<<<< HEAD
  mySubCat <- subCat
=======
=======
#Main category word cloud
Kik$NmTknMainPlot <- function(mainCat) {
  myMainCat <- mainCat
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
  
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
<<<<<<< HEAD
       paste("Top 60 Most Popular Words in", subCat, "Category"))
=======
       paste("Top 60 Most Popular Words in", mainCat, "Category"))
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
  myTknCld <- myTkn %>%
    count(word) %>%
    with(wordcloud::wordcloud(word, n, max.words = 60,
                              color = brewer.pal(8, "Dark2"),
                              scale = c(3,1.5)))
<<<<<<< HEAD
=======
  
}

#Kik$NmTknSubPlotInput <- data.frame(subCat = unique(Kik$kiksrt$category))
#Kik$NmTknSubPlotOutput <- purrr::pmap(Kik$NmTknSubPlotInput, Kik$NmTknSubPlot)

#Subcategory Word Cloud
Kik$NmTknSubPlot <- function(subCat) {
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
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
<<<<<<< HEAD
       paste("Top 60 Most Popular Words in", subCat, "Category"))
=======
       paste("Top 60 =Most Popular Words in", subCat, "Category"))
>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
  myTknCld <- myTkn %>%
    count(word) %>%
    with(wordcloud::wordcloud(word, n, max.words = 60,
                              color = brewer.pal(8, "Dark2"),
                              scale = c(3,1.5)))
<<<<<<< HEAD
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
}

#Kik$nmTknSubPlotInput <- data.frame(subCat = unique(Kik$kiksrt$category))
#Kik$nmTknSubPlotOutput <- purrr::pmap(Kik$nmTknSubPlotInput, Kik$nmTknSubPlot)

#Are projects in different denominations interested in different categories?----
#Process
#1. Create a distribution of word frequency from entire kikstarter dataset
#2. Compare against word frequency distributions for different currencies
#3. Make note of words with drastically different rank position to highlight
#significant differences in the types of products a currency is interested in

#Top 100 words overall
Kik$tknRank100 <- Kik$kikNmTkn %>%
  dplyr::count(word, sort = TRUE)
Kik$tknRank100 <- cbind(Kik$tknRank100[1:100,], rank = 1:100)

#Main Category Word Freq Table by Currency
Kik$tknRankMainFx <- function(mainCat, currency) {
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




#Compare relative positions of top 100 ranked words by currency
Kik$tknFxRank <- function(mainCat, baseCurr, curr2) {
  if(identical(baseCurr, curr2)){
    failIdCurr <- data.frame(`Base Currency` = "",
                             `Relative Currency` = "",
                             `Relative Rank` = "")
    failIdCurrDt <- DT::datatable(failIdCurr,
                                  caption = "Please Choose Two Different Currencies")
    
    return(failIdCurrDt)
  } else {
    
    myBaseCurr <- Kik$tknRankMainFx(mainCat, baseCurr)[1:100,1] %>%
      na.omit(myBaseCurr)
    
    myCurr2 <- Kik$tknRankMainFx(mainCat, curr2)[1:100,1] %>%
      na.omit(myCurr2)
    
    minWordList <- min(length(myBaseCurr$word), length(myCurr2$word))
    
    myBaseCurr <- myBaseCurr[1:minWordList,] %>%
      cbind(1:minWordList)
    colnames(myBaseCurr)[2] <- "Rank"
    myCurr2 <- myCurr2[1:minWordList,] %>%
      cbind(1:minWordList)
    colnames(myCurr2)[2] <- "Rank"
    
    
    #Words found only in base currency
    onlyBase <- setdiff(myBaseCurr, myCurr2)
    #Words found only in currency 2
    onlyCurr2 <- setdiff(myCurr2, myBaseCurr)
    
    
    #Base currency word rank positions
    rankBase <- dplyr::filter(myBaseCurr, word %in% myCurr2$word)
    #Currency 2 word rank positions positions
    rankCurr2 <- dplyr::filter(myCurr2, word %in% myBaseCurr$word)
    
    
    #Compare word ranks against base currency.
    #These are words found in both currencies
    #Take note of big differences in rank
    #They explain cultural differences
    compRank <- cbind(baseCurr = arrange(rankBase, word),
                      curr2 = arrange(rankCurr2, word))
    compRank$curr2Pos <- compRank[,2] - compRank[,4]
    compRank <- compRank[, -c(3,4)]
    compRank <- arrange(compRank, compRank[,2])
    
    #Add back in gbp words not found in usd
    #Assign words not found in usd a position of NA
    outMerge <- dplyr::filter(myBaseCurr, !(word %in% myCurr2[,1]))
    
    outMerge$curr2Pos <- paste("Not in", curr2)
    colnames(outMerge)[1:3] <- c("baseCurr.word", "baseCurr.Rank", "curr2Pos")
    
    outFinal <- rbind(compRank, outMerge)
    #Final output. Ranked gbp words compared to usd rank.
    outFinal <- arrange(outFinal, outFinal[,2])
    colnames(outFinal) <- c(paste(baseCurr),
                            paste(baseCurr, "Absolute Word Rank"),
                            paste(curr2, "Relative Rank"))
    
    outFinalDt <- DT::datatable(outFinal, 
                                caption = paste0("Table 1: ",
                                                 baseCurr,
                                                 "/", 
                                                 curr2,
                                                 " Words in Common Rank Comparison",
                                                 " For Category",
                                                 " ",
                                                 mainCat))
    
    if(identical(outFinal[,3],(rep(paste("Not in", curr2),
                                   times = length(outFinal[,3]))))) {
      outFinal <- outFinal[1,]
      outFinal[1,] <- "No Words in Common"
      outFinalDt <- DT::datatable(outFinal, 
                                  caption = paste0("Table 1: ",
                                                   baseCurr,
                                                   "/",
                                                   curr2,
                                                   " Words in Common Rank Comparison",
                                                   " For Category",
                                                   " ",
                                                   mainCat, " (Max 100)"))
      
      return(outFinalDt)
    } else {
      notInFilter <- dplyr::filter(outFinal,
                                   !outFinal[3] == paste("Not in", curr2))
      notInFilterOut <- DT::datatable(notInFilter,
                                      caption = paste0("Table 1: ",
                                                       baseCurr,
                                                       "/",
                                                       curr2,
                                                       " Words in Common Rank Comparison",
                                                       " For Category",
                                                       " ",
                                                       mainCat, " (Max 100)"))
      return(notInFilterOut)
    }
  }
}




#Alphabetical inputs for shiny
Kik$tknFxRankInput <- data.frame(currency = unique(Kik$kiksrt$currency))
Kik$tknFxRankInput <- arrange(Kik$tknFxRankInput, -desc(currency))

Kik$tknFxRankInputCat <- data.frame(mainCategory = unique(Kik$kiksrt$main_category))
Kik$tknFxRankInputCat <- arrange(Kik$tknFxRankInputCat, -desc(mainCategory))
<<<<<<< HEAD
=======
=======
  
  
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















>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886

#TOPIC: CATEGORY ANALYSIS ######################################################
#Determine which main_category on average raised the most excess funds. ---------
#Dollars pledged and goal amount is aggregated per category to determine 
#which categories raised the most in excess funds.
#Super successful projects included.
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

#Design has the highest ratio of funds pledged per goal. 400% pledged to goal. 
Kik$catRatioResults <- pmap(Kik$mainCatRatioInput, Kik$mainCatRatio) %>%
  unlist %>%
  matrix(., nrow = 15, byrow = TRUE) %>%
  as.data.frame 
colnames(Kik$catRatioResults) <- c("Main Category", "Excess Funds Raised Multiple")

Kik$catRatioResults <- arrange(Kik$catRatioResults, desc(`Excess Funds Raised Multiple`)) %>% 
  DT::datatable(.,
  caption = "Table 1: This Table Compares Aggregated Excess Funds Raised for All Projects in Each Category.")



#TOPIC: SUMMARY STATS ##########################################################
#Create yearly timeline of new projects ----------------------------------------
Kik$timeline <- Kik$kiksrt

#Set each launch day to first of month for easy visual analysis
Kik$timeline$launchYear <- lubridate::year(Kik$timeline$launched) %>%
  as.factor()

Kik$timelineStCtInput <- data.frame(kikSize = c(1:5))
Kik$timelineStCtInput$kikSize[1] <- "All Projects"
Kik$timelineStCtInput$kikSize[2:5] <- levels(Kik$kiksrt$size) %>%
  as.character


Kik$timelineStCtPlot <- function(kikSize) {
  if(kikSize == "All Projects") {
    
    Kik$timelineStCt <- dplyr::filter(Kik$timeline, state == c("successful", "failed")) %>%
      dplyr::group_by(state, launchYear) %>%
      summarise(n())
    
    myPlot <-  Kik$timelineStCt %>%
      ggplot2::ggplot(., aes(x = launchYear, y = `n()`, fill = state)) +
      geom_col(position = "stack") +
      Shy$plotColsEy +
      scale_fill_manual(values = c(failed = "red", successful = "green"),
                        labels = c("Failed", "Successful"),
                        name = "State") +
      xlab("Year") +
      ylab("Number of Campaigns") +
      labs(title = "All Campaigns Launched by Year") +
      Kik$ggAutoTheme +
      coord_flip() 
    myPlot <- plotly::ggplotly(myPlot)
    
    return(myPlot)
  } else {
    mySize <- kikSize
    #mySubtitle <- dplyr::filter(Kik$sizeExplain, size == kikSize)[1,1]
    
    Kik$timelineStCt <- dplyr::filter(Kik$timeline,
                                      state == c("successful", "failed"),
                                      size == mySize) %>%
      dplyr::group_by(state, launchYear) %>%
      summarise(n())
    
    myPlot <- Kik$timelineStCt %>%
      ggplot2::ggplot(., aes(x = launchYear, y = `n()`, fill = state)) +
      geom_col(position = "stack") +
      Kik$ggAutoTheme +
      Shy$plotColsEy +
      scale_fill_manual(values = c(failed = "red", successful = "green"),
                        labels = c("Failed", "Successful"),
                        name = "State") +
      xlab("Year") +
      ylab("Number of Campaigns") +
      labs(title = paste(mySize, "Campaigns Launched by Year")) +
      theme(plot.subtitle = element_text(size = 20, hjust = 0.5,
                                         margin = margin(b = 15))) +
      coord_flip()
    myPlot <- plotly::ggplotly(myPlot)
    
    return(myPlot)
  }
}

#Breakdown of Number of Projects per Category ----------------------------------
Kik$catSmry <- Kik$kiksrtSuccFail

#Plot all Main categories
Kik$mainCatPlot <- function(mainCategory, kikSize) {
  if(kikSize == "All Sizes") {
    myMainCategory <- mainCategory
    
    mySubcatPlot <- Kik$catSmry %>%
      dplyr::group_by(main_category, category, state) %>%
      summarise(n()) %>%
      dplyr::filter(.,
                    main_category == myMainCategory &
                      !category == myMainCategory) %>%
      ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`,
                             fill = state)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c(successful = "green", failed = "red"),
                        labels = c("Failed", "Successful"),
                        name = "State") +
      Kik$ggAutoTheme +
      Shy$plotColsEy +
      coord_flip() +
      Kik$ggAutoTheme +
      xlab(paste(myMainCategory, "Subcategory")) +
      ylab("Count") +
      labs(title = paste("Number of Campaigns in", myMainCategory,
                         "Subcategories 2009-2018"))
    mySubcatPlot <- plotly::ggplotly(mySubcatPlot)
    return(mySubcatPlot)
  } else {
    myMainCategory <- mainCategory
    mySize <- kikSize
    
    mySubcatPlot <- Kik$catSmry %>%
      dplyr::filter(., size == mySize) %>%
      dplyr::group_by(main_category, category, state) %>%
      summarise(n()) %>%
      dplyr::filter(.,
                    main_category == myMainCategory &
                      !category == myMainCategory) %>%
      ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`,
                             fill = state)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = c(successful = "green", failed = "red"),
                        labels = c("Failed", "Successful"),
                        name = "State") +
      Shy$plotColsEy +
      Kik$ggAutoTheme +
      coord_flip() +
      xlab(paste(myMainCategory, "Subcategory")) +
      ylab("Count") +
      labs(title = paste("Number of", mySize, "Campaigns Launched in", myMainCategory,
                         "Subcategories 2009-2018"))
    mySubcatPlot <- plotly::ggplotly(mySubcatPlot)
    return(mySubcatPlot)
  }
}



Kik$mainCatPlotInput <- data.frame(mainCategory = unique(Kik$kiksrt$main_category))
Kik$mainCatPlotInput <- arrange(Kik$mainCatPlotInput, -desc(mainCategory))

Kik$mainCatPlotInputSz <- data.frame(kikSize = c(1:5))
Kik$mainCatPlotInputSz$kikSize[1] <- "All Sizes"
Kik$mainCatPlotInputSz$kikSize[2:5] <- levels(Kik$kiksrt$size) %>%
  as.character

#Must convert fct to char for pmap to input valid argument to Kik$mainCatPlot
#Kik$mainCatPlotInput$mainCategory <- as.character(Kik$mainCatPlotInput$mainCategory)
#Kik$mainCatPlotOutput <- pmap(Kik$mainCatPlotInput, Kik$mainCatPlot)

#Average length of a campaign  -------------------------------------------------
Kik$projLenMedian <- dplyr::filter(Kik$kiksrt, state == "successful")
#Median successful campaign length is 30 days. Knowing this helps client plan
#timeline for project fundraising
Kik$projLenMedian <- stats::median(Kik$projLenMedian$projLen)

Kik$projLenMedianPlot <- ggplot2::ggplot(Kik$kiksrt, aes(x = projLen)) +
  geom_bar(fill = "blue",
           stat = "count") +
  xlab("Days") +
  ylab("Number of Campaigns") +
  labs(title = "Length of Kickstarter Campaigns 2009-2018") +
  theme(plot.title = element_text(hjust = 0.5)) 

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
#Small failed projects only raise (Average ~16.7%, Median ~10%) of 
#their required capital
mean(Kik$kikPartialFailSmall$partialFail)
stats::median(Kik$kikPartialFailSmall$partialFail)

#Med
Kik$kikPartialFailMed <- dplyr::filter(Kik$kikPartialFail, size == "Med",
                                       !partialFail == 0)
Kik$kikPartialFailMedZero <- dplyr::filter(Kik$kikPartialFail, size == "Med", 
                                           partialFail == 0)
#~28k Med projects raised some money. ~7.3k Med projects raised no money
dplyr::count(Kik$kikPartialFailMed)
dplyr::count(Kik$kikPartialFailMedZero)
#Med failed projects only raise (Average ~13%, Median ~6.2%) of 
#their required capital
mean(Kik$kikPartialFailMed$partialFail)
stats::median(Kik$kikPartialFailMed$partialFail)

#Large
Kik$kikPartialFailLarge <- dplyr::filter(Kik$kikPartialFail, size == "Large",
                                         !partialFail == 0)
Kik$kikPartialFailLargeZero <- dplyr::filter(Kik$kikPartialFail, size == "Large", 
                                             partialFail == 0)
#~36k Large projects raised some money. ~8.5k Large projects raised no money
dplyr::count(Kik$kikPartialFailLarge)
dplyr::count(Kik$kikPartialFailLargeZero)
#Large failed projects only raise (Average ~11%, Median ~4.3%) of 
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
#Prem failed projects only raise (Average ~9%, Median ~2%) of 
#their required capital
mean(Kik$kikPartialFailPrem$partialFail)
stats::median(Kik$kikPartialFailPrem$partialFail)

#Plot Distribution of Partially Raised Funds 
Kik$partialFailPlot <- function(size) {
  mySize <- size
  myCut <- dplyr::filter(Kik$kikPartialFail, size == mySize)
  myCut <- cut(myCut$partialFail*100, breaks = seq(0,100, by = 10)) %>%
    table %>%
    data.frame
  colnames(myCut) <- c("Breaks", "Count")
  
  myPlot <- ggplot2::ggplot(myCut, aes(x = Breaks, y = Count, fill = Breaks)) +
    geom_histogram(stat = "identity", fill = "#FFE700") +
    Shy$plotColsEy +
    theme(axis.text.x = element_text(angle = 90,
                                     size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(hjust = 0.5,
                                    size = 28),
          legend.position = "none",
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 24)) +
    xlab("Percent Bins (%)") +
    ylab("Number of Kickstarter Campaigns") +
    labs(title = paste("Percent of Goal Reached:", "Failed", mySize, "Campaigns")) +
    coord_flip()
  myPlot <- plotly::ggplotly(myPlot)
  return(myPlot)
}

<<<<<<< HEAD
Kik$partialFailPlotInputs <- data.frame(size = c("Small", "Med", "Large", "Prem"))
#purrr::pmap(Kik$partialFailPlotInputs, Kik$partialFailPlot)
=======
Kik$partialFailPlotInputs <- data.frame(size = c("Small", "Mid", "Large", "Prem"))
<<<<<<< HEAD
#purrr::pmap(Kik$partialFailPlotInputs, Kik$partialFailPlot)
=======
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





















>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4
>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886


