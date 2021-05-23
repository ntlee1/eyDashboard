#Is Kickstarter Pledge Amount Data Accurate?------------------------------------
#I suspect data scraping errors in fx conversion columns usd_pledged and usd_pledged_real
#Logically, if a project is in base currency USD,
#columns "pledged", "usd_pledged", and "usd_pledged_real" should be equivalent.
#They all measure the same metric, total USD dollars pledged
#This is not always the case. There are major discrepancies. 
#For simplicity, lets analyze USD projects as the FX conversion is 1:1
#TODO MAYBE check NAs for other currencies and import external data for use

#Pledge Accuracy: Kickstarter vs Fixer.io ---------------------------------------
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

#Can we rely on Fixer.io to provide 100% accuracy for other denominations?------
#Validation Process: 
#1. Import external USD FX conversion data for time period 
#2. Convert pledged column manually and compare to fixer.io 
#3. If relatively similar then fixer.io is assumed accurate 
#Note: External data will not match exact fx timestamp from fixer.io,
#some variation expected from daily price fluctuations.
unique(kikstrt$currency)
#view(table(kikstrt$currency))

#Step1: 
#Import FX data from https://www.ofx.com/en-us/forex-news/historical-exchange-rates/
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
#Fixer.io is soft accurate ofx_usd_pledged column. Kickstarter is not
ofxPledgeCompare <- baseAud$usd_pledged_real/baseAud$ofx_usd_pledged
#1715 NAs detected. These are projects with zero money raised. Remove
sum(is.na(baseAud$usd_pledged_real))
ofxPledgeCompare <- na.omit(ofxPledgeCompare)
range(ofxPledgeCompare)
var(ofxPledgeCompare)
#view(baseAud)
