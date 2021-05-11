# import ------------------------------------------------------------------
kikstrt <- readr::read_csv(here::here("data", "Kickstarter Data", "ksprojects.csv"))
print(kikstrt, tibble.width = "inf")
kikstrt <- as_tibble(kikstrt)
colnames(kikstrt)

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

#3797 Projects have no value for usd pledged. No action taken now until needed
view(kikstrt[naOut$`usd pledged`,])
usdPlgNaRows <- naOut$`usd pledged`


#usd pledged vs usd_pledged_real data scraping accuracy check------------------
#Logically, if a project is in base currency USD,
#columns "pledged", "usd_pledged", and "usd_pledged_real" should be equivalent.
#They all measure the same metric, total USD dollars pledged
#This is not always the case. There are major discrepancies. 
#I suspect data scraping errors. 

#For simplicity, lets analyze USD projects as the FX conversion is 1:1

#Filter all projects with base USD currency
#295,365 USD Projects
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

#Test fixer.io accuracy for 3 non-USD currencies
#USD AUD
ofxUsdAud <- readxl::read_xlsx(here::here("data", "ofx_fx_data", "USDAUD_ofx.xlsx"))
baseAud <- filter(kikstrt, `currency` == "AUD")
#TODO Match dates of USD AUD and multiply pledged by currency value















































