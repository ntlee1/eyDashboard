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
            "tidytext")
lapply(pkgReq, require, character.only = TRUE)

#Import Data -------------------------------------------------------------------
kikstrt <- readr::read_csv(here::here("data", "Kickstarter Data", "ksprojects.csv"))
#print(kikstrt, tibble.width = "inf")
kikstrt <- as_tibble(kikstrt)
colnames(kikstrt)

#Reclassify state == undefined ---------------------------------------------------
#When state == undefined, I noticed usd pledged is NA in every observation 
#It appears the API relies on state being defined to give usd pledged a value 
#We can still infer campaign success by usd_pledged_real >= usd_goal_real
unique(kikstrt$state)
stateUndef <- (filter(kikstrt, state == "undefined"))
sum(is.na(stateUndef$`usd pledged`)) == nrow(stateUndef)
stateUndefSuccess <- (filter(kikstrt, state == "undefined" & usd_pledged_real >= usd_goal_real))
stateUndefFail <- (filter(kikstrt, state == "undefined" & usd_pledged_real <= usd_goal_real))
kikstrt <- rbind(kikstrt, stateUndefSuccess, stateUndefFail)

#Cleanse NAs -------------------------------------------------------------------
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

#Fix misleading deadline dates -------------------------------------------------
#Some launch dates are misformatted as 1970-01-01
#This is not a valid kickstarter was founded in 2009
#It appears the web scraping API defaulted to the POSIX start date for an unknown reason
#For practical purposes I have set the launch date 1 month before deadline
mod1970 <- (filter(kikstrt, launched < "1971-01-01"))
# %m+% adds one month. Imported from lubridate
kikstrt <- full_join(mod1970, kikstrt)
#Append modifications then filter out erroneous data
kikstrt <- filter(kikstrt, launched > "1971-01-01")























