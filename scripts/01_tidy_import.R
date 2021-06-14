#Personal preference for no scientific notation
options(scipen = 999)

#Environments start with uppercase
#For initialization
Init <- new.env()

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






























