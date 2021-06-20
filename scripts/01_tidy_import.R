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
<<<<<<< HEAD
                 "rlang",
                 "DT",
                 "ggrepel")
lapply(Init$pkgReq, require, character.only = TRUE)
=======
                 "rlang")
lapply(Init$pkgReq, require, character.only = TRUE)

<<<<<<< HEAD
=======


>>>>>>> b8810510fc1d6fa3c698f1545a21df855412e0b4





























>>>>>>> 64d38e589a9ae0f3e4836c1d1dcb096fdad1e886
