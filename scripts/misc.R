#Not having to use literal deparse substitute
myCatNms <- data.frame(x = catNms)
myCatNms$x <- as.character(myCatNms$x)
myCatNms[1,]

hee <- "Art"

heyo <- myCatNms[myCatNms$x == hee,]
eval(substitute(heyo))

catNms







subcatPlots <- function(subcatName) {
  
  mySubcat <- deparse(substitute(subcatName))
  
  filter(categorySummaryCount, main_category == mySubcat & !category == mySubcat) %>%
    ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`, fill = category)) +
    geom_col(position = "stack") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    xlab(paste(mySubcat, "Subcategory")) +
    ylab("Count") +
    labs(title = paste("Number of Projects in", mySubcat, "Subcategories 2009-2018")) +
    scale_fill_discrete()
}


#Copy to notes
mainCatRatio <- function(mainCategory) {
  catRatio <- filter(kikstrt, state == "successful")
  catRatio$`pledged/goal` <- catRatio$pledged/catRatio$usd_goal_real
  
  mainCatRatioInput <- data.frame(x = unique(kikstrt$main_category))
  colnames(mainCatRatioInput) <- "mainCategory"
  
  #Convert input name to character to find matching index in possible arguments
  #Input is seen as a variable, convert that variable to character
  myCatIndex <- which(mainCatRatioInput$mainCategory == deparse(substitute(mainCategory)))
  myCat <-mainCatRatioInput$mainCategory[myCatIndex]
  
  filterMe <- filter(catRatio, main_category == myCat)
  filterMe <- sum(filterMe$usd_pledged_real)/sum(filterMe$usd_goal_real)
  return(list(filterMe, myCat))
}




Kik$duplicateIdKik <- dplyr::filter(Kik$kiksrt, ID %in% Kik$duplicateId$Var1)
#Remove duplicate data
Kik$kiksrt <- Kik$kiksrt[!duplicateinstd(Kik$kiksrt[,c("ID")]),]
#TRUE
length(Kik$kiksrt$ID) == length(unique(Kik$kiksrt$ID))














################
#Working output
#Find which words are found in both datasets
test1 <- Kik$tknRankMain("Food", "GBP")[1:100,1] %>%
  cbind(., 1:100)
test2 <- Kik$tknRankMain("Food", "USD")[1:100,1] %>%
  cbind(., 1:100)



#Elements found only in test1
testDiff1 <- setdiff(test1, test2)

#Elements found only in test2
testDiff2 <- setdiff(test2, test1)

#Elements found in both 
#Test1 rank positions
testDiff3 <- dplyr::filter(test1, word %in% test2$word)

#Test2 rank positions
testDiff4 <- dplyr::filter(test2, word %in% test1$word)

#Compare Ranks
#Take note of big differences in rank
#They explain cultural differences
compMe <- cbind(gbp = arrange(testDiff3, word), usd = arrange(testDiff4, word))
compMe$usdPosition <- compMe$`gbp.1:100` - compMe$`usd.1:100`
compMe <- subset(compMe, select = -c(usd.word, `usd.1:100`))
compMe <- arrange(compMe, `gbp.1:100`)
#Add back in gbp words not found in usd
#Assign usd position of NA
testNow <- dplyr::filter(test1, !(word %in% test2$word))
testNow$usdPosition <- 99999
colnames(testNow)[1:2] <- c("gbp.word", "gbp.1:100")

goodTest <- rbind(compMe, testNow)
#Final output. Ranked gbp words compared to usd rank.
goodTest <- arrange(goodTest, `gbp.1:100`)

#################




###
###############################################
ggplot2::ggplot(Kik$kiksrt, aes(x = projLen, y = ID)) +
  geom_col(fill = "blue") +
  xlab("Days") +
  labs(title = "Length of Kickstarter Projects 2009-2018") +
  scale_y_discrete(labels = NULL, breaks = NULL) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 













































