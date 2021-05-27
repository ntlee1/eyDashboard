#Which main_category has the highest ratio of funds pledged vs goal -----------
#Super successful projects included
catRatio <- filter(kikstrt, state == "successful")
catRatio$`pledged/goal` <- catRatio$pledged/catRatio$usd_goal_real

mainCatRatio <- function(mainCategory) {
  myCat <- mainCategory
  filterMe <- filter(catRatio, main_category == myCat)
  filterMe <- sum(filterMe$usd_pledged_real)/sum(filterMe$usd_goal_real)
  filterMe <- round(filterMe, digits = 2)
  cbind(myCat, filterMe)
   
}

mainCatRatioInput <- data.frame(x = unique(kikstrt$main_category))
colnames(mainCatRatioInput) <- "mainCategory"
mainCatRatioInput$mainCategory <- as.character(mainCatRatioInput$mainCategory)
catRatioResults <- pmap(mainCatRatioInput, mainCatRatio) 
catRatioResults <- as.data.frame(matrix(unlist(catRatioResults), nrow = 15, byrow = TRUE))
colnames(catRatioResults) <- c("Category", "pledged/goal")
#Design has the highest ratio of funds pledged per goal. 400% of goal reached
catRatioResults <- arrange(catRatioResults, desc(`pledged/goal`))








#What are the most common words per category? ----------------------------------