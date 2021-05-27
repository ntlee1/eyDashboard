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

mainCatRatio(Food)



subCatPlotOutput[1]



















testMe <- subCatPlotOutput[1]












































