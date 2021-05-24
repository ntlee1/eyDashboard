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