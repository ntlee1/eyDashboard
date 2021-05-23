#Create yearly timeline of new projects ----------------------------------------
timelineKik <- kikstrt
timelineKik$state <- as.factor(timelineKik$state)
#Set each launch day to first of month for easy visual analysis
timelineKik$launchYear <- lubridate::year(timelineKik$launched) %>%
  as.factor()

timelineStateCount <- filter(timelineKik, state == c("successful", "failed")) %>%
dplyr::group_by(state, launchYear) %>%
  summarise(n())

timelineStateCountPlot <-  timelineStateCount %>%
  ggplot2::ggplot(., aes(x = launchYear, y = `n()`, fill = state)) +
  geom_col(position = "dodge") +
  scale_fill_discrete(name = "State", labels = c("Successful", "Failed")) +
  xlab("Year") +
  ylab("Number of Kickstarter Campaigns") +
  labs(title = "Kickstarter Campaigns Launched by Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(from = 0, to = 25000, by = 2500))
  
timelineStateCountPlot <- plotly::ggplotly(timelineStateCountPlot)

#Breakdown of Number of Projects per Category ----------------------------------
categorySummary <- filter(kikstrt, state == c("successful", "failed")) 
categorySummary$category <- as.factor(categorySummary$category)
categorySummary$main_category <- as.factor(categorySummary$main_category)
categorySummaryCount <- categorySummary %>%
  dplyr::group_by(main_category, category) %>%
  summarise(n())

#15 Unique Main Categories
catNms <- unique(categorySummaryCount$main_category)
#159 Unique Sub Categories
subcatNms <- unique(categorySummaryCount$category)

#Plot all subcategories
#Call index number of catNms for specific plot
#non-generalized function
subcatPlots <- function(num) {
  
  myCatNms <- data.frame(x = catNms)
  myCatNms$x <- as.character(myCatNms$x)
  myIndex <- myCatNms[num,]
  mySubcat <- eval(substitute(myIndex))

  mySubcatPlot <- filter(categorySummaryCount,
                         main_category == mySubcat & !category == mySubcat) %>%
    ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`, fill = category)) +
    geom_col(position = "stack") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    xlab(paste(mySubcat, "Subcategory")) +
    ylab("Count") +
    labs(title = paste("Number of Projects in", mySubcat, "Subcategories 2009-2018")) +
    scale_fill_discrete()
  
  plotly::ggplotly(mySubcatPlot)
}

#Number of Projects By Currency ------------------------------------------------




subcatPlots(1)





