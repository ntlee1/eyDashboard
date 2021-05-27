#Overall summarization stats on entire dataset
#Create yearly timeline of new projects ----------------------------------------
timelineKik <- kikstrt
#Set each launch day to first of month for easy visual analysis
timelineKik$launchYear <- lubridate::year(timelineKik$launched) %>%
  as.factor()

timelineStateCount <- dplyr::filter(timelineKik, state == c("successful", "failed")) %>%
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

#Plot all subcategories
subcatPlots <- function(mainCategory) {
  myMainCategory <- mainCategory
  mySubcatPlot <- dplyr::filter(categorySummaryCount,
                         main_category == myMainCategory & !category == myMainCategory) %>%
    ggplot2::ggplot(., aes(x = reorder(category, -`n()`), y = `n()`, fill = category)) +
    geom_col(position = "stack") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    xlab(paste(myMainCategory, "Subcategory")) +
    ylab("Count") +
    labs(title = paste("Number of Projects in", myMainCategory, "Subcategories 2009-2018")) +
    scale_fill_discrete()
  
  plotly::ggplotly(mySubcatPlot)
}

subCatPlotInput <- data.frame(mainCategory = unique(kikstrt$main_category))
#as.Character for filter
subCatPlotInput$mainCategory <- as.character(subCatPlotInput$mainCategory)
pmap(subCatPlotInput, subcatPlots)



#Average length of a campaign  -------------------------------------------------
daysLengthMedian <- dplyr::filter(kikstrt, state == "successful")
#Median successful campaign length is 30 days. Knowing this helps client
#plan anticipated timeline for project fundraising
daysLengthMedian <- stats::median(daysLengthMedian$projDays)





















