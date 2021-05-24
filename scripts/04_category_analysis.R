#Which main_category has the highest ratio of funds pledged vs goal? -----------
mainUnique <- c(unique(kikstrt$main_category))

#TODO MAYBE Generalize this function
mainRatio <- function(x) {
  filterMe <- filter(kikstrt, kikstrt$main_category == mainUnique[x])[c("usd_pledged_real",
                                                                 "usd_goal_real")]
  sum(filterMe$usd_pledged_real)/sum(filterMe$usd_goal_real)
}

mainSuccess <- map(c(1:length(mainUnique)), mainRatio) %>%
  flatten %>%
  unlist %>%
   cbind(mainUnique) %>%
   data.frame

#Category "Design" is has the highest ratio of funds pledged vs goal
mainSuccess <- mainSuccess[,c(2,1)]
colnames(mainSuccess) <- c("main_category",
                          "Pledged Funds/Goal Funds")
mainSuccess <- arrange(mainSuccess, desc(`Pledged Funds/Goal Funds`))

#Which subcategory of Design has the highest ratio of funds pledged vs goal? ----
designUnique <- filter(kikstrt, kikstrt$main_category == "Design")
designUnique <- unique(designUnique$category)
designRatio <- function(x) {
  filterMe <- filter(kikstrt, kikstrt$category == designUnique[x])[c("usd_pledged_real",
                                                                        "usd_goal_real")]
  sum(filterMe$usd_pledged_real)/sum(filterMe$usd_goal_real)
}

designSuccess <- map(c(1:length(designUnique)), designRatio) %>%
  flatten %>%
  unlist %>% 
  cbind(designUnique) %>%
  data.frame

designSuccess <- designSuccess[,c(2,1)]
colnames(designSuccess) <- c("design_category",
                           "Pledged Funds/Goal Funds")
#Typography has raised on average 40% more than its goal amount
designSuccess <- arrange(designSuccess, desc(`Pledged Funds/Goal Funds`))
















































#What are the most common words per category? ----------------------------------