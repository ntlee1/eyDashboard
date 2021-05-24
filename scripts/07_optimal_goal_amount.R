#Expected Pledge Amount Per Backer Based On Project Size And Category ----------
test <- (filter(projSmall, !backers == 0, 10 <= backers, backers <= 50,
                state == "successful", 500 <= usd_goal_real, usd_goal_real <= 1000 ))
uniqueTest <- unique(projSmall$category)
test1 <- filter(projSmall, category == uniqueTest[6])
test2 <- (sum(test1$usd_pledged_real)/sum(test1$backers)) %>%
  print

test <- filter(projSmall, 800 <= usd_pledged_real, usd_pledged_real <= 1000, !backers == 0,
               state == "successful")
test$backers

view(test)
