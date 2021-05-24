#How close do different size failed campaigns come to reaching their goal amount? ----
#Use success quantiles to group project sizes. See 03_
projSmall <- filter(kikstrt, usd_goal_real < 1302) 
projMid <- filter(kikstrt, 1302 <= usd_goal_real & usd_goal_real < 3838) 
projLarge <- filter(kikstrt, 3838 <= usd_goal_real & usd_goal_real < 10000) 
projPrem <- filter(kikstrt, 10000 <= usd_goal_real)

#Analyze only failed projects that raised some amount of money
#Reason: We are not interested in projects that raised no money because it is obvious
#that if a project is not raising money for an extended period then it is very likely to fail.
#We want to help client figure out their odds of success if some money has 
#been raised and there is evidence of some fundraising traction. 

#Small failed projects
projSmallFail <- filter(projSmall, state == "failed")
#Create ratio of % of goal reached 
projSmallFail$failedRatio <- projSmallFail$usd_pledged_real/projSmallFail$usd_goal_real
#20919 small failed projects raised some money. 8046 small projects raised no money
count(filter(projSmallFail, !failedRatio == 0))
count(filter(projSmallFail, failedRatio == 0))
#Small failed projects only raise about ~16.7% of their required capital
failSmallRatio <- filter(projSmallFail, !failedRatio == 0)
mean(failSmallRatio$failedRatio)



projSmallFail <- filter(projSmall, state == "failed")
#Create ratio of % of goal reached 
projSmallFail$failedRatio <- projSmallFail$usd_pledged_real/projSmallFail$usd_goal_real
#20919 small failed projects raised some money. 8046 small projects raised no money
count(filter(projSmallFail, !failedRatio == 0))
count(filter(projSmallFail, failedRatio == 0))
#Small failed projects only raise about ~16.7% of their required capital
failSmallRatio <- filter(projSmallFail, !failedRatio == 0)
mean(failSmallRatio$failedRatio)

#Mid failed projects
projMidFail <- filter(projMid, state == "failed")
#Create ratio of % of goal reached 
projMidFail$failedRatio <- projMidFail$usd_pledged_real/projMidFail$usd_goal_real
#28682 Mid failed projects raised some money. 6977 Mid projects raised no money
count(filter(projMidFail, !failedRatio == 0))
count(filter(projMidFail, failedRatio == 0))
#Mid failed projects only raise about ~13% of their required capital
failMidRatio <- filter(projMidFail, !failedRatio == 0)
mean(failMidRatio$failedRatio)


#Large failed projects
projLargeFail <- filter(projLarge, state == "failed")
#Create ratio of % of goal reached 
projLargeFail$failedRatio <- projLargeFail$usd_pledged_real/projLargeFail$usd_goal_real
#36909 Large failed projects raised some money. 8204 Large projects raised no money
count(filter(projLargeFail, !failedRatio == 0))
count(filter(projLargeFail, failedRatio == 0))
#Large failed projects only raise about ~11% of their required capital
failLargeRatio <- filter(projLargeFail, !failedRatio == 0)
mean(failLargeRatio$failedRatio)

#Prem failed projects
projPremFail <- filter(projPrem, state == "failed")
#Create ratio of % of goal reached 
projPremFail$failedRatio <- projPremFail$usd_pledged_real/projPremFail$usd_goal_real
#72553 Prem failed projects raised some money. 15429 Prem projects raised no money
count(filter(projPremFail, !failedRatio == 0))
count(filter(projPremFail, failedRatio == 0))
#Prem failed projects only raise about ~9% of their required capital
failPremRatio <- filter(projPremFail, !failedRatio == 0)
mean(failPremRatio$failedRatio)


#Plot smallRatio distribution
smallRatioDist <- cut(failSmallRatio$failedRatio, breaks = seq(0,1, by = 0.1)) %>%
  table %>%
  data.frame
colnames(smallRatioDist) <- c("Breaks", "Count")
ggplot2::ggplot(smallRatioDist, aes(x = Breaks, y = Count, fill = Breaks)) +
  geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete() +
  xlab("Percent Breaks") +
  ylab("Number of Kickstarter Campaigns") +
  labs(title = "Percent of Goal Amount Raised for Small Failed Projects")

#Plot MidRatio distribution
MidRatioDist <- cut(failMidRatio$failedRatio, breaks = seq(0,1, by = 0.1)) %>%
  table %>%
  data.frame
colnames(MidRatioDist) <- c("Breaks", "Count")
ggplot2::ggplot(MidRatioDist, aes(x = Breaks, y = Count, fill = Breaks)) +
  geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete() +
  xlab("Percent Breaks") +
  ylab("Number of Kickstarter Campaigns") +
  labs(title = "Percent of Goal Amount Raised for Mid Failed Projects")

#Plot LargeRatio distribution
LargeRatioDist <- cut(failLargeRatio$failedRatio, breaks = seq(0,1, by = 0.1)) %>%
  table %>%
  data.frame
colnames(LargeRatioDist) <- c("Breaks", "Count")
ggplot2::ggplot(LargeRatioDist, aes(x = Breaks, y = Count, fill = Breaks)) +
  geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete() +
  xlab("Percent Breaks") +
  ylab("Number of Kickstarter Campaigns") +
  labs(title = "Percent of Goal Amount Raised for Large Failed Projects")

#Plot PremRatio distribution
PremRatioDist <- cut(failPremRatio$failedRatio, breaks = seq(0,1, by = 0.1)) %>%
  table %>%
  data.frame
colnames(PremRatioDist) <- c("Breaks", "Count")
ggplot2::ggplot(PremRatioDist, aes(x = Breaks, y = Count, fill = Breaks)) +
  geom_histogram(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_discrete() +
  xlab("Percent Breaks") +
  ylab("Number of Kickstarter Campaigns") +
  labs(title = "Percent of Goal Amount Raised for Prem Failed Projects")
































