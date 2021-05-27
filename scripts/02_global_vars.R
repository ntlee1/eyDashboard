#Global vars will be reused often throughout scripts
#Filters -----------------------------------------------------------------------
kikSuccess <- filter(kikstrt, state == "successful")
summary(kikSuccess$usd_goal_real)

#Filter out super successful kickstarter campaigns -----------------------------
#Reason: Useful for modeling realistic fundraisers where pledge amount is 
#similar, to goal amount
#Define breakout success as ratio of pledge dollars to goal amount and keep
#only campaigns below median ratio 

#Step 1: Find and filter out breakout success kickstarter campaigns 
#Interested only in successful campaigns for success modeling
breakoutFilter <- kikstrt
breakoutFilter <- filter(breakoutFilter, state == "successful")
#Example: goalExceedRatio of 1.4 means pledge funds matched 140% of the goal amount
breakoutFilter$goalExceedRatio <- breakoutFilter$usd_pledged_real/breakoutFilter$usd_goal_real
breakoutFilterSummary <- summary(breakoutFilter$goalExceedRatio)

#Step 2: Find median for each project size classification
#Each size category will have different median ratio. Larger projects will naturally
#have a smaller multiplier that signifies excessive pledge funds raised 
breakoutSmall <- dplyr::filter(breakoutFilter, size == "Small")
breakoutSmall <- dplyr::filter(breakoutSmall,
                               goalExceedRatio <= stats::median(breakoutSmall$goalExceedRatio))

breakoutMid <- dplyr::filter(breakoutFilter, size == "Mid")
breakoutMid <- dplyr::filter(breakoutMid,
                               goalExceedRatio <= stats::median(breakoutMid$goalExceedRatio))

breakoutLarge <- dplyr::filter(breakoutFilter, size == "Mid")
breakoutLarge <- dplyr::filter(breakoutLarge,
                             goalExceedRatio <= stats::median(breakoutLarge$goalExceedRatio))

breakoutPrem <- dplyr::filter(breakoutFilter, size == "Mid")
breakoutPrem <- dplyr::filter(breakoutPrem,
                               goalExceedRatio <= stats::median(breakoutPrem$goalExceedRatio))


breakoutFilter <- rbind(breakoutSmall,
                          breakoutMid,
                          breakoutLarge,
                          breakoutPrem) %>%
  as.data.frame































