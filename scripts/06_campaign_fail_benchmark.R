#How close do different size failed campaigns come to reaching their goal amount? ----
kikPartialFail <- rbind(kikSmall, kikMid, kikLarge, kikPrem)
kikPartialFail <- dplyr::filter(kikPartialFail, state == "failed")
kikPartialFail$partialFail <- kikPartialFail$usd_pledged_real/kikPartialFail$usd_goal_real

#Overall 159063 projects raised some money. 38656 small projects raised no money
count(filter(kikPartialFail, !partialFail == 0))
count(filter(kikPartialFail, partialFail == 0))

#Small
kikPartialFailSmall <- filter(kikPartialFail, size == "Small")
kikPartialFailSmall <-dplyr::filter(kikPartialFailSmall, !partialFail == 0)
kikPartialFailSmallZero <- dplyr::filter(kikPartialFail, partialFail == 0)
#20919 Small projects raised some money. 8046 Small projects raised no money
dplyr::count(kikPartialFailSmall)
dplyr::count(kikPartialFailSmallZero)
#Small failed projects only raise about (Average ~16.7%, Median ~10%) of 
#their required capital
mean(kikPartialFailSmall$partialFail)
stats::median(kikPartialFailSmall$partialFail)

#Mid
kikPartialFailMid <- filter(kikPartialFail, size == "Mid")
kikPartialFailMid <-dplyr::filter(kikPartialFailMid, !partialFail == 0)
kikPartialFailMidZero <- dplyr::filter(kikPartialFail, partialFail == 0)
#28682 Mid projects raised some money. 38656 Mid projects raised no money
dplyr::count(kikPartialFailMid)
dplyr::count(kikPartialFailMidZero)
#Mid failed projects only raise about (Average ~13%, Median ~6.2%) of 
#their required capital
mean(kikPartialFailMid$partialFail)
stats::median(kikPartialFailMid$partialFail)

#Large
kikPartialFailLarge <- filter(kikPartialFail, size == "Large")
kikPartialFailLarge <-dplyr::filter(kikPartialFailLarge, !partialFail == 0)
kikPartialFailLargeZero <- dplyr::filter(kikPartialFail, partialFail == 0)
#36909 Large projects raised some money. 38656 Large projects raised no money
dplyr::count(kikPartialFailLarge)
dplyr::count(kikPartialFailLargeZero)
#Large failed projects only raise about (Average ~11%, Median ~4.3%) of 
#their required capital
mean(kikPartialFailLarge$partialFail)
stats::median(kikPartialFailLarge$partialFail)

#Plot Distribution of Partially Raised Funds -----------------------------------
partialFailPlot <- function(size) {
  mySize <- size
  myCut <- filter(kikPartialFail, size == mySize)
  myCut <- cut(myCut$partialFail, breaks = seq(0,1, by = 0.1)) %>%
    table %>%
    data.frame
  colnames(myCut) <- c("Breaks", "Count")
  
  myPlot <- ggplot2::ggplot(myCut, aes(x = Breaks, y = Count, fill = Breaks)) +
    geom_histogram(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_discrete() +
    xlab("Percent Breaks") +
    ylab("Number of Kickstarter Campaigns") +
    labs(title = paste("Percent of Goal Amount Raised for", mySize, "Failed Projects"))
  return(myPlot)
}

partialFailPlotInputs <- data.frame(size = c("Small", "Mid", "Large", "Prem"))
pmap(partialFailPlotInputs, partialFailPlot)































