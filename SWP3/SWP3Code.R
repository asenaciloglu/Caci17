setwd("~/Caci17/SWP3")
bluetooth <- read.csv("indivData.csv")

nrow(bluetooth)
summary(bluetooth)
str(bluetooth)

seg.summ <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}
