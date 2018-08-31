source("1_plot_data.r")

# 2_get_catches.r
# just need recent catches for PlanBsmooth and FSD

# remember to set working directory to source file location

# catch estimates from tables A34, A35 in metric tons
catch.dat <- read.csv("..\\data\\fluke_catches.csv", header = TRUE)

mean.recent.catch.old <- mean(catch.dat$Old.Catch[catch.dat$Year %in% seq(2015, 2017)])
mean.recent.catch.new <- mean(catch.dat$New.Catch[catch.dat$Year %in% seq(2015, 2017)])

c(mean.recent.catch.old, mean.recent.catch.new)

