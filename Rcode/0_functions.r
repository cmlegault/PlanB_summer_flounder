# 0_functions.r
# functions used in the analyses

# remember to set working directory to source file location

# only need to run next two lines if have not already installed PlanBsmooth package
#library(devtools)
#devtools::install_github("cmlegault/PlanBsmooth")

library(PlanBsmooth)  # package to standardize the approach
library(dplyr)        # data wrangling
library(tidyr)        # data wrangling
library(ggplot2)      # pretty plots
library(viridis)      # nice colors


############################################################################################
ApplyFSD <- function(surveys,          # matrix with Year in first column and surveys in remaining columns
                     npoint = 5,       # number of points in regression for slope (default = 5)
                     termyr = NA,      # terminal year to use when computing slope (default = last year)
                     Kp     = 0.75,    # gain for first derivative (default = 0.75)
                     Kd     = 0.50     # gain for second derivative (default = 0.50)
){
  
  # select data to use
  if(is.na(termyr)) termyr <- max(surveys$Year, na.rm=TRUE)
  dat.use <- filter(surveys, Year <= termyr) 
  nyears <- max(dat.use$Year) - min(dat.use$Year) + 1 
  
  # get average slope from surveys
  nSurveys <- length(surveys[1,]) - 1
  sdat <- filter(dat.use, Year >= (termyr - npoint + 1)) 
  sdat1 <- filter(dat.use, Year >= (termyr - npoint), Year <= (termyr - 1))
  slopeterm <- rep(NA, nSurveys)  # slope for terminal year period for each survey
  slopeterm1 <- rep(NA, nSurveys) # slope for period prior to terminal year for each survey
  for (i in 1:nSurveys){
    mylm <- lm(log(sdat[,(i+1)]) ~ sdat[,1])
    mylm1 <- lm(log(sdat1[,(i+1)]) ~ sdat1[,1])
    slopeterm[i] <- as.numeric(coefficients(mylm)[2])
    slopeterm1[i] <- as.numeric(coefficients(mylm1)[2])
  }
  avgslope <- mean(slopeterm, na.rm=TRUE)
  deltaslope <- avgslope - mean(slopeterm1, na.rm=TRUE)
  
  # compute multiplier
  multiplier <- exp(Kp * avgslope + Kd * deltaslope)
  
  # make results list
  res <- list()
  res$avgslope <- avgslope
  res$deltaslope <- deltaslope
  res$multiplier <- multiplier
  
  return(res)
}
############################################################################################
