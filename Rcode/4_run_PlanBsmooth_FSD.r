source("3a_how_FSD_works.r")

# 4_run_PlanBsmooth_FSD.r
# loop through a bunch of combinations of data across the four Sources (orig, Rescaled X Orig, Shifted)

# remember to set working directory to source file location

# set up the combinations of indices to be used
myCombo <- list()
myCombo[[1]] <- c("NEFSC")
myCombo[[2]] <- c("MADMF")
myCombo[[3]] <- c("NEAMA")  # note only using first 5 characters to distinguish indices
myCombo[[4]] <- c("NEFSC", "MADMF")
myCombo[[5]] <- c("NEFSC", "NEAMA")
myCombo[[6]] <- c("MADMF", "NEAMA")
myCombo[[7]] <- c("NEFSC", "MADMF", "NEAMA")
ncombos <- length(myCombo)

# create the data frame for results
resdf <- data.frame(Source = character(),
                    Case = integer(),
                    Estimator = character(),
                    Multiplier = double(),
                    Bretro = double(),
                    deltamult2017 = double(),
                    deltamult2018 = double())

# loop through Source, Case (aka Combo), and Estimator (PlanBsmooth vs FSD)
for (i in 1:nSource){
  for (j in 1:ncombos){
    thisdat <- df %>%
      filter(Source == mySource[i], substr(Index, 1, 5) %in% myCombo[[j]]) %>%
      select(-Source) 
    
    thisdat.pbs <- thisdat %>%
      group_by(Year) %>%
      summarize(avg = mean(Value, na.rm=TRUE))
    
    thisdat.fsd <- thisdat %>%
      spread(key = Index, value = Value)
    
    # run the two estimators
    pbs <- ApplyPlanBsmooth(dat = thisdat.pbs,
                            terminal.year = 2018,
                            showplots = FALSE)
    
    fsd <- ApplyFSD(surveys = thisdat.fsd,
                    termyr = 2018)

    # run the PlanBsmooth retro (FSD does not have a retro because no model or smoothing)
    pbs.retro <- RunRetro(dat = thisdat.pbs,
                          terminal.year = 2018,
                          npeels = 5,
                          showplots = FALSE)
    
    pbs.deltamult2017 <- filter(pbs.retro$mult.ribbon, Year == 2017, peel == 0)$mult -
      filter(pbs.retro$mult.ribbon, Year == 2017, peel == 1)$mult

    pbs.deltamult2018 <- filter(pbs.retro$mult.ribbon, Year == 2018, peel == 0)$mult -
      filter(pbs.retro$mult.ribbon, Year == 2017, peel == 1)$mult

    # run one peel of fsd to get deltamult2018
    fsd2017 <- ApplyFSD(surveys = thisdat.fsd,
                        termyr = 2017)
    
    fsd.deltamult2018 <- fsd$multiplier - fsd2017$multiplier
    
    # create the data frame of output for this Source and combo
    thisdf <- data.frame(Source = rep(mySource[i], 2),
                         Case = rep(j, 2),
                         Estimator = c("PlanBsmooth", "FSD"),
                         Multiplier = c(pbs$multiplier, fsd$multiplier),
                         Bretro = c(pbs.retro$rho.B, NA), 
                         deltamult2017 = c(pbs.deltamult2017, NA),
                         deltamult2018 = c(pbs.deltamult2018, fsd.deltamult2018))
    resdf <- rbind(resdf, thisdf)
  }
}

