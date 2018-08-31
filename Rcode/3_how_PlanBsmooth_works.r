source("2_get_catches.r")

# 3_how_PlanBsmooth_works.r
# some simple demonstrations of features of PlanBsmooth using fluke data

# remember to set working directory to source file location

# run PlanBsmooth on NEFSC spring data
NEFSC.spring.dat <- dat %>%
  select(Year, NEFSC.Spring) %>%
  drop_na() %>%
  rename(avg = NEFSC.Spring)
NEFSC.spring.dat

NEFSC.spring <- ApplyPlanBsmooth(dat = NEFSC.spring.dat,
                                 od = "..\\output\\",
                                 my.title = "NEFSC Spring",
                                 terminal.year = NA,
                                 nyears = 33,
                                 loess.span = NA,
                                 saveplots = TRUE,
                                 showplots = FALSE,
                                 nameplots = "demo_NEFSC_Spring_")
NEFSC.spring
NEFSC.spring$multiplier

# run retro on NEFSC spring data
NEFSC.spring.retro <- RunRetro(dat = NEFSC.spring.dat,
                               od = "..\\output\\",
                               my.title = "NEFSC Spring",
                               terminal.year = NA,
                               nyears = 33,
                               loess.span = NA,
                               npeels = 7,
                               saveretroplot = TRUE,
                               showplots = FALSE,
                               nameplots = "demo_NEFSC_Spring_")
NEFSC.spring.retro

# what if want to see the results using all the years in the time series?
NEFSC.spring.long <- ApplyPlanBsmooth(dat = NEFSC.spring.dat,
                                      od = "..\\output\\",
                                      my.title = "NEFSC Spring Long",
                                      terminal.year = NA,
                                      nyears = 52,
                                      loess.span = NA,
                                      saveplots = TRUE,
                                      showplots = FALSE,
                                      nameplots = "demo_NEFSC_Spring_Long_")
NEFSC.spring.long$multiplier

# if use wrong span, then get different result with long time series
NEFSC.spring.long.wrong <- ApplyPlanBsmooth(dat = NEFSC.spring.dat,
                                            od = "..\\output\\",
                                            my.title = "NEFSC Spring Long Wrong",
                                            terminal.year = NA,
                                            nyears = 52,
                                            loess.span = 0.30,
                                            saveplots = TRUE,
                                            showplots = FALSE,
                                            nameplots = "demo_NEFSC_Spring_Long_Wrong_")
NEFSC.spring.long.wrong$multiplier

# what if have only short time series
NEFSC.spring.short <- ApplyPlanBsmooth(dat = filter(NEFSC.spring.dat, Year >= 2000),
                                       od = "..\\output\\",
                                       my.title = "NEFSC Spring Short",
                                       terminal.year = NA,
                                       nyears = 33,
                                       loess.span = NA,
                                       saveplots = TRUE,
                                       showplots = FALSE,
                                       nameplots = "demo_NEFSC_Spring_Short_")
NEFSC.spring.short$multiplier

# again, if use wrong span, then get different result with short time series
NEFSC.spring.short.wrong <- ApplyPlanBsmooth(dat = filter(NEFSC.spring.dat, Year >= 2000),
                                             od = "..\\output\\",
                                             my.title = "NEFSC Spring Short Wrong",
                                             terminal.year = NA,
                                             nyears = 33,
                                             loess.span = 0.30,
                                             saveplots = TRUE,
                                             showplots = FALSE,
                                             nameplots = "demo_NEFSC_Spring_Short_Wrong_")
NEFSC.spring.short.wrong$multiplier

# make table of NEFSC spring multipliers
demo.table.multipliers <- rbind(c("NEFSC.spring", NEFSC.spring$multiplier),
                                c("NEFSC.spring.long", NEFSC.spring.long$multiplier),
                                c("NEFSC.spring.long.wrong", NEFSC.spring.long.wrong$multiplier),
                                c("NEFSC.spring.short", NEFSC.spring.short$multiplier),
                                c("NEFSC.spring.short.wrong", NEFSC.spring.short.wrong$multiplier))
write.csv(demo.table.multipliers, file="..\\output\\demo_table_multiplier.csv", row.names = FALSE)

