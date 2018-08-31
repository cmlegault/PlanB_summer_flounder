source ("5_summarize_results.r")

# 6_survey_expansion.r
# apply survey expansion approach to fluke data

# remember to set working directory to source file location

BB <- read.csv("..\\data\\Bigelow_biomass_estimates.csv", header = TRUE)
BB

# add the spring 2017 and 2018 data based on ratio of kg/tow in these years to 2016
added.years <- data.frame(Year = c(2017, 2018),
                          Fall = rep(NA, 2),
                          Spring = BB$Spring[BB$Year == 2016] * 
                            dat$NEFSC.Spring[dat$Year %in% c(2017, 2018)] / 
                            dat$NEFSC.Spring[dat$Year == 2016])
BBay <- rbind(BB, added.years)

# create data frame with shifted fall and compute two averages
BBdf <- BBay %>%
  mutate(Fall.shifted = lag(Fall)) %>%
  rowwise() %>%
  mutate(avg = mean(c(Fall, Spring), na.rm = TRUE)) %>%
  mutate(avg.shifted = mean(c(Fall.shifted, Spring), na.rm = TRUE))

# plot the data and two averages
survey_expansion_data_plot <- ggplot(BBdf, aes(x=Year)) +
  geom_point(aes(y=Fall), color="red") +
  geom_point(aes(y=Fall.shifted), color="red", shape=21, fill="white") +
  geom_point(aes(y=Spring), color="blue") +
  geom_line(aes(y=avg), color="black", linetype="solid") +
  geom_line(aes(y=avg.shifted), color="black", linetype="dashed") +
  ylab("Population Biomass (mt)") +
  expand_limits(y = 0) +
  theme_bw()

print(survey_expansion_data_plot)
ggsave("..\\output\\survey_expansion_data_plot.png", survey_expansion_data_plot)

# estimate exploitation rates
expl.rate.df <- merge(BBdf, catch.dat, by="Year")

expl.rate.df.avg <-  expl.rate.df %>%
  select(Year, avg, Old.Catch, New.Catch) %>%
  mutate(F.old = Old.Catch / avg,
         F.new = New.Catch / avg)

mean.F.avg <- c(mean(expl.rate.df.avg$F.old), mean(expl.rate.df.avg$F.new))

exp.rate.table.avg <- rbind(expl.rate.df.avg, c("mean", NA, NA, NA, mean.F.avg))
write.csv(exp.rate.table.avg, file = "..\\output\\survey_expansion_F_table_avg.csv", row.names = FALSE)

expl.rate.df.avg.shifted <-  expl.rate.df %>%
  select(Year, avg.shifted, Old.Catch, New.Catch) %>%
  mutate(F.old = Old.Catch / avg.shifted,
         F.new = New.Catch / avg.shifted)

mean.F.avg.shifted <- c(mean(expl.rate.df.avg.shifted$F.old), mean(expl.rate.df.avg.shifted$F.new))

exp.rate.table.avg.shifted <- rbind(expl.rate.df.avg.shifted, c("mean", NA, NA, NA, mean.F.avg.shifted))
write.csv(exp.rate.table.avg.shifted, file = "..\\output\\survey_expansion_F_table_avg_shifted.csv", row.names = FALSE)

# compute catch advice (note avg and avg.shifted are the same due to fall 2017 NA)
B2018 <- BBdf$avg[BBdf$Year == 2018]
survey.expansion.catch.advice <- B2018 * c(mean.F.avg, mean.F.avg.shifted)

survey_expansion_catch_advice_table <- rbind(c(NA, "Mean F.old Avg", "Mean F.new Avg", 
                                               "Mean F.old Avg Shifted", "Mean F.new Avg Shifted"),
                                             c("Biomass 2018", mean.F.avg, mean.F.avg.shifted),
                                             c(B2018, survey.expansion.catch.advice))
write.csv(survey_expansion_catch_advice_table, file = "..\\output\\survey_expansion_catch_advice.csv", row.names = FALSE)
