source("4_run_PlanBsmooth_FSD.r")

# 5_summarize_results.r
# make tables and plots of multipliers and catch advice

# remember to set working directory to source file location

# compute catch advice two ways (old and new rec catch data)
catch.advice.df <- resdf %>%
  mutate(catch.old = Multiplier * mean.recent.catch.old,
         catch.new = Multiplier * mean.recent.catch.new)

# output the multipliers by estimator for the 28 Source X Case combinations
restable <-  resdf %>%
  select(Source, Case, Estimator, Multiplier) %>%
  spread(key = Estimator, value = Multiplier)

write.csv(restable, file = "..\\output\\results_table.csv", row.names = FALSE)

# plot the multipliers by Source and Case
multipliers.plot <- ggplot(resdf, aes(x=Case, y=Multiplier, color=Estimator)) +
  geom_point() +
  facet_wrap(~Source) +
  theme_bw()

print(multipliers.plot)
ggsave("..\\output\\multipliers_plot.png", multipliers.plot)

multipliers.plot2 <- ggplot(resdf, aes(x=Case, y=Multiplier, color=Estimator, shape=Source)) +
  geom_jitter(width = 0.2, height = 0) +
  scale_x_continuous(breaks = seq(1,7)) +
  theme_bw()

print(multipliers.plot2)
ggsave("..\\output\\multipliers_plot2.png", multipliers.plot2)

# plot distribution of catch advice
ca.long <- catch.advice.df %>%
  gather(key = "Basis", value = "Advice", catch.old:catch.new)

catch.advice.dist.plot <- ggplot(ca.long, aes(x=Advice, fill=Basis)) +
  geom_histogram(binwidth = 500, position = "identity", alpha = 0.5) +
  xlab("Catch Advice for 2019 (mt)") +
  scale_y_continuous(breaks = seq(0,12,2)) +
  scale_fill_viridis(discrete=TRUE) +
  theme_bw()

print(catch.advice.dist.plot)
ggsave("..\\output\\catch_advice_distribution_plot.png", catch.advice.dist.plot)

# plot Bretro for PlanBsmooth by Case and Source
Bretro.plot <- ggplot(resdf, aes(x=Case, y=Bretro, color=Estimator, shape=Source)) +
  geom_jitter(width = 0.2, height = 0, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(1,7)) +
  theme_bw()

print(Bretro.plot)
ggsave("..\\output\\Bretro_plot.png", Bretro.plot)

# plot deltamult2017 for PlanBsmooth by Case and Source
deltamult2017.plot <- ggplot(resdf, aes(x=Case, y=deltamult2017, color=Estimator, shape=Source)) +
  geom_jitter(width = 0.2, height = 0, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(1,7)) +
  theme_bw()

print(deltamult2017.plot)
ggsave("..\\output\\deltamult2017_plot.png", deltamult2017.plot)

# plot deltamult2018 for PlanBsmooth by Case and Source
deltamult2018.plot <- ggplot(resdf, aes(x=Case, y=deltamult2018, color=Estimator, shape=Source)) +
  geom_jitter(width = 0.2, height = 0, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(1,7)) +
  theme_bw()

print(deltamult2018.plot)
ggsave("..\\output\\deltamult2018_plot.png", deltamult2018.plot)


