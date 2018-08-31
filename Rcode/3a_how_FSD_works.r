source("3_how_PlanBsmooth_works.r")

# 3a_how_FSD_works.r
# create some example plots showing how Rago's FSD works

# remember to set working directory to source file location

# use defaults for gain of first and second derivatives
Kp <- 0.75
Kd <- 0.50

# use NEFSC spring data only for this FSD example to match with PlanBsmooth example
sdat <- filter(NEFSC.spring.dat, Year >= 2014)
sdat1 <- filter(NEFSC.spring.dat, Year >= 2013, Year <= 2017)
mylm <- lm(log(sdat[,2]) ~ sdat[,1])
mylm1 <- lm(log(sdat1[,2]) ~ sdat1[,1])
slopeterm <- as.numeric(coefficients(mylm)[2])
slopeterm1 <- as.numeric(coefficients(mylm1)[2])
avgslope <- slopeterm
deltaslope <- avgslope - slopeterm1
multiplier <- exp(Kp * avgslope + Kd * deltaslope)

# mutate data for plotting
fsd.dat <- NEFSC.spring.dat %>%
  mutate(logIndex = log(avg)) %>%
  filter(Year >= 2010)

# create the data frames of predicted values for plotting
predterms <- data.frame(Year = seq(2013, 2018),
                        logpred = c(NA, as.numeric(predict(mylm))),
                        logpred1 = c(as.numeric(predict(mylm1)), NA))

# make the plot
demo_fsd_plot <- ggplot(fsd.dat, aes(x=Year, y=logIndex)) +
  geom_point() +
  geom_line(data=predterms, aes(x=Year, y=logpred), color = "blue", na.rm = TRUE) +
  geom_line(data=predterms, aes(x=Year, y=logpred1), color = "red", na.rm = TRUE) +
  ggtitle(paste0("NEFSC Spring multiplier = ", round(multiplier, 3)), 
          subtitle = paste0("avgslope = ", round(avgslope, 3),"  deltaslope = ", round(deltaslope, 3))) +
  theme_bw()

#print(demo_fsd_plot)
ggsave("..\\output\\demo_fsd_plot.png", demo_fsd_plot)
