source("0_functions.r")

# 1_plot_data.r
# make some simple plots and comparisons across all available fluke indices

# remember to set working directory to source file location

# get the data
dat <- read.csv("..\\data\\fluke_indices.csv", header = TRUE)
dat

# create second data set with fall surveys shifted ahead one year (similar to GB cod)
dat.fall.shifted <- dat %>%
  mutate(NEFSC.Fall = lag(NEFSC.Fall),
         MADMF.Fall = lag(MADMF.Fall),
         NEAMAP.Fall = lag(NEAMAP.Fall))

# rescale by mean for years 2008-2016 (the only years all indices overlap)
dat0816 <- filter(dat, Year %in% seq(2008, 2016))
mean0816 <- apply(dat0816, 2, mean, na.rm = TRUE)
mean0816[1] <- 1
dat.rescaled <- dat
for (i in 1:length(dat[1,])){
  dat.rescaled[,i] <- dat[,i] / as.numeric(mean0816[i])
}
dat.rescaled

# do the same rescaling for fall shifted data
dat.fall.shifted.0816 <- filter(dat.fall.shifted, Year %in% seq(2008, 2016))
mean.fall.shifted.0816 <- apply(dat.fall.shifted.0816, 2, mean, na.rm = TRUE)
mean.fall.shifted.0816[1] <- 1
dat.rescaled.fall.shifted <- dat.fall.shifted
for (i in 1:length(dat.fall.shifted[1,])){
  dat.rescaled.fall.shifted[,i] <- dat.fall.shifted[,i] / as.numeric(mean.fall.shifted.0816[i])
}
dat.rescaled.fall.shifted

# calculate average of surveys in each year for both cases
avg <- apply(dat.rescaled[,2:length(dat.rescaled[1,])], 1, mean, na.rm=TRUE)
avg.fall.shifted <- apply(dat.rescaled.fall.shifted[,2:length(dat.rescaled.fall.shifted[1,])], 1, 
                          mean, na.rm=TRUE)
avg.df <- data.frame(Year = dat.rescaled[,1],
                     avg = avg,
                     avg.fall.shifted = avg.fall.shifted) %>%
  drop_na()

# convert to long form
df.orig <- dat %>%
  gather(key = "Index", value = "Value", 2:length(dat[1,]), na.rm = TRUE) %>%
  mutate(Source = "Original")

df.rs <- dat.rescaled %>%
  gather(key = "Index", value = "Value", 2:length(dat[1,]), na.rm = TRUE) %>%
  mutate(Source = "Original Rescaled")

df.fs <- dat.fall.shifted %>%
  gather(key = "Index", value = "Value", 2:length(dat[1,]), na.rm = TRUE) %>%
  mutate(Source = "Fall Shifted")

df.rs.fs <- dat.rescaled.fall.shifted %>%
  gather(key = "Index", value = "Value", 2:length(dat[1,]), na.rm = TRUE) %>%
  mutate(Source = "Fall Shifted Rescaled")

df <- rbind(df.orig, df.rs, df.fs, df.rs.fs)

# plot standardized values
mySource <- unique(df$Source)
nSource <- length(mySource)
for (i in 1:nSource){
  plot1 <- ggplot(filter(df, Source == mySource[i]), aes(x=Year, y=Value, color=Index)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = c(2007.5, 2016.5), col="black", linetype = "dashed") +
    scale_x_continuous(breaks = seq(1960, 2020, 10)) +
    scale_color_viridis(discrete=TRUE) +
    ylab("Index") +
    ggtitle(mySource[i]) +
    theme_bw()
  
#  print(plot1)
  ggsave(paste0("..\\output\\plot1_indices_",i,".png"), plot1)
  
  if (i %in% c(1,3)){
    plot1a <- plot1 +
      geom_point(data=avg.df, aes(x=Year, y=avg), size=2, color="red")
  }
  if (i %in% c(2,4)){
    plot1a <- plot1 +
      geom_point(data=avg.df, aes(x=Year, y=avg.fall.shifted), size=2, color="red")
  } 

#  print(plot1a)
  ggsave(paste0("..\\output\\plot1_indices_with_avg_",i,".png"), plot1a)

  plot1b <- plot1 +
    facet_wrap(~Index) +
    scale_x_continuous(breaks = seq(1960, 2020, 20))
  
#  print(plot2)
  ggsave(paste0("..\\output\\plot1_indices_faceted_",i,".png"), plot1b)
}





  
