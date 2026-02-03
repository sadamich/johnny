### Source: https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Sampling-Error/index.html ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ### 
### Example 1 12, p.62                                                     ### 
xm101<- read.csv("xm101.csv", header =TRUE)
attach(xm101)
str(xm101)
hist(FGPA)
pop <- FGPA # initialize population
n <- 30 # sample size

# set plotting parameters
par(mfrow = c(3, 2), mar = c(2, 2, 2, 3), xpd = FALSE)

# start experiment
no_samples <- c(1, 10, 100, 500, 1000, 3000) # set number of samples to be drawn

# run experiment 6 times
for (i in 1:length(no_samples)) {
  # draw either 1, 10, 100, 500, 1000 or 2000 random samples of sample size n=30
  my_samples <- rep(NA, no_samples[i]) # initialize empty vector of size i
  for (j in 1:no_samples[i]) {
    # take random samples j times and calculate the sample mean
    my_samples[j] <- mean(sample(pop, n))
  }
  # plot result (NOTE: the stripchart() function does not scale well.
  # If you want to experiment with the code you should plot histograms instead)
  stripchart(my_samples,
    method = "stack",
    offset = 0.4,
    at = .01,
    pch = 19,
    col = "red",
    xlim = c(2, 3.5)
  )

  abline(v = mean(pop), lty = 2)
  text(
    x = mean(pop) * 1.25,
    y = 1.8,
    labels = paste(no_samples[i], " random\nsamples"),
    col = "red"
  )
  text(x = mean(pop) * 0.98, y = 1.8, labels = expression(mu))
}
# add title
mtext(expression(paste(
  "Relative frequency distribution (occurrences) of ",
  bar(x)
)), outer = TRUE, cex = 1, line = -1.5)
