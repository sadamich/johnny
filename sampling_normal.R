https://www.geo.fu-berlin.de/en/v/soga-r/Basics-of-statistics/Central-Limit-Theorem/Sampling-a-Normally-Distributed-Population/index.html


trials <- 1000
n <- c(5, 15, 30, 50) # sample size

# empty matrix to store results of computations
out <- matrix(nrow = trials, ncol = length(n))

# plotting parameters
my_seq <- seq(-4, 4, by = 0.001)
color <- c(2, 3, 4, 5, 6)

# random sampling
for (i in seq(trials)) {
  for (j in seq(length(n))) {
    out[i, j] <- mean(rnorm(n[j]))
  }
}

# plotting
par(mfrow = c(2, 2), mar = c(3, 4, 2, 3))

for (i in seq(1, 4)) {
  h <- hist(out[, i],
    breaks = "Scott",
    plot = FALSE
  )
  plot(h,
    freq = FALSE,
    xlim = c(-2, 2),
    main = paste("Empirical Probabilities vs.\nSampling Distribution for sample size n=", n[i]),
    cex.main = 0.75
  )
  curve(dnorm(x,
    mean = 0, sd = 1 / sqrt(n[i])
  ),
  from = -4, to = 4, n = 1000,
  type = "l", # set line type
  lwd = 2, # set line width
  add = TRUE,
  col = color[i]
  ) # set line color
  legend(
    x = 0.8, # set x position
    y = max(h$density) * 0.7, # set y position
    paste("n = ", n[i]), # set appropriate legend names
    lty = 1, # set line type
    lwd = 2, # set line width
    col = color[i], # set line color
    cex = 0.7 # set font size
  )
}

