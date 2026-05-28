trials<- 10
n<- 30

out<- NULL
my_seq <- seq(-4, 4, by = 0.001)
color <- c(2)
for (i in seq(trials)) {
    out[i] <- mean(rnorm(n))
  }

out
[1]  0.1391736247 -0.1485318953  0.0002835847 -0.1238599073  0.0608310112
[6]  0.0485930666 -0.1232333319  0.0250609651  0.2438776278 -0.0104850837

mean(out)
[1] 0.01117097
sd(out)
[1] 0.1233725

 h <- hist(out,
    breaks = "Scott",
    plot = FALSE
  )
  plot(h,
    freq = FALSE,
    xlim = c(-2, 2),
    main = paste("Empirical Probabilities for sample size n=", n),
    cex.main = 0.75
  )
color<- c(2)
  curve(dnorm(x,
    mean = 0, sd = 1 / sqrt(n)
  ),
  from = -4, to = 4, n = 30,
  type = "l", # set line type
  lwd = 2, # set line width
  add = TRUE,
  col = color
  ) 
  legend(
    x = 0.8, # set x position
    y = max(h$density) * 0.7, # set y position
    paste("n = ", n), # set appropriate legend names
    lty = 1, # set line type
    lwd = 2, # set line width
    col = color, # set line color
    cex = 0.7 # set font size
  )
}


