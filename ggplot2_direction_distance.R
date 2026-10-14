https://cran.r-project.org/web/packages/ggplot2/refman/ggplot2.html#geom_spoke
library(ggplot2)
df <- expand.grid(x = 1:10, y=1:10)

set.seed(1)
df$angle <- runif(100, 0, 2*pi)
df$speed <- runif(100, 0, sqrt(0.1 * df$x))

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_spoke(aes(angle = angle), radius = 0.5)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_spoke(aes(angle = angle, radius = speed))