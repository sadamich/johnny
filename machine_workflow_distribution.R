https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Workflow/index.html

### Inspect data

### 1 Check distributions

library(ggplot2)

data %>%
  select(prec, sun_dur, snow_depth, vapor_pres, pres, rel_humid, temp) %>%
  gather(key = "feature", value = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 35, fill = "blue", alpha = 0.7) +
  facet_wrap(~feature, scales = "free") + # This change here makes both x and y axis free.
  theme_minimal() +
  labs(title = "Feature Distributions", x = NULL)

### Features 
describe_data <- data %>%
  select(prec, sun_dur, snow_depth, vapor_pres, pres, rel_humid, temp) %>%
  summary()
describe_data



data <- subset(data, select = -c(prec, sun_dur, snow_depth))
head(data)
