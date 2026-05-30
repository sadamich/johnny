https://www.geo.fu-berlin.de/en/v/soga-r/Machine-Learning/Application-Example-Geppert/index.html

Random Forest Regression Analysis 
url <- "https://doi.pangaea.de/10.1594/PANGAEA.944811?format=textfile"

data <- read.table(url, skip = 268, header = TRUE, sep = "\t", fileEncoding = "UTF-8", check.names = FALSE)
head(data,5)
install.packages("party")
library(party)

load(url("https://userpage.fu-berlin.de/soga/data/r-data/IsoW.data_untransf.fact_lo_TOPO_all2021comb_means.RData"))

# change names
names(TrajIsoLC.wmean)[names(TrajIsoLC.wmean) == 'mw18O'] <- 'O18'
names(TrajIsoLC.wmean)[names(TrajIsoLC.wmean) == 'mwdD'] <- 'H2'
names(TrajIsoLC.wmean)[names(TrajIsoLC.wmean) == 'Monat'] <- 'month'

# exclude variables
TrajIsoLC.wmean <- TrajIsoLC.wmean[,-which(colnames(TrajIsoLC.wmean)=="H2")]
TrajIsoLC.wmean <- TrajIsoLC.wmean[,-which(colnames(TrajIsoLC.wmean)=="d.Excess")]
TrajIsoLC.wmean <- TrajIsoLC.wmean[,-which(colnames(TrajIsoLC.wmean)=="Land.Ocean")]
TrajIsoLC.wmean <- TrajIsoLC.wmean[,-which(colnames(TrajIsoLC.wmean)=="Africa")]
TrajIsoLC.wmean <- TrajIsoLC.wmean[,-which(colnames(TrajIsoLC.wmean)=="Oceans")]
TrajIsoLC.wmean <- TrajIsoLC.wmean[,-which(colnames(TrajIsoLC.wmean)=="ISO")]

### Create a subset
IsoW <- TrajIsoLC.wmean
IsoW.06 <- subset(IsoW, IsoW$expl.frac > 0.6)

### Initial Model

# for reproduciblity
my.seed <- 196
set.seed(my.seed)

# initial RF model
library(randomForest)

model1 <- randomForest(
  formula = O18 ~ .,
  ntree = 2000,
  data = IsoW.06
)

model1

plot(model1)


### Hyper-parameter Tuning

set.seed(my.seed)
ntree <- 500

# hyper parameter grid
hyper_grid <- expand.grid(
  mtry = seq(1, 54, by = 2), # mtry-max has to be smaller than max number of variables!
  ntree = seq(ntree, ntree, by = ntree / 10),
  OOB_RMSE = 0
)
# run models
library(party)
library(caret)

set.seed(my.seed)
  for(i in 1:nrow(hyper_grid)) {
    model <- party::cforest(formula = O18 ~ .,
                            data    = IsoW.06,
                            controls = cforest_unbiased(ntree = hyper_grid$ntree[i],
                                                        mtry = hyper_grid$mtry[i]))
    error <- caret:::cforestStats(model)
  # add OOB error to grid
    hyper_grid$OOB_RMSE[i] <- error[1]
  }
library(magrittr)
# top 10 performing models
  hyper_grid %>%
    dplyr::arrange(OOB_RMSE) %>%
    head(10)



### The final Model

IsoW06.O18.seed <- my.seed
IsoW06.O18.seed


my.ntree <- 500
set.seed(IsoW06.O18.seed)
IsoW06.O18.mtry <- hyper_grid$mtry[hyper_grid$OOB_RMSE == min(hyper_grid$OOB_RMSE)]

# default RF model
cf <- party::cforest(formula = O18 ~ .,
                     data    = IsoW.06,
                     controls = cforest_unbiased(ntree = my.ntree, 
                     mtry = IsoW06.O18.mtry))

#error <- caret:::cforestStats(cf)
stats <- caret:::cforestStats(cf)

library(tibble)
IsoW06.O18.var.imp.all <- party::varimp(cf, conditional = TRUE) %>% 
  sort(decreasing=TRUE) %>% 
  enframe() %>%
  dplyr::top_n(54)

IsoW06.O18.var.imp.10 <- IsoW06.O18.var.imp.all %>% 
  dplyr::top_n(10)


library(cowplot)

IsoW06.O18 <-
  ggplot(IsoW06.O18.var.imp.10, aes(value, reorder(name, value))) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.00001), add = c(0, 0.02))) +
  labs(
    title = "δ¹⁸O",
    subtitle = paste("ntree = ", ntree, ", mtry =", IsoW06.O18.mtry, "; \nRMSE = ", round(error[1], digits = 2), "$R² = ", round(error[2], digits = 2))
  ) +
  xlab("Importance score") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 9),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 11),
    plot.subtitle = element_text(size = 8),
    axis.text = element_text(size = 7),
    text = element_text(size = 8),
    rect = element_rect(fill = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA, color = "black"),
    panel.ontop = TRUE,
    legend.background = element_rect(fill = "transparent"),
    legend.position = "right",
    legend.title = element_text(angle = 90, size = 8, face = "bold"),
    legend.title.align = 0,
    legend.key.size = unit(1, "mm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.key = element_rect(fill = "transparent"),
    legend.key.height = unit(0.4, "cm"),
    legend.text = element_text(margin = ggplot2::margin(r = 5, unit = "mm"), size = 7)
  )

# Specify fundamental plot dimensions
width.inch <- 9 / 2.54 # cm / 1 inch
height.inch <- 10 / 2.54
pointsize <- 8
x11(
  width = width.inch,
  height = height.inch,
  pointsize = pointsize
)

ggdraw(IsoW06.O18)




