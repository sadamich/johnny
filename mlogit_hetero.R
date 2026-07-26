https://cran.r-project.org/web/packages/mlogit/vignettes/c4.relaxiid.html


### The heteroskedastic logit model
### the Gauss-Laguerre quadrature method

### The nested logit model (McFadden)


library(mlogit)
data(ModeCanada)
MC <- dfidx(ModeCanada, subset = noalt == 4)
ml.MC <- mlogit(choice ~ freq + cost + ivt + ovt | urban + income, MC, 
                reflevel = 'car', alt.subset = c("car", "train", "air"))
hl.MC <- mlogit(choice ~ freq + cost + ivt + ovt | urban + income, MC, 
                reflevel = 'car', alt.subset = c("car", "train", "air"),
                heterosc = TRUE, hessian = FALSE)
coef(summary(hl.MC))[11:12, ]
  Estimate Std. Error   z-value     Pr(>|z|)
sp.train 1.2371829  0.1104610 11.200182 0.000000e+00
sp.air   0.5403239  0.1118353  4.831425 1.355592e-06

### The tests for the heteroskedasticity
lrtest(hl.MC, ml.MC)
 ratio test
Model 1: choice ~ freq + cost + ivt + ovt | urban + income
Model 2: choice ~ freq + cost + ivt + ovt | urban + income
  #Df  LogLik Df  Chisq Pr(>Chisq)  
1  12 -1838.1                       
2  10 -1841.6 -2 6.8882    0.03193 *
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1

waldtest(hl.MC, heterosc = FALSE)
        Wald test
data:  homoscedasticity
chisq = 25.196, df = 2, p-value = 3.38e-06

lrtest(hl.MC) |> gaze()
## Chisq = 6.888, df: 2, pval = 0.032
waldtest(hl.MC) |> gaze()
## chisq = 25.196, df: 2, pval = 0.000
The Wald test can also be computed using the lht function from the 
car package:
car::lht(hl.MC, c('sp.air = 1', 'sp.train = 1')) |> gaze()
## Chisq = 25.196, df: 2, pval = 0.000

### Japanese FDI
data(JapaneseFDI)
str(JapaneseFDI)
A dataframe containing :
firm: the investment id,
country: the country,
region: the region (nuts1 nomenclature),
choice: a dummy indicating the chosen region ,
choice.c: the chosen country,
wage: wage rate in the region,
unemp: unemployment rate in the region,
elig: is the country eligible to european subsidies,
area: the area of the region,
scrate: social charge rate (country level),
ctaxrate: corporate tax rate (country level),
gdp: regional gdp,
harris: harris' market potential,
krugman: krugman's market potential,
domind: domestic industry count,
japind: japan industry count,
network: network count.

Classes ‘tbl_df’, ‘tbl’ and 'data.frame':   25764 obs. of  17 variables:
 $ firm    : Factor w/ 452 levels "3","4","5","7",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ country : chr  "BE" "BE" "BE" "BE" ...
 $ region  : chr  "BE0" "BE1" "BE2" "BE3" ...
 $ choice  : int  0 0 0 0 0 0 0 0 0 0 ...
 $ choice.c: chr  "FR" "FR" "FR" "FR" ...
 $ wage    : num  14.2 16.6 14.9 16.8 17.5 ...
 $ unemp   : num  0.103 0.095 0.136 0.142 0.081 ...
 $ elig    : num  0 0 0 0 0 0 0 0 0 0 ...
 $ area    : num  335.8 1351.2 1684.4 16.1 1573.1 ...
 $ scrate  : num  0.598 0.598 0.598 0.598 0.267 ...
 $ ctaxrate: num  0.45 0.45 0.45 0.45 0.63 ...
 $ gdp     : num  28593 66857 31037 18237 31774 ...
 $ harris  : num  750 692 646 1027 393 ...
 $ krugman : num  12135 8972 5500 45989 3180 ...
 $ domind  : num  50 35 32 4 5 ...
 $ japind  : num  0 0 0 0 0 0 0 0 0 0 ...
 $ network : num  0 0 0 0 0 0 0 0 0 0 ...




jfdi <- dfidx(JapaneseFDI, idx = c("firm", country = "region"),
              drop.index = FALSE)



ml.fdi <- mlogit(choice ~ log(wage) + unemp + elig + log(area) +
                     scrate + ctaxrate | 0, data = jfdi)
lm.fdi <- mlogit(choice ~ log(wage) + unemp + elig + log(area) | 0,
                 data = jfdi,
                 subset = country == choice.c &
                     ! country %in% c("PT", "IE"))     

lmformula <- formula(lm.fdi)
head(logsum(ml.fdi, data = jfdi, formula = lmformula, type = "group"), 2)
        BE       DE       ES       FR       IE       IT       NL       PT
3 3.595818 5.415838 3.593702 5.153709 1.933707 5.051387 4.077845 2.702028
4 4.113243 5.765190 4.445012 5.383095 1.960462 5.687569 4.490379 3.200124
        UK
3 4.900622
4 5.378561

head(logsum(ml.fdi, data = jfdi, formula = lmformula, type = "global"))
head(logsum(ml.fdi, data = jfdi, formula = lmformula, output = "obs")) 

head(logsum(ml.fdi, data = jfdi, formula = lmformula, type = "global",
            output = "obs"))  

JapaneseFDI$iv <- logsum(lm.fdi, data = jfdi, formula = lmformula,
                         output = "obs")
JapaneseFDI.c <- subset(JapaneseFDI,
                        select = c("firm", "country", "choice.c",
                                   "scrate", "ctaxrate", "iv"))
JapaneseFDI.c <- unique(JapaneseFDI.c)
JapaneseFDI.c$choice.c <- with(JapaneseFDI.c, choice.c == country)

jfdi.c <- dfidx(JapaneseFDI.c, choice = "choice.c",
                idnames = c("chid", "alt"))
um.fdi <- mlogit(choice.c ~ scrate + ctaxrate + iv | 0, data = jfdi.c)

um2.fdi <- mlogit(choice.c ~ scrate + ctaxrate | 0 | iv, data = jfdi.c, 
                  constPar = c("iv:PT" = 1, "iv:IE" = 1))


nl.fdi <- mlogit(choice ~ log(wage) + unemp + elig + log(area) +
                     scrate + ctaxrate | 0, data = jfdi,
                 nests = TRUE, un.nest.el = TRUE)
nl2.fdi <- update(nl.fdi, un.nest.el = FALSE,
                  constPar = c('iv:PT' = 1, 'iv:IE' = 1))

lrtest(nl2.fdi) |> gaze()
## Chisq = 50.122, df: 9, pval = 0.000
waldtest(nl2.fdi) |> gaze()
## chisq = 208.407, df: 7, pval = 0.000
scoretest(ml.fdi, nests = TRUE,
          constPar = c('iv:PT' = 1, 'iv:IE' = 1))  |>
    gaze()
## chisq = 60.280, df: 7, pval = 0.000
The Wald test can also be performed using the lht function:

car::lht(nl2.fdi, c("iv:BE = 1", "iv:DE = 1", "iv:ES = 1", "iv:FR = 1",
                    "iv:IT = 1", "iv:NL = 1", "iv:UK = 1")) |> gaze()
## Chisq = 208.407, df: 7, pval = 0.000

lrtest(nl2.fdi, nl.fdi) |> gaze()
## Chisq = 46.954, df: 8, pval = 0.000
waldtest(nl2.fdi, un.nest.el = TRUE) |> gaze()
## chisq = 73.535, df: 6, pval = 0.000
scoretest(ml.fdi, nests = TRUE, un.nest.el = FALSE,
          constPar = c('iv:PT' = 1, 'iv:IE' = 1)) |> gaze()
## chisq = 60.280, df: 7, pval = 0.000
car::lht(nl2.fdi, c("iv:BE = iv:DE", "iv:BE = iv:ES",
                    "iv:BE = iv:FR", "iv:BE = iv:IT",
                    "iv:BE = iv:NL", "iv:BE = iv:UK")) |>
    gaze()
## Chisq = 73.535, df: 6, pval = 0.000