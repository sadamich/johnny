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