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
          