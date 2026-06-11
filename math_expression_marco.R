
### ein neues Blatt, a new sheet
plot.new(); plot.window(c(0,4), c(15,1))
https://www.bundesbank.de/resource/blob/989080/a5c2143d2bc6bfb6fd7e3e8c4fd79a8d/472B63F073F071307366337C94F8C870/2026-02-09-dkp-04-data.pdf
### Praeferenz, preference
### Erwartungswert, expectation value 
text(1  , 2  , expression(E * group("[", bolditalic(X), "]" )))
### die Budgetbeschraenkung, the budget constraint 
text(1  , 4  , expression(C_t+ B_t/P_t == W_t/P_t*L_t ))
text(1  , 6  , expression((B_t-1/P_t)*(1 + i_t-1) - T_t))

text(1, 1, "universal", adj = 0); text(2.5, 1,  "\\042")
text(3,1, "Makro",adj =0); text(2.5, 2, "Makro Modell")





plot(1:10, 1:10, main = quote(1 <= {1 < 2}))
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)",
     cex = .8)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))
text(4, 6.4, "expression(bar(x) == sum(frac(x[i], n), i==1, n))",
     cex = .8)
text(8, 5, expression(paste(frac(1, sigma*sqrt(2*pi)), " ",
                            plain(e)^{frac(-(x-mu)^2, 2*sigma^2)})),
     cex = 1.2)


           integral(Q_t/P_t ~~ dj, -infinity, infinity)))