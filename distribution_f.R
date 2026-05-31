https://search.r-project.org/R/refmans/stats/html/Fdist.html

Usage
df(x, df1, df2, ncp, log = FALSE)
pf(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
qf(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE)
rf(n, df1, df2, ncp)

### F value and P-value                                                      ###


(p.174)
pf(0.838, 27, 442)
1-pf(0.838, 27, 442)
[1] 0.7018644
(p.177)
pf(2.082, 84, 385)
1 - pf(2.082, 84, 385)
[1] 1.549551e-06


(p.317)
pf(19.93, 4, 424+50-8)
1 - pf(19.93, 4, 424+50-8)
[1] 3.663736e-15
F(k,n1+n2-2k)

(p.318)
pf(2.67, 50, 420)
1- pf(2.67, 50, 420)
[1] 5.360112e-08

pf(0.261, 1, 238)
1 - pf(0.261, 1, 238)
[1] 0.6099075
