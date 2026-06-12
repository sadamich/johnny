help("plotmath")
### ein neues Blatt, a new sheet
plot.new(); plot.window(c(0,4), c(15,1))
https://www.bundesbank.de/resource/blob/989080/a5c2143d2bc6bfb6fd7e3e8c4fd79a8d/472B63F073F071307366337C94F8C870/2026-02-09-dkp-04-data.pdf
### Praeferenz, preference  (1)                                            ###
text(1, 1, expression(E*sum(beta^t[t], t==1,Inf)*(log(C) - L/(1+x))))
### Erwartungswert, expectation value 
text(1, 2 , expression(E * group("[", bolditalic(X), "]" )))
text(1, 4 , expression(Y==(integral(Y(j)*dj,0,1))^x))

### die Budgetbeschraenkung, the budget constraint 
text(1, 6,expression(C_t+ B_t/P_t == W_t/P_t*L_t+(B_t-1/P_t)*(1 + i_t-1)-T_t))

text(1,8,expression(Y[jt]==A[t]*Z[jt]*L[jt]))
text(1,9, expression(A[t]==a[t]*A[t-1]))
text(1,10, expression(Z[jt]==g[t]*Z[jt]))
text(1,11, expression(Z[jt]==Q[t]))
text(1,12, expression(Q[t]==q[t]*Q[t-1]))
text(1,13, expression(a[t]==a*exp(a[t])))
text(1,14, expression(q[t]==q*exp(q[t])))
text(1,15, expression(g[t]==g*exp(g[t])))

### Steady state, stationaerer Zustand (6)                                 ###
text(3, 1, expression((1-delta)*(g/q)^(theta-1)< 1))
text(3, 2, expression(Z[jt]==G[jt]*Q[t-s[jt]]))
text(3, 3, expression(G[jt]==g[t]*G[jt-1]))

text(3, 7, expression(P[t]==(integral(Y[jt]^(1-delta)*dj,0,1))^(1/(1-theta))))

### Sum and Integral                                                       ###
text(1, 1, expression(sum(x[i], i==1, n)))
text(1, 3, expression(integral(f(x)*dx, a, b)))
text(1, 5, expression(integral(f(x)*dx, -Inf, Inf)))
text(1, 7, expression(integral((Q/P)*dj, 0, Inf)))
text(4, 9, expression(sum(x[j], j==1,n)))
### Gradient
text(2, 10, expression(nabla))



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