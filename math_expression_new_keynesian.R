help("plotmath")
### ein neues Blatt, a new sheet
plot.new(); plot.window(c(0,4), c(15,1))
### The Taylor rule                                                         ###
text(1, 1, "Taylor rule")
text(1, 2,expression(i[t]== Pi[t]+a[Pi]*(Pi[t]-Pi[t]^(star))+b[y]*
                                             (y[t]-y[t]^(star))))
### The New Keynesian Phillips curve                                        ###
text(1, 4, "New Keynesian Phillips curve")
text(1, 5,expression(Pi[t]==Beta*E * group("[", bolditalic(Pi[t+1]), "]" )+kappa*y[t]))
text(3, 5, expression(where: kappa == (h(1-(1-h)*beta)/(1-h))*gamma))

### The dynamich IS curve

text(1, 8,expression(y[t]== E[t]*y[t+1] - 1/sigma*(i[t]-E[t]*Pi[t+1])+ v[t]))