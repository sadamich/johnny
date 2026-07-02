### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 29  Primary metal industries ###
xm729<- read.csv("xm729.csv", header = TRUE)
attach(xm729)
str(xm729)

LOGPROD<- LOGPROD_1+ LOGPROD_2+LOGPROD_3+LOGPROD_4+LOGPROD_5+
          LOGPROD_6+ LOGPROD_7+LOGPROD_8+LOGPROD_9+LOGPROD_10+
          LOGPROD_11+ LOGPROD_22+LOGPROD_13+LOGPROD_14+LOGPROD_15+
          LOGPROD_15+ LOGPROD_17+LOGPROD_18+LOGPROD_19+LOGPROD_20+
          LOGPROD_21+ LOGPROD_22+LOGPROD_23+LOGPROD_24+LOGPROD_25+
          LOGPROD_26

LOGLAB<-  LOGLAB_1+ LOGLAB_2+LOGLAB_3+LOGLAB_4+LOGLAB_5+
          LOGLAB_6+ LOGLAB_7+LOGLAB_8+LOGLAB_9+LOGLAB_10+
          LOGLAB_11+ LOGLAB_22+LOGLAB_13+LOGLAB_14+LOGLAB_15+
          LOGLAB_15+ LOGLAB_17+LOGLAB_18+LOGLAB_19+LOGLAB_20+
          LOGLAB_21+ LOGLAB_22+LOGLAB_23+LOGLAB_24+LOGLAB_25+
          LOGLAB_26
LOGCAP<-  LOGCAP_1+ LOGCAP_2+LOGCAP_3+LOGCAP_4+LOGCAP_5+
          LOGCAP_6+ LOGCAP_7+LOGCAP_8+LOGCAP_9+LOGCAP_10+
          LOGCAP_11+ LOGCAP_22+LOGCAP_13+LOGCAP_14+LOGCAP_15+
          LOGCAP_15+ LOGCAP_17+LOGCAP_18+LOGCAP_19+LOGCAP_20+
          LOGCAP_21+ LOGCAP_22+LOGCAP_23+LOGCAP_24+LOGCAP_25+
          LOGCAP_26
d1<- c(1,rep(0,36))
d2<- c(0,1, rep(0,35))
d3<- c(0,0,1,rep(0,34))
d4<- c(0,0,0,1,rep(0,33))
d5<- c(rep(0,4),1,rep(0,32))
d6<- c(rep(0,5),1,rep(0,31))
d7<- c(rep(0,6),1,rep(0,30))
d8<- c(rep(0,7),1,rep(0,29))
d9<- c(rep(0,8),1,rep(0,28))
d10<- c(rep(0,9,),1,rep(0,27))
d11<- c(rep(0,10),1,rep(0,26))
d12<- c(rep(0,11),1,rep(0,25))
d13<- c(rep(0,12),1,rep(0,24))
d14<- c(rep(0,13),1,rep(0,23))
d15<- c(rep(0,14),1,rep(0,22))
d16<- c(rep(0,15),1,rep(0,21))
d17<- c(rep(0,16),1,rep(0,20))
d18<- c(rep(0,17),1,rep(0,19))
d19<- c(rep(0,18),1,rep(0,18))
d20<- c(rep(0,19),1,rep(0,17))
d21<- c(rep(0,20),1,rep(0,16))
d22<- c(rep(0,21),1,rep(0,15))
d23<- c(rep(0,22),1,rep(0,14))
d24<- c(rep(0,23),1,rep(0,13))
d25<- c(rep(0,24),1,rep(0,12))
d26<- c(rep(0,25),1,rep(0,11))
panel01<- lm(LOGPROD ~d1+d2+d3+d4+d5+
                      d6+d7+d8+d9+d10+
                      d11+d12+d13+d14+d15+
                      d16+d17+d18+d19+d20+
                      d21+d22+d23+d24+d25+d26+LOGLAB+LOGCAP -1)
summary(panel01)

Call:
lm(formula = LOGPROD ~ d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + 
    d9 + d10 + d11 + d12 + d13 + d14 + d15 + d16 + d17 + d18 + 
    d19 + d20 + d21 + d22 + d23 + d24 + d25 + d26 + LOGLAB + 
    LOGCAP - 1)

Residuals:
   Min     1Q Median     3Q    Max 
-3.662  0.000  0.000  0.000  4.047 

Coefficients:
       Estimate Std. Error t value Pr(>|t|)    
d1     -5.40613    2.92539  -1.848 0.097660 .  
d2     -1.16564    2.93828  -0.397 0.700824    
d3     -2.01988    2.96614  -0.681 0.513021    
d4     -3.27295    2.88797  -1.133 0.286371    
d5     -0.39988    3.20130  -0.125 0.903338    
d6      1.61179    3.48970   0.462 0.655136    
d7      3.26668    3.97237   0.822 0.432124    
d8      5.90321    4.55527   1.296 0.227253    
d9      8.67536    5.27355   1.645 0.134365    
d10     7.28975    5.07742   1.436 0.184912    
d11     7.80267    5.18403   1.505 0.166549    
d12     8.62779    5.69466   1.515 0.164057    
d13     5.09754    4.77815   1.067 0.313824    
d14     1.54919    4.18779   0.370 0.719989    
d15     6.07917    5.12017   1.187 0.265501    
d16    10.23980    6.03674   1.696 0.124074    
d17    14.07875    6.17419   2.280 0.048542 *  
d18     6.44076    4.32932   1.488 0.171003    
d19     8.50300    5.03440   1.689 0.125489    
d20    10.02557    5.48945   1.826 0.101082    
d21    11.01999    6.27532   1.756 0.112957    
d22    15.19417    6.75726   2.249 0.051123 .  
d23    11.09025    5.71301   1.941 0.084143 .  
d24     5.65942    5.36762   1.054 0.319199    
d25    -4.57708    3.39712  -1.347 0.210809    
d26    -1.46377    3.04407  -0.481 0.642090    
LOGLAB -0.15776    0.46919  -0.336 0.744391    
LOGCAP  0.32300    0.06433   5.021 0.000718 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.741 on 9 degrees of freedom
Multiple R-squared:  0.9995,    Adjusted R-squared:  0.9979 
F-statistic: 638.6 on 28 and 9 DF,  p-value: 6.571e-12


