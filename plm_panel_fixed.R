### Source:Christiaan Heij, Paul de Boer, Philip Hans Franses, Teun Kloek, ###
### Herman K. van Dijk (2004).Econometric Methods with Applications in     ###
### Business and Economics. Oxford University Press                        ###
### https://global.oup.com/booksites/content/0199268010/                   ###
### Example 7 29  Primary metal industries ###
xm729<- read.csv("xm729.csv", header = TRUE)
attach(xm729)
str(xm729)

ID<- c(rep(1,37), rep(2,37), rep(3,37), rep(4,37), rep(5,37),
       rep(6,37), rep(7,37), rep(8,37), rep(9,37), rep(10,37),
       rep(11,37),rep(12,37),rep(13,37),rep(14,37),rep(15,37),
       rep(16,37),rep(17,37),rep(18,37),rep(19,37),rep(20,37),
       rep(21,37),rep(22,37),rep(23,37),rep(24,37),rep(25,37),
       rep(26,37))
ID<- as.integer(ID)
str(ID)

y<- rbind(LOGPROD_1, LOGPROD_2,LOGPROD_3,LOGPROD_4,LOGPROD_5,
          LOGPROD_6, LOGPROD_7,LOGPROD_8,LOGPROD_9,LOGPROD_10,
          LOGPROD_11,LOGPROD_12,LOGPROD_13,LOGPROD_14,LOGPROD_15,
          LOGPROD_16,LOGPROD_17,LOGPROD_18,LOGPROD_19,LOGPROD_20,
          LOGPROD_21,LOGPROD_22,LOGPROD_23,LOGPROD_24,LOGPROD_25,
          LOGPROD_26)
y<- as.numeric(y)
str(y)

x1<- rbind(LOGLAB_1, LOGLAB_2,LOGLAB_3,LOGLAB_4,LOGLAB_5,
          LOGLAB_6, LOGLAB_7,LOGLAB_8,LOGLAB_9,LOGLAB_10,
          LOGLAB_11,LOGLAB_12,LOGLAB_13,LOGLAB_14,LOGLAB_15,
          LOGLAB_16,LOGLAB_17,LOGLAB_18,LOGLAB_19,LOGLAB_20,
          LOGLAB_21,LOGLAB_22,LOGLAB_23,LOGLAB_24,LOGLAB_25,
          LOGLAB_26)
x1<- as.numeric(x1)
str(x1)

x2<- rbind(LOGCAP_1, LOGCAP_2,LOGCAP_3,LOGCAP_4,LOGCAP_5,
          LOGCAP_6, LOGCAP_7,LOGCAP_8,LOGCAP_9,LOGCAP_10,
          LOGCAP_11,LOGCAP_12,LOGCAP_13,LOGCAP_14,LOGCAP_15,
          LOGCAP_16,LOGCAP_17,LOGCAP_18,LOGCAP_19,LOGCAP_20,
          LOGCAP_21,LOGCAP_22,LOGCAP_23,LOGCAP_24,LOGCAP_25,
          LOGCAP_26)
x2<- as.numeric(x2)
str(x2)
const<- rep(1,962)       
p_data<- data.frame(y,x1,x2,const, ID)
p_data<- pdata.frame(p_data, index ="ID")
w1 <- plm(y ~ x1 + x2,
          data = p_data, model = "within", effect = "twoways")
summary(w1)
fixef(w1, type = "dmean")
   1          2          3          4          5          6          7 
-0.0968665 -0.0711004 -0.1385356 -0.0680962 -0.0464899  0.0200483  0.0101323 
         8          9         10         11         12         13         14 
 0.0049056 -0.0270432 -0.1198352 -0.0020711  0.1173438  0.0444627  0.0605440 
        15         16         17         18         19         20         21 
 0.0323359  0.1257796 -0.0794206 -0.2006385 -0.0616342 -0.1845308  0.0251244 
        22         23         24         25         26 
 0.1991426  0.1412854  0.0630665  0.0514385  0.2006525 
summary(fixef(w1, type = "dmean"))
w1a <- plm(y ~ x1 + x2,
          data = p_data, model="within", effect="twoways")
fixef(w1, effect = "time")
        1         2         3         4         5         6         7         8 
0.0576200 0.1570055 0.0907879 0.0906551 0.0876245 0.1229756 0.1135150 0.0323085 
        9        10        11        12        13        14        15        16 
0.0317052 0.0255569 0.1166953 0.0349479 0.0120585 0.0458207 0.0825160 0.0515052 
       17        18        19        20        21        22        23        24 
0.0082198 0.1013312 0.1200264 0.0537757 0.0759650 0.1106077 0.0675382 0.0973387 
       25        26        27        28        29        30        31        32 
0.0896300 0.1519655 0.0868522 0.1200996 0.0869674 0.1253925 0.1163397 0.1244531 
       33        34        35        36        37 
0.1274016 0.0682101 0.0586205 0.1074465 0.1510440 


w2 <- plm(y ~ x1 + x2 + factor(id)-1,
          data = p_data, model = "within")
summary(w2)
library(knitr)
library(broom)
kable(tidy(w2), digits=3, 
      caption="Fixed effects in a subsample")
Table: Fixed effects in a subsample

|term | estimate| std.error| statistic| p.value|
|:----|--------:|---------:|---------:|-------:|
|x1   |    0.784|     0.016|    47.529|       0|
|x2   |    0.161|     0.017|     9.748|       0|

w3 <- lm(y ~ x1 + x2 + factor(ID)-1)
summary(w3)
Call:
lm(formula = y ~ x1 + x2 + factor(ID) - 1)

Residuals:
     Min       1Q   Median       3Q      Max 
-3.04228 -0.15501 -0.00818  0.16320  1.12634 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
x1            0.78407    0.01650  47.529  < 2e-16 ***
x2            0.16092    0.01651   9.748  < 2e-16 ***
factor(ID)1   0.08911    0.10686   0.834  0.40457    
factor(ID)2   0.11487    0.10737   1.070  0.28495    
factor(ID)3   0.04748    0.10809   0.439  0.66055    
factor(ID)4   0.11788    0.10717   1.100  0.27164    
factor(ID)5   0.13954    0.10682   1.306  0.19176    
factor(ID)6   0.20609    0.10631   1.938  0.05286 .  
factor(ID)7   0.19623    0.10783   1.820  0.06909 .  
factor(ID)8   0.19110    0.10921   1.750  0.08047 .  
factor(ID)9   0.15912    0.11120   1.431  0.15279    
factor(ID)10  0.06639    0.11155   0.595  0.55192    
factor(ID)11  0.18412    0.11165   1.649  0.09948 .  
factor(ID)12  0.30358    0.11127   2.728  0.00649 ** 
factor(ID)13  0.23070    0.11396   2.024  0.04321 *  
factor(ID)14  0.24680    0.11359   2.173  0.03004 *  
factor(ID)15  0.21865    0.11277   1.939  0.05282 .  
factor(ID)16  0.31206    0.11496   2.715  0.00676 ** 
factor(ID)17  0.10690    0.11521   0.928  0.35374    
factor(ID)18 -0.01435    0.11968  -0.120  0.90456    
factor(ID)19  0.12467    0.11782   1.058  0.29027    
factor(ID)20  0.00172    0.11972   0.014  0.98854    
factor(ID)21  0.21142    0.12036   1.757  0.07933 .  
factor(ID)22  0.38543    0.11852   3.252  0.00119 ** 
factor(ID)23  0.32753    0.12123   2.702  0.00702 ** 
factor(ID)24  0.24934    0.11882   2.098  0.03613 *  
factor(ID)25  0.23766    0.12034   1.975  0.04858 *  
factor(ID)26  0.38689    0.11839   3.268  0.00112 ** 
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
Residual standard error: 0.2761 on 934 degrees of freedom
Multiple R-squared:  0.9883,    Adjusted R-squared:  0.988 
F-statistic:  2823 on 28 and 934 DF,  p-value: < 2.2e-16

fixef(w2, type = "dmean")

id<- t(ID)%*%d
d<- as.matrix(diag(26))
str(d)
i<- rep(1,37)
D<- i*26
p_data<- pdata.frame(xm729)
z<- make.dummies(ID)



a<- matrix(rep(rep(1,37), 26))
D<- a*d
X<- cbind(X1,X2)
a<- a*t(D)
eq<- lm(Y~ X1+X2+a -1)
summary(eq)
res_Y<- resid(eq)

eq1<- lm(X1~d -1)
res_x1<- resid(eq1)

eq2<- lm(X2~d -1 )
res_x2<- resid(eq2)

eq_res<- lm(res_Y~ res_x1+res_x2 -1)
summary(eq_res)
ID<- ID_1+ID_2+ID_3+ID_4+ID_5+
ID_6+ID_7+ID_8+ID_9+ID_10+
ID_11+ID_12+ID_13+ID_14+ID_15+
ID_16+ID_17+ID_18+ID_19+ID_20+
ID_21+ID_22+ID_23+ID_24+ID_25+
ID_26 
str(ID)

IDc<- cbind(ID_1,ID_2,ID_3,ID_4,ID_5,
           ID_6,ID_7,ID_8,ID_9,ID_10,
           ID_11,ID_12,ID_13,ID_14,ID_15,
           ID_16,ID_17,ID_18,ID_19,ID_20,
           ID_21,ID_22,ID_23,ID_24,ID_25,
           ID_26) 
d<- IDc#*#t(D)
str(d)

eq2<- plm(LOGPROD~ LOGPROD_1+ LOGPROD_2+LOGPROD_3+LOGPROD_4+LOGPROD_5+
          LOGPROD_6+ LOGPROD_7+LOGPROD_8+LOGPROD_9+LOGPROD_10+
          LOGPROD_11+ LOGPROD_22+LOGPROD_13+LOGPROD_14+LOGPROD_15+
          LOGPROD_15+ LOGPROD_17+LOGPROD_18+LOGPROD_19+LOGPROD_20+
          LOGPROD_21+ LOGPROD_22+LOGPROD_23+LOGPROD_24+LOGPROD_25+
          LOGPROD_26, data=xm729)
summary(eq)

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


d1<- c(log(0),rep(log(1),36))
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

D<- cbind(d1,d2,d3,d4,d5,d6,d7,d8, d9, d10,
          d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,
          d21,d22,d23,d24,d25,d26)    


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

eq1<- lm(LOGPROD_1~LOGLAB_1+LOGCAP_1)
summary(eq1)
