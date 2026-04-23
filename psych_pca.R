### Quelle:                                                                 ###
### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ###
### Kapitel 11 (S. 456-464)                                                 ###
library("psych")

smarkt <- read.table("smarkt.dat", header = TRUE)

smd <- na.omit(smarkt[, 6:25])
itemnam <- c("Nichtlebensmittel", "offene Kassen", "Expresskassen",
"Babyeinrichtungen", "Tankstelle", "Restaurant", "Stammkundenrabbatt",
"Parkplaetze", "Standort", "Kundenservice", "Sonderangebote", 
"Freundlichkeit", "Atmosphaere", "Einpackhilfe", "Schlangen Kassen",
"Preise", "Qual.Frischprod.", "Qual.verp.Prod.", "Qual.Wagen", "Zustellung")
colnames(smd)<- itemnam
install.packages("REdaS")
library("REdaS")
bart_spher(smd)

        Bartlett's Test of Sphericity

Call: bart_spher(x = smd)

     X2 = 3035.989
     df = 190
p-value < 2.22e-16

kmosmd<- KMOS(smd)
print(kmosmd, stats = "KMO")
Kaiser-Meyer-Olkin Statistic
Call: KMOS(x = smd)
KMO-Criterion: 0.8557595

print(kmosmd, stats = "MSA", sort = TRUE, digits = 3, show = 1:5)
Kaiser-Meyer-Olkin Statistics
Call: KMOS(x = smd)
Measures of Sampling Adequacy (MSA):
 Zustellung Parkplaetze  Tankstelle      Preise    Standort 
      0.682       0.694       0.744       0.794       0.811 

library("psych")
VSS.scree(smd)

https://www.geo.fu-berlin.de/en/v/soga-r/Advanced-statistics/Multivariate-approaches/Principal-Component-Analysis/index.html

pca.smd <- principal(smd, 5, rotate = "none")
pca.smd$criteria <- NULL
pca.smd
Principal Components Analysis
Call: principal(r = smd, nfactors = 5, rotate = "none")
Standardized loadings (pattern matrix) based upon correlation matrix
                    PC1   PC2   PC3   PC4   PC5   h2   u2 com
Nichtlebensmittel  0.48  0.50  0.10 -0.12 -0.10 0.52 0.48 2.3
offene Kassen      0.46  0.46  0.02 -0.15  0.34 0.56 0.44 3.1
Expresskassen      0.45  0.10  0.26 -0.41  0.28 0.52 0.48 3.5
Babyeinrichtungen  0.41  0.38  0.12  0.26 -0.01 0.40 0.60 2.9
Tankstelle         0.50  0.50 -0.51  0.02 -0.04 0.76 0.24 3.0
Restaurant         0.46  0.39  0.31  0.17 -0.27 0.56 0.44 3.8
Stammkundenrabbatt 0.57  0.19  0.10 -0.18 -0.26 0.47 0.53 2.0
Parkplaetze        0.42  0.34 -0.66  0.01  0.03 0.73 0.27 2.3
Standort           0.35 -0.18 -0.09 -0.45  0.44 0.56 0.44 3.3
Kundenservice      0.77 -0.13  0.11 -0.11  0.05 0.63 0.37 1.2
Sonderangebote     0.64 -0.08  0.18 -0.28 -0.36 0.66 0.34 2.3
Freundlichkeit     0.72 -0.36  0.00  0.01 -0.01 0.65 0.35 1.5
Atmosphaere        0.66 -0.34 -0.01  0.10  0.18 0.60 0.40 1.7
Einpackhilfe       0.50  0.08  0.25  0.30 -0.10 0.42 0.58 2.4
Schlangen Kassen   0.62 -0.17  0.04 -0.17 -0.06 0.45 0.55 1.4
Preise             0.43 -0.40  0.01 -0.19 -0.38 0.53 0.47 3.4
Qual.Frischprod.   0.54 -0.26 -0.33  0.31 -0.16 0.59 0.41 3.2
Qual.verp.Prod.    0.58 -0.31 -0.16  0.31  0.10 0.56 0.44 2.4
Qual.Wagen         0.59 -0.17 -0.12  0.28  0.29 0.55 0.45 2.3
Zustellung         0.23  0.10  0.55  0.39  0.32 0.61 0.39 3.0

                       PC1  PC2  PC3  PC4  PC5
SS loadings           5.69 1.87 1.46 1.21 1.09
Proportion Var        0.28 0.09 0.07 0.06 0.05
Cumulative Var        0.28 0.38 0.45 0.51 0.57
Proportion Explained  0.50 0.16 0.13 0.11 0.10
Cumulative Proportion 0.50 0.67 0.80 0.90 1.00

Mean item complexity =  2.5
Fit based u


pca.smdr <- principal(smd, 5)
pca.smdr$criteria <- NULL
print(pca.smdr, cut = 0.5, sort = TRUE, digits = 2)
Principal Components Analysis
Call: principal(r = smd, nfactors = 5)
Standardized loadings (pattern matrix) based upon correlation matrix
                   item   RC1   RC2   RC5   RC3   RC4   h2   u2 com
Qual.verp.Prod.      18  0.73                         0.56 0.44 1.1
Atmosphaere          13  0.69                         0.60 0.40 1.6
Qual.Wagen           19  0.68                         0.55 0.45 1.4
Qual.Frischprod.     17  0.64                         0.59 0.41 1.9
Freundlichkeit       12  0.63                         0.65 0.35 2.1
Kundenservice        10                               0.63 0.37 3.6
Restaurant            6        0.69                   0.56 0.44 1.4
Zustellung           20        0.58                   0.61 0.39 2.7
Babyeinrichtungen     4        0.58                   0.40 0.60 1.4
Einpackhilfe         14        0.52                   0.42 0.58 2.0
Nichtlebensmittel     1        0.52                   0.52 0.48 2.9
Sonderangebote       11              0.73             0.66 0.34 1.5
Preise               16              0.66             0.53 0.47 1.4
Stammkundenrabbatt    7              0.51             0.47 0.53 2.6
Schlangen Kassen     15                               0.45 0.55 2.8
Parkplaetze           8                    0.82       0.73 0.27 1.2
Tankstelle            5                    0.81       0.76 0.24 1.3
Expresskassen         3                          0.66 0.52 0.48 1.4
Standort              9                          0.66 0.56 0.44 1.6
offene Kassen         2                          0.54 0.56 0.44 2.6

                       RC1  RC2  RC5  RC3  RC4
SS loadings           3.07 2.32 2.27 1.93 1.72
Proportion Var        0.15 0.12 0.11 0.10 0.09
Cumulative Var        0.15 0.27 0.38 0.48 0.57
Proportion Explained  0.27 0.20 0.20 0.17 0.15
Cumulative Proportion 0.27 0.48 0.68 0.85 1.00

Mean item complexity =  1.9
Fit based

fa.diagram(pca.smdr, cut = 0.5, cex = 0.8, rsize= 0.5, main= "")

pca.smd2 <- principal(smd, 2)
pca.smd2$criteria <- NULL

print(pca.smd2, cut = 0.5, sort = TRUE, digits = 2)


Principal Components Analysis
Call: principal(r = smd, nfactors = 2)
Standardized loadings (pattern matrix) based upon correlation matrix
                   item  RC1   RC2    h2   u2 com
Freundlichkeit       12 0.79       0.649 0.35 1.1
Atmosphaere          13 0.73       0.555 0.44 1.1
Kundenservice        10 0.68       0.603 0.40 1.5
Qual.verp.Prod.      18 0.65       0.432 0.57 1.1
Schlangen Kassen     15 0.59       0.412 0.59 1.3
Preise               16 0.59       0.349 0.65 1.0
Qual.Frischprod.     17 0.58       0.354 0.65 1.1
Qual.Wagen           19 0.57       0.372 0.63 1.3
Sonderangebote       11 0.55       0.413 0.59 1.6
Standort              9            0.155 0.84 1.1
Tankstelle            5       0.70 0.500 0.50 1.0
Nichtlebensmittel     1       0.69 0.484 0.52 1.0
offene Kassen         2       0.64 0.423 0.58 1.0
Restaurant            6       0.59 0.362 0.64 1.1
Babyeinrichtungen     4       0.55 0.315 0.68 1.1
Parkplaetze           8       0.53 0.291 0.71 1.1
Stammkundenrabbatt    7            0.359 0.64 1.8
Einpackhilfe         14            0.254 0.75 2.0
Expresskassen         3            0.213 0.79 1.9
Zustellung           20            0.063 0.94 1.5

                       RC1  RC2
SS loadings           4.25 3.30
Proportion Var        0.21 0.17
Cumulative Var        0.21 0.38
Proportion Explained  0.56 0.44
Cumulative Proportion 0.56 1.00

Mean item complexity =  1.3
Fit based upon off diagonal values = 0.96> 