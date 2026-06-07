### R. Hatzinger, K. Hornik, H. Nagel, M.J.Maier (2014), R Einführung durch ###
### angewandte Statistik, Pearson                                           ###
### Quelle: https://www.pearson.de/r-9783868942507                          ##

kstand <- read.table("kstand.dat", header = TRUE)
attach(kstand)
WOCHENTAG[1:50]
 [1] "MO" "DO" "DO" "DO" "DI" "MO" "DI" "MO" "MO" "DI" "DO" "DI" "MO" "MO" "MO"
[16] "MI" "MO" "DI" "MO" "SO" "FR" "MI" "MI" "DI" "MO" "SO" "FR" "FR" "DO" "MI"
[31] "MI" "DO" "MO" "FR" "DI" "FR" "MO" "DO" "MI" "MO" "MO" "SO" "DO" "MO" "MI"
[46] "MO" "DI" "MO" "FR" "DI"

table(WOCHENTAG)
WOCHENTAG
DI DO FR MI MO SA SO 
60 45 30 51 96  9  9 

table(WOCHENTAG)/length(WOCHENTAG)
WOCHENTAG
  DI   DO   FR   MI   MO   SA   SO 
0.20 0.15 0.10 0.17 0.32 0.03 0.03 
prop.table(table(WOCHENTAG))
WOCHENTAG
  DI   DO   FR   MI   MO   SA   SO 
0.20 0.15 0.10 0.17 0.32 0.03 0.03 
100*prop.table(table(WOCHENTAG))

WOCHENTAG
DI DO FR MI MO SA SO 
20 15 10 17 32  3  3 
wtag <- factor(WOCHENTAG, levels = c("MO", "DI", "MI", "DO", "FR", "SA", "SO"))
detach(kstand)
absH <- table(wtag)
relH <- prop.table(table(wtag))
proz <- 100*relH
relH <- round(relH, digits = 2)
proz <- round(proz, digits = 0)
cbind(absH, relH, proz)
   absH relH proz
MO   96 0.32   32
DI   60 0.20   20
MI   51 0.17   17
DO   45 0.15   15
FR   30 0.10   10
SA    9 0.03    3
SO    9 0.03    3
