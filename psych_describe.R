https://cran.r-project.org/web/packages/psych/refman/psych.html#describeBy
library(psych)

data(sat.act)
describeBy(sat.act,sat.act$gender) #just one grouping variable
 Descriptive statistics by group 
group: 1
          vars   n   mean     sd median trimmed    mad min max range  skew
gender       1 247   1.00   0.00      1    1.00   0.00   1   1     0   NaN
education    2 247   3.00   1.54      3    3.12   1.48   0   5     5 -0.54
age          3 247  25.86   9.74     22   24.23   5.93  14  58    44  1.43
ACT          4 247  28.79   5.06     30   29.23   4.45   3  36    33 -1.06
SATV         5 247 615.11 114.16    630  622.07 118.61 200 800   600 -0.63
SATQ         6 245 635.87 116.02    660  645.53  94.89 300 800   500 -0.72
          kurtosis   se
gender         NaN 0.00
education    -0.60 0.10
age           1.43 0.62
ACT           1.89 0.32
SATV          0.13 7.26
SATQ         -0.12 7.41
------------------------------------------------------------ 
group: 2
          vars   n   mean     sd median trimmed    mad min max range  skew
gender       1 453   2.00   0.00      2    2.00   0.00   2   2     0   NaN
education    2 453   3.26   1.35      3    3.40   1.48   0   5     5 -0.74
age          3 453  25.45   9.37     22   23.70   5.93  13  65    52  1.77
ACT          4 453  28.42   4.69     29   28.63   4.45  15  36    21 -0.39
SATV         5 453 610.66 112.31    620  617.91 103.78 200 800   600 -0.65
SATQ         6 442 596.00 113.07    600  602.21 133.43 200 800   600 -0.58
          kurtosis   se
gender         NaN 0.00
education     0.27 0.06
age           3.03 0.44
ACT          -0.42 0.22
SATV          0.42 5.28
SATQ          0.13 5.38


describeBy(sat.act ~ gender)   #describe the entire set	formula input
 Descriptive statistics by group 
gender: 1
          vars   n   mean     sd median trimmed    mad min max range  skew
gender       1 247   1.00   0.00      1    1.00   0.00   1   1     0   NaN
education    2 247   3.00   1.54      3    3.12   1.48   0   5     5 -0.54
age          3 247  25.86   9.74     22   24.23   5.93  14  58    44  1.43
ACT          4 247  28.79   5.06     30   29.23   4.45   3  36    33 -1.06
SATV         5 247 615.11 114.16    630  622.07 118.61 200 800   600 -0.63
SATQ         6 245 635.87 116.02    660  645.53  94.89 300 800   500 -0.72
          kurtosis   se
gender         NaN 0.00
education    -0.60 0.10
age           1.43 0.62
ACT           1.89 0.32
SATV          0.13 7.26
SATQ         -0.12 7.41
------------------------------------------------------------ 
gender: 2
          vars   n   mean     sd median trimmed    mad min max range  skew
gender       1 453   2.00   0.00      2    2.00   0.00   2   2     0   NaN
education    2 453   3.26   1.35      3    3.40   1.48   0   5     5 -0.74
age          3 453  25.45   9.37     22   23.70   5.93  13  65    52  1.77
ACT          4 453  28.42   4.69     29   28.63   4.45  15  36    21 -0.39
SATV         5 453 610.66 112.31    620  617.91 103.78 200 800   600 -0.65
SATQ         6 442 596.00 113.07    600  602.21 133.43 200 800   600 -0.58
          kurtosis   se
gender         NaN 0.00
education     0.27 0.06
age           3.03 0.44
ACT          -0.42 0.22
SATV          0.42 5.28
SATQ          0.13 5.38


describeBy(SATV + SATQ ~ gender,data =sat.act)  #specify the data set if using formula
 Descriptive statistics by group 
gender: 1
     vars   n   mean     sd median trimmed    mad min max range  skew kurtosis
SATV    1 247 615.11 114.16    630  622.07 118.61 200 800   600 -0.63     0.13
SATQ    2 245 635.87 116.02    660  645.53  94.89 300 800   500 -0.72    -0.12
       se
SATV 7.26
SATQ 7.41
------------------------------------------------------------ 
gender: 2
     vars   n   mean     sd median trimmed    mad min max range  skew kurtosis
SATV    1 453 610.66 112.31    620  617.91 103.78 200 800   600 -0.65     0.42
SATQ    2 442 596.00 113.07    600  602.21 133.43 200 800   600 -0.58     0.13
       se
SATV 5.28
SATQ 5.38

#describeBy(sat.act,list(sat.act$gender,sat.act$education))  #two grouping variables
describeBy(sat.act ~ gender +  education) #two grouping variables


des.mat <- describeBy(age ~ education,mat=TRUE,data = sat.act) #matrix (data.frame) output 
des.mat <- describeBy(age ~ education + gender, data=sat.act,
               mat=TRUE,digits=2)  #matrix output  rounded to 2 decimals  