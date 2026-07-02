https://cran.r-project.org/web/packages/dfidx/refman/dfidx.html
library(dfidx)
# the first two columns contain the indexes
str(munnell)
attach(munnell)
'data.frame':   816 obs. of  12 variables:
 $ state     : chr  "Alabama" "Alabama" "Alabama" "Alabama" ...
 $ year      : int  1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 ...
 $ region    : chr  "East-South Central" "East-South Central" "East-South Central" "East-South Central" ...
 $ president : chr  "Nixon" "Nixon" "Nixon" "Nixon" ...
 $ publiccap : num  15033 15502 15972 16406 16763 ...
 $ highway   : num  7326 7526 7765 7908 8026 ...
 $ water     : num  1656 1721 1765 1742 1735 ...
 $ utilities : num  6051 6255 6442 6756 7002 ...
 $ privatecap: num  35794 37300 38670 40084 42057 ...
 $ gsp       : int  28418 29375 31303 33430 33749 33604 35764 37463 39964 40979 ...
 $ labor     : num  1010 1022 1072 1136 1170 ...
 $ unemp     : num  4.7 5.2 4.7 3.9 5.5 7.7 6.8 7.4 6.3 

str(state)
 chr [1:816] "Alabama" "Alabama" "Alabama" "Alabama" "Alabama" "Alabama" ...
str(year)
int [1:816] 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 .
mn <- dfidx(munnell)
head(mn, 5)
~~~~~~~
 first 5 observations out of 816 
~~~~~~~
              region president publiccap highway   water utilities privatecap
1 East-South Central     Nixon  15032.67 7325.80 1655.68   6051.20   35793.80
2 East-South Central     Nixon  15501.94 7525.94 1721.02   6254.98   37299.91
3 East-South Central     Nixon  15972.41 7765.42 1764.75   6442.23   38670.30
4 East-South Central     Nixon  16406.26 7907.66 1742.41   6756.19   40084.01
5 East-South Central     Nixon  16762.67 8025.52 1734.85   7002.29   42057.31
    gsp  labor unemp       idx
1 28418 1010.5   4.7 Alab:1970
2 29375 1021.9   5.2 Alab:1971
3 31303 1072.3   4.7 Alab:1972
4 33430 1135.5   3.9 Alab:1973
5 33749 1169.8   5.5 Alab:1974

~~~ indexes ~~~~
    state year
1 Alabama 1970
2 Alabama 1971
3 Alabama 1972
4 Alabama 1973
5 Alabama 1974
indexes:  1, 2 


# explicitely indicate the two indexes using either a vector or a
# list of two characters
mn <- dfidx(munnell, idx = c("state", "year"))
head(mn,5)
mn <- dfidx(munnell, idx = list("state", "year"))