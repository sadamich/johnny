constrast
utils::example(factor)
str(ff)
str(ff)
 Factor w/ 26 levels "a","b","c","d",..: 19 20 1 20 9 19 20 9 3 19
ff
[1] s t a t i s t i c s
Levels: a b c d e f g h i j k l m n o p q r s t u v w x y z
fff <- ff[, drop = TRUE]  # reduce to 5 levels.
fff
 [1] s t a t i s t i c s
Levels: a c i s t
contrasts(fff) # treatment contrasts by default
  c i s t
a 0 0 0 0
c 1 0 0 0
i 0 1 0 0
s 0 0 1 0
t 0 0 0 1
contrasts(C(fff, sum))
  [,1] [,2] [,3] [,4]
a    1    0    0    0
c    0    1    0    0
i    0    0    1    0
s    0    0    0    1
t   -1   -1   -1   -1
contrasts(fff, contrasts = FALSE) # the 5x5 identity matrix
 a c i s t
a 1 0 0 0 0
c 0 1 0 0 0
i 0 0 1 0 0
s 0 0 0 1 0
t 0 0 0 0 1

contrasts(fff) <- contr.sum(5); contrasts(fff)  # set sum contrasts
contrasts(fff, 2) <- contr.sum(5); contrasts(fff)  # set 2 contrasts
# supply 2 contrasts, compute 2 more to make full set of 4.
contrasts(fff) <- contr.sum(5)[, 1:2]; contrasts(fff)

## using sparse contrasts: % useful, once model.matrix() works with these :
ffs <- fff
contrasts(ffs) <- contr.sum(5, sparse = TRUE)[, 1:2]; contrasts(ffs)
stopifnot(all.equal(ffs, fff))
contrasts(ffs) <- contr.sum(5, sparse = TRUE); contrasts(ffs)