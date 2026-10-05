library(complex)
https://cran.r-project.org/web/packages/complex/refman/complex.html

# Generate random complex variables
y <- complex(real=rnorm(100,100,10), imaginary=rnorm(100,100,10))
yLog <- clog(y)
cexp(yLog)

# Generate random complex variables
x <- complex(real=rnorm(100,10,10), imaginary=rnorm(100,10,10))

# Get a matrix and a vector for one value
complex2mat(x[1])
complex2vec(x[1])

# Get matrices for all values
complex2mat(x)
complex2vec(x)



# Generate random complex variables
x <- complex(real=rnorm(100,10,10), imaginary=rnorm(100,10,10))
y <- complex(real=rnorm(100,10,10), imaginary=rnorm(100,10,10))
cplot(x, y)