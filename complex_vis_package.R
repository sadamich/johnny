library(viscomplexr)
https://cran.r-project.org/web/packages/viscomplexr/index.html

# Generate random vector of 17 zeroes inside the unit disk
n <- 17
a <- complex(modulus = runif(n, 0, 1), argument = runif(n, 0, 2*pi))

# Portrait the Blaschke product
phasePortrait(blaschkeProd, moreArgs = list(a = a),
  xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
  nCores = 1) # Max. two cores on CRAN, not a limit for your use


phasePortrait(jacobiTheta, moreArgs = list(tau = 1i/2-1/4),
pType = "p", xlim = c(-2, 2), ylim = c(-2, 2),
nCores = 1) # Max. two cores on CRAN, not a limit for your use


# Map the complex plane on itself

# x11(width = 8, height = 8)   # Screen device commented out
                               # due to CRAN test requirements.
                               # Use it when trying this example
phasePortrait("z", xlim = c(-2, 2), ylim = c(-2, 2),
              xlab = "real", ylab = "imaginary",
              verbose = FALSE, # Suppress progress messages
              nCores = 2)      # Max. two cores allowed on CRAN
                               # not a limit for your own use
