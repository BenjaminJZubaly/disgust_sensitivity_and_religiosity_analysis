# Loading the powerMediation package to calculate sample size given expected parameters
library(powerMediation)

# Calculating sample size needed for sufficient power for Sobel tests of mediation
 # Calculate power for Sobel test 

# Calculate sample size assuming small relationships with standardized variables
  # Use a small R^2 value (.1) to estimate sigma.epsilon
      #(given sigma.Y is 1 due to standardization)
sigma.Y <- 1
R.squared.1 <- .1
sigma.epsilon.1 <- sqrt(sigma.Y^2 * (1-R.squared.1))
  # Estimate the sample size
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .1, # Very small estimated coefficient
                  lambda.a = .1, # Very small estimated coefficient
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.1, # Estimated above
                  alpha = .05,
                  verbose = TRUE)
  # Result: 1491

# Calculate sample size assuming small-medium relationships with standardized variables
  # Use same sigma.epsilon to estimate conservatively
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .2, # Small-medium estimated coefficient
                  lambda.a = .2, # Small-medium estimated coefficient
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.1, # Estimated above
                  alpha = .05,
                  verbose = TRUE)
  # Result: 373

# Calculate sample size assuming medium relationships with standardized variables
  # Use same sigma.epsilon to estimate conservatively
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .3, # Medium estimated coefficient
                  lambda.a = .3, # Medium estimated coefficient
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.1, # Estimated above
                  alpha = .05,
                  verbose = TRUE)
  # Result: 166

# Rerunning the same estimates but with only 5% of variance explained by the models
  # Meaning the sigma.epsilon is more conservative
sigma.Y <- 1
R.squared.2 <- .05
sigma.epsilon.2 <- sqrt(sigma.Y^2 * (1-R.squared.2))

  # For small coefficients
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .1, # Very small estimated coefficient
                  lambda.a = .1, # Very small estimated coefficient
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.2, # Estimated above
                  alpha = .05,
                  verbose = TRUE)
    # Result: 1531

  # For small-medium coefficients
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .2, # Small-medium estimated coefficient
                  lambda.a = .2, # Small-medium estimated coefficient
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.2, # Estimated above
                  alpha = .05,
                  verbose = TRUE)
    # Result: 383

  # For medium coefficients
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .3, # Medium estimated coefficient
                  lambda.a = .3, # Medium estimated coefficient
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.2, # Estimated above
                  alpha = .05,
                  verbose = TRUE)
    # Result: 171

# Using estimates from previous studies
  # For conventionalism as mediator
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .26, # From Tybur et al. (2015)
                  lambda.a = .22, # Parents estimate from Dunwoody and Funke (2016)
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.1, # Estimated from R^2 = .1
                  alpha = .05,
                  verbose = TRUE)
    # Result: 265
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .26, # From Tybur et al. (2015)
                  lambda.a = .22, # Parents estimate from Dunwoody and Funke (2016)
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.2, # Estimated from R^2 = .05
                  alpha = .05,
                  verbose = TRUE)
    # Result: 274

  # For GENE as mediator
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .26, # From Tybur et al. (2015)
                  lambda.a = .20, # Arbitrary because not aware of study with GENE and religiosity
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.1, # Estimated from R^2 = .1
                  alpha = .05,
                  verbose = TRUE)
    # Result: 298
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .26, # From Tybur et al. (2015)
                  lambda.a = .20, # Arbitrary because not aware of study with GENE and religiosity
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.2, # Estimated from R^2 = .05
                  alpha = .05,
                  verbose = TRUE)
    # Result: 309

  # For SOI-R attitudes as mediator
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .26, # From Tybur et al. (2015)
                  lambda.a = .21, # From Penke and Asendorpf (2008)
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.1, # Estimated from R^2 = .1
                  alpha = .05,
                  verbose = TRUE)
    # Result: 281
ssMediation.Sobel(power = .8, # For 80% power
                  theta.1a = .26, # From Tybur et al. (2015)
                  lambda.a = .21, # From Penke and Asendorpf (2008)
                  sigma.x = 1, # Assume 1 due to standardization
                  sigma.m = 1, # Assume 1 due to standardization
                  sigma.epsilon = sigma.epsilon.2, # Estimated from R^2 = .1
                  alpha = .05,
                  verbose = TRUE)
    # Result: 290

