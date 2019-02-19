# install.packages('BiocManager')
# BiocManager::install('impute', version = '3.8')
# install.packages('PMA')

#steps: 
# 1. find the best tuning parameter
# 2. determine best number of CVs
# 3. compute sCCA
# 4. compute p-value on permuted data
# 5. check CV accuracy of this procedure?

library(PMA)

#make some fake data

make_fake_data <- function(N, j_X, k_Y){
  nvars <- j_X + k_Y
  amat <- MASS::mvrnorm(n = N, mu = rep(0, nvars), Sigma = diag(nvars))
  X <- amat[, 1:j_X]
  Z <- amat[, (j_X+1):(j_X+k_Y)]
  return(list(X = X, Z = Z))
}

set.seed(9929) #so that output is the same
somedata <- make_fake_data(300, 40, 19*18/2)
X <- somedata$X # pretend each is a self report item
Z <- somedata$Z # pretend each is a connectivity value

# check out the help file: ?PMA::CCA.permute

#choose tuning parameter based on the first canonical variates
system.time(acca <- CCA.permute(X, Z, typex = 'standard', typez = 'standard', nperms = 100))
print(acca)
plot(acca)
