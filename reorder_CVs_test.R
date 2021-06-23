
#from https://github.com/cedricx/sCCA/commit/3964fb616071980e6a0827a70938d425421eea62 
# https://github.com/cedricx/sCCA/blob/master/sCCA/code/final/cca_functions.R
reorderCCA <- function(res,org,k){
  u <- org$u
  v <- org$v
  
  res.match<-apply(abs(cor(v,res$v[,1:k])),1,which.max)
  res.cor <- apply(abs(cor(v,res$v[,1:k])),1,max)
  res.match.count <- length(unique(res.match))
  
  u.od <- res$u[,res.match]
  v.od <- res$v[,res.match]
  cors.final <- res$cors[res.match]
  
  org.sign <- sign(colMeans(sign(v)))
  res.sign <- sign(colMeans(sign(v.od)))
  
  sign.prod <- org.sign *res.sign
  
  u.final <- t(t(u.od) * sign.prod)
  v.final <- t(t(v.od) * sign.prod)
  
  res.one.reorder <- list(u= u.final ,v= v.final, cors = cors.final, pos = res.match, dimcor = res.cor)
  
  if (res.match.count < dim(u)[2]  ) {
    u.na <- array(NA,dim= c(dim(u)[1],dim(u)[2]))
    v.na <- array(NA,dim= c(dim(v)[1],dim(v)[2]))
    cors.na <- rep(NA,dim(u)[2])
    match.na <- cors.na
    dimcor.na <- cors.na
    res.one.reorder <- list(u= u.na,v= v.na, cors = cors.na, pos = res.match, dimcor = dimcor.na) 
  }
  out <- res.one.reorder
}

# X <- X.orig[sample(1:nrow(X.orig)),]
# Z <- Z.orig[sample(1:nrow(Z.orig)),]
gen_aperm <- function(py=.5, px=.5, rank = FALSE, K = 100, X, Z, orig){
  p.x.z <- expand.grid(x = seq(.1, .8, length.out = 3), z = seq(.1, .8, length.out = 3))
  gridsearch <- PMA::CCA.permute(X, Z, penaltyxs = p.x.z$x, penaltyzs = p.x.z$z, nperms = 25, trace = FALSE)
  r <- PMA::CCA(X, Z, penaltyx = gridsearch$bestpenaltyx, penaltyz = gridsearch$bestpenaltyx, trace = FALSE, K = K)
  final_cors <- r$cors
    
  if(rank){
    reord <- reorderCCA(res = r, org = orig, k = K)
    final_cors <- reord$cors
  }
  return(final_cors)
}

do_perm_set <- function(X.orig, Y.orig, orig, rank, K, iter){
  cl <- parallel::makePSOCKcluster(5)
  parallel::clusterExport(cl = cl, 
                          varlist = c('gen_aperm', 'X.orig', 'Z.orig', 'orig', 'reorderCCA', 'rank'), 
                          envir = rlang::env(gen_aperm = gen_aperm,
                                             X.orig = X.orig,
                                             Z.orig = Z.orig, 
                                             orig = orig,
                                             reorderCCA = reorderCCA,
                                             rank = rank,
                                             K = K))
  
  perms <- parallel::parLapply(cl = cl, X = split(1:iter, 1:5), fun = function(i_chunk){
    r <- lapply(i_chunk, function(i){
      X <- X.orig[sample(1:nrow(X.orig)),]
      Z <- Z.orig[sample(1:nrow(Z.orig)),]
      r <- gen_aperm(py=.5, px=.5, rank = rank, K = K, X, Z, orig)
      return(r)
    })
    return(r)
  })
  
  parallel::stopCluster(cl)
  return(perms)
}
library(ggplot2)

iter = 1000
K = 7
Sigma <- diag(400)
Sigma[1:50, 1:50] <- .35
Sigma[51:150, 51:150] <- .35
Sigma[151:200, 151:200] <- .35
Sigma[1:50+200, 1:50+200] <- .35
Sigma[51:150+200, 51:150+200] <- .35
Sigma[151:200+200, 151:200+200] <- .35
Sigma[1:50, 1:50+200] <- .15
Sigma[51:150, 51:150+200] <- .15
Sigma[151:200, 151:200+200] <- .15
Sigma[1:50+200, 1:50] <- .15
Sigma[51:150+200, 51:150] <- .15
Sigma[151:200+200, 151:200] <- .15
diag(Sigma) <- 1
ggplot(data = reshape2::melt(Sigma), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

ALL.orig <- MASS::mvrnorm(500, mu = rep(0, 400), Sigma = Sigma)
dim(ALL.orig)
X.orig <- ALL.orig[, 1:200]
Z.orig <- ALL.orig[, 201:400]

ggplot(data = reshape2::melt(cor(ALL.orig)), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

cor(apply(X.orig[,1:50], 1, sum), apply(Z.orig[,1:50], 1, sum))
cor(apply(X.orig[,51:150], 1, sum), apply(Z.orig[,51:150], 1, sum))
cor(apply(X.orig[,151:200], 1, sum), apply(Z.orig[,151:200], 1, sum))

p.x.z <- expand.grid(x = seq(.1, .8, length.out = 3), z = seq(.1, .8, length.out = 3))
orig.gridsearch <- PMA::CCA.permute(X.orig, Z.orig, penaltyxs = p.x.z$x, penaltyzs = p.x.z$z, nperms = 25, trace = FALSE)
orig <- PMA::CCA(X.orig, Z.orig, 
                 penaltyx = orig.gridsearch$bestpenaltyx, 
                 penaltyz = orig.gridsearch$bestpenaltyz, 
                 trace = FALSE, K = 3)

orig3 <- PMA::CCA(X.orig, Z.orig, penaltyx = orig.gridsearch$bestpenaltyx, penaltyz = orig.gridsearch$bestpenaltyz, trace = FALSE, K = 3)
orig5 <- PMA::CCA(X.orig, Z.orig, penaltyx = orig.gridsearch$bestpenaltyx, penaltyz = orig.gridsearch$bestpenaltyz, trace = FALSE, K = 5)
orig5.perm <- PMA::CCA(X.orig[sample(1:nrow(X.orig)), ], Z.orig, penaltyx = orig.gridsearch$bestpenaltyx, penaltyz = orig.gridsearch$bestpenaltyz, trace = FALSE, K = 5)
orig5.perm2 <- PMA::CCA(X.orig[sample(1:nrow(X.orig)), ], Z.orig, penaltyx = orig.gridsearch$bestpenaltyx, penaltyz = orig.gridsearch$bestpenaltyz, trace = FALSE, K = 5)
d <- rbind(reshape2::melt(orig3$v), reshape2::melt(orig5$v), reshape2::melt(orig5.perm$v), reshape2::melt(orig5.perm2$v))
d$model <- c(rep(3, 600), rep(5, 1000), rep('perm', 1000), rep('perm2', 1000))
ggplot(data = d, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  facet_grid(model ~.)

perms_rank_10 <- do_perm_set(X.orig = X.orig, Y.orig = Y.orig, orig = orig, rank = TRUE, K = K, iter = iter)
perms_rank_10_1 <- unlist(lapply(unlist(perms_rank_10, recursive = FALSE), `[[`, 1))
N_na <- sum(is.na(perms_rank_10_1))
hist(perms_rank_10_1)

perms_norank_10 <- do_perm_set(X.orig = X.orig, Y.orig = Y.orig, orig = orig, rank = FALSE, K = K, iter = iter)
perms_norank_10_1 <- unlist(lapply(unlist(perms_norank_10, recursive = FALSE), `[[`, 1))

perms_norank_1 <- do_perm_set(X.orig = X.orig, Y.orig = Y.orig, orig = orig, rank = FALSE, K = 1, iter = iter)
perms_norank_1_1 <- unlist(lapply(unlist(perms_norank_1, recursive = FALSE), `[[`, 1))

library(ggplot2)
library(patchwork)
df <- data.frame(x = c(perms_rank_10_1, perms_norank_10_1, perms_norank_1_1),
                 model = c(rep(sprintf('Rank %d', K), length(perms_rank_10_1)),
                           rep(sprintf('No Rank %d', K), length(perms_norank_10_1)),
                           rep('Singleton', length(perms_norank_1_1))))
ggplot(df, aes(x = x, fill = model)) + 
  geom_density(alpha = .5, position = 'identity', aes(y = ..density..)) + 
  geom_vline(xintercept = orig$cors[[1]]) + 
  theme_minimal() +
  labs(y = '') + 
  scale_y_continuous(breaks = NULL) +
ggplot(df, aes(x = x, fill = model)) + 
  geom_density(alpha = 1, position = 'identity', aes(y = ..count..)) + 
  geom_label(data = data.frame(model = sprintf('Rank %d', K), label = sprintf('Missing: %d / %d', N_na, iter)),
             aes(x = .3, y = 0, label = label, fill = NULL), alpha = .5) + 
  theme_minimal() + 
  facet_grid(~model) + 
  labs(y = '') + 
  scale_y_continuous(breaks = NULL) +
# ggplot(df, aes(x = x, fill = model)) + 
#   geom_histogram(alpha = 1, position = 'identity', aes(y = ..count..)) + 
#   theme_minimal() + 
#   facet_grid(~model) + 
plot_layout(design = 'A\nB')

