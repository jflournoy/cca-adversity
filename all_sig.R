library(permute)
library(CCA)

select_features_drysdale <- function(X, Y, n_selected_vars = NULL){
  if(is.null(n_selected_vars)){
    total_n <- dim(X)[1]
    n_selected_vars <- round(.80*total_n)
  }
  correlations <- cor(Y, X, method = "spearman")
  correlations <- apply(correlations, 2, function(x){max(abs(x))})
  corr.threshold <- sort(correlations, decreasing = T)[n_selected_vars]
  selected.X <- correlations >= corr.threshold
  selected.X <- X[,selected.X]
  return(selected.X)
}


null_cca <- function(N, pX, pY){
  system.time({
    X_ <- mvnfast::rmvn(n = N, mu = rep(0, pX), sigma = diag(pX), ncores = 1)
    Y <- mvnfast::rmvn(n = N, mu = rep(0, pY), sigma = diag(pY), ncores = 1)
  })
  
  n <- dim(X_)[1]
  X <- select_features_drysdale(X_, Y, n_selected_vars = .8*n)
  
  cc1 <- CCA::cc(X, Y)
  rho <- cc1$cor
  p <- dim(X)[2]
  q <- dim(Y)[2]
  
  test <- CCP::p.asym(rho = rho, N = n, p = p, q = q, tstat = 'Wilks')
  stat <- test$stat
  p <- test$p.value
  
  return(list(rho = rho, stat = stat, p = p, k = 1:length(r)))
}


pY <- 30
pX <- 1000
N <- 300

if(FALSE){
  rez <- parallel::mclapply(1:1000, 
                            function(i) return(null_cca(N = N, pX = pX, pY = pY)),
                            mc.cores = parallel::detectCores()-1)
  saveRDS(rez, file = 'all_sig.Rds')
} else {
  rez <- readRDS('all_sig.Rds')  
}

rez_dt <- data.table::rbindlist(rez)
rez_dt[, k_fac := factor(k, levels = rev(unique(k)))]

library(ggplot2)
segcol <- '#bb6a23'
dotfill <- '#ffd653'
denscol <- '#2a5078'
densfill <- '#a7c6b8'

lapply(list('rho', 'p', 'stat'),
       function(avar){
         ggplot(rez_dt[k < (2*3 + 1)], aes_string(x = avar)) + 
           geom_histogram(bins = 50, fill = densfill, color = denscol) + 
           facet_wrap(~k, ncol = 2) + 
           theme_minimal()
         
       })
