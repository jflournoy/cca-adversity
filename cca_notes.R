##---------CCA-------

# We're running sparse CCA. Why?
# In high-dimensional settings where the number of variables exceeds the sample 
# size or when the variables are highly correlated, traditional CCA is no 
# longer appropriate. Sparse estimation produces linear combinations of only 
# a subset of variables from each dataset, thereby increasing the interpretability 
# of the canonical variates (Wilms & Croux, 2015).

# install.packages('BiocManager')
# BiocManager::install('impute', version = '3.8')
# install.packages('PMA')

#steps: 
# 1. find the best tuning parameter
# 2. determine best number of CVs
# 3. compute sCCA
# 4. compute p-value on permuted data
# 5. check CV accuracy of this procedure?

# adversity data: data_final
# MRI data: df_wide

# X # pretend each is a self report item
# Z # pretend each is a connectivity value

#check out the help file: 
#?PMA::CCA
#?PMA::CCA.permute

#Choose tuning parameter based on the first canonical variates
#Basically what's going on here is this:
#For each pair of tuning parameters (the penalties), the rows of X are shuffled
#(so they no longer correspond to to the rows of Y -- this is makes the null
#hypothesis true). A CCA is computed and the correlation between the canonical
#variates is found, c*_i for each of i permutations. The correlation of the
#canonical variates on the unpermuted data is also found, called c. All c*, and
#c, are transformed using fisherz transformation. Then a Z statistic is computed
#that looks at how extreme the original data correlation is in reference to the
#distribution of c* computed on the shuffled/permuted data. We want tuning
#parameters that maximize the correlation in the real data relative to the null.
#The z statistic is just:
# (Fisher(c) - mean(Fisher(c*))) / sd(Fisher(c*))
#


# Remove items that have 0 variance
sort(apply(behavioral_df, 2, var))
# behavioral_df <- subset(behavioral_df, select = -c(vars you want to remove)) # if needed fill in with vars you want to remove
# sort(apply(mri_df, 2, var)) # this would be very unlikely

# Alternative way to remove items with 0 variance
# behavioral_df <- behavioral_df[vapply(behavioral_df, function(x) length(unique(x)) > 1, logical(1L))] 
# mri_df <- mri_df[vapply(mri_df, function(x) length(unique(x)) > 1, logical(1L))] 

# Convert to matrices for CCA.permute
behavioral_df <- data.matrix(behavioral_df) # final behavioral data matrix
mri_df <- data.matrix(mri_df) # final imaging data matrix

system.time(acca <- CCA.permute(behavioral_df, mri_df, typex = 'standard', typez = 'standard', nperms = 100))
# Specify range with: penaltyxs = seq(0, .9, by = .05), penaltyzs = seq(0, .9, by = .05)
# Do more like 1,000 permutations on real data, some might say 10,000 but start with 1,000
print(acca)
plot(acca) # I believe we see here that the correlation for real/permuted data is maximized at .7, and the 
# z-statistic (that looks at how extreme the original data correlation (between canonical variates)
# is in reference to shuffled data correlation) is also maximized at .7
# The CCA.permute function automatically selects tuning parameters for sparse CCA using
# the penalized matrix decomposition. *Original data is (X,Z) and canonical variates are (u,v)
acca$bestpenaltyx # this is the x penalty that resulted in the highest z-statistic
acca$bestpenaltyz # this is the z penalty that resulted in the highest z-statistic
# We're focing it to be fully constrained (by starting at 0), but maybe be don't want to ever fully constrain the model

permute_k_CCs <- function(behavioral_df, mri_df, K = 1, ...){
  nrow <- dim(behavioral_df)[1]
  i <- sample(1:nrow)
  acc <- CCA(behavioral_df[i, ], mri_df, K = K, trace = F, ...)
  return(acc$cors)
}
cc_with_null <- function(behavioral_df, mri_df, K = 1, iter = 100, ...){
  acc <- CCA(behavioral_df, mri_df, K = K, ...)
  f <- function(){
    permute_k_CCs(behavioral_df, mri_df, K = K, ...)
  }
  acc.perm <- replicate(iter, f())
  return(list(CCA = acc, cors.perm = acc.perm))
}

acca2 <- cc_with_null(behavioral_df, mri_df, K = 5, iter = 1000,
                      typex = 'standard', typez = 'standard', 
                      penaltyx = acca$bestpenaltyx,
                      penaltyz = acca$bestpenaltyz)
# We seem to be specifying here that we want two u's and v's (2 canonical variates)
print(acca2$CCA)

par(mfrow = c(1,2))
hist(acca2$cors.perm[1,], breaks = 25)
abline(v = acca2$CCA$cors[1])
hist(acca2$cors.perm[2,], breaks = 25)
abline(v = acca2$CCA$cors[2])
# Seems to show the Cor(Xu, Zv) from the CCA output, which seems to be the 
# canonical correlations between canonical variates 1 and 2
# Show out-of-sample prediction performance
# Calculate p-value of this procedure

# Choose number of CVs that are significant against the null distribution, and automate this process of choosing number

# Use function cca with K = number of CVs and the penalty that we chose 
# We get K canonical pairs, and for each of these pairs we get the vector of u and v, which are the features on
# each side that are important (which was decided by the cca.permute function above)

# Lets say we get to pairs of canonical variates and they're correlated at .9 and .7, what we don't know is 
# is that a high correlation? Because the whole procedure is designed to maximize that number.
# If you only select the imaging values that correlate highly with the behavioral data, then you have to re-do
# the whole procedure many times to make sure that ... 
# One thing that might be interesting - cross-validation procedure where we train test sets on these variates
# and predict resting state features from these canonical variate pairs 



##---------Outstanding to-do's-------

## To do:
# Resolve discrepancies between variables in the two datasets - DONE
# Add back in subject ID variable - DONE
# What to do with NAs? - DONE


##---------Recycling Bin-------

filelist <- dir('')
listofdfs <- lapply(filelist, function(filepath){
  adf <- read.csv(filepath)
  pnumber <- grep('')
  adf$id <- pnumber
  return(adf)
})

setwd("/Users/katherinegrisanzio/Dropbox/Harvard/CCA/all_corrmat")

files = list.files(pattern="*.csv")
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

write.csv(myfiles, "myfiles.csv")



#___

files = list.files(pattern="*.csv")

listofdfs <- lapply(files, function(filepath){
  df <- read.csv(filepath)
  pnumber <- gsub("_.*", "", files)
  df$id <- pnumber
  return(df)
})


#__

files = list.files(pattern="*.csv")

myfiles <- do.call(rbind, lapply(files, function(x){
  df <- read.csv(x, stringsAsFactors = FALSE)
  pnumber <- gsub("_.*", "", files)
  df$id <- pnumber
}))



# Original select and fit cca function
select_and_cca_fit <- function(X, Y, n_selected_vars){
  #select features
  correlations <- cor(Y, X, method = "spearman")
  correlations <- apply(correlations, 2, function(x){max(abs(x))})
  corr.threshold <- sort(correlations, decreasing = T)[n_selected_vars]
  selected.X <- correlations >= corr.threshold
  selected.X <- X[,selected.X]
  #cca fit with best penalty
  system.time(acca <- CCA.permute(Y, selected.X, typex = 'standard', typez = 'standard', nperms = 100))
  permute_k_CCs <- function(Y, selected.X, K = 1, ...){
    nrow <- dim(Y)[1]
    i <- sample(1:nrow)
    acc <- CCA(Y[i, ], selected.X, K = K, trace = F, ...)
    return(acc$cors)
  }
  cc_with_null <- function(Y, selected.X, K = 1, iter = 100, ...){
    acc <- CCA(Y, selected.X, K = K, ...)
    f <- function(){
      permute_k_CCs(Y, selected.X, K = K, ...)
    }
    acc.perm <- replicate(iter, f())
    return(list(CCA = acc, cors.perm = acc.perm))
  }
  acca2 <- cc_with_null(Y, selected.X, K = 5, iter = 1000,
                        typex = 'standard', typez = 'standard', 
                        penaltyx = acca$bestpenaltyx,
                        penaltyz = acca$bestpenaltyz)
  print(acca2$CCA, verbose = TRUE)
  return(list(penalty = c(acca$bestpenaltyx, acca$bestpenaltyz), cca_cors = acca2$CCA$cors))
}

