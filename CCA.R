# CCA Analysis Questionnaire Coding
# Katherine Grisanzio
# 5/15/19


##---------Load packages and import data-------

if (!require("PMA")) {install.packages("PMA"); require("PMA")}
if (!require("readr")) {install.packages("readr"); require("readr")}

behavioral_df_all <- data.frame(readr::read_csv('data/behavior_data.csv'))
mri_df_all <- readRDS('data/rsfc_data.rds')
sub_list_DT <- read.csv("subject_lists/Rest_DT-CCA_n118.csv")
sub_list_MT <- read.csv("subject_lists/Rest_MT-CCA_n121.csv")

##---------Select resting state features-------

# Retain participants who have clean scan data from Steph's QC
sub_list <- rbind(sub_list_DT, sub_list_MT)
names(sub_list)[1] <- "ID"

behavioral_df <- merge(behavioral_df_all, sub_list, by = "ID", all = FALSE) # final behavioral dataframe
mri_df <- merge(mri_df_all, behavioral_df[1], by = "ID", all = FALSE) # final imaging dataframe
  # the reason I merged mri_df_all with behavioral_df instead of sub_list is because there are apparently two
  # subjects who completed the scan but not the behavioral data. So this way of merging makes the dimensions
  # of the two datasets to be the same (N=236)


## Drysdale (2017) and Dinga (2019) method

# From the 37,675 connectivity features (the lower diagonal of the
# functional correlation matrix), they selected the subset of features with
# the highest Spearman's correlation with any of their 17 IDS symptoms.
# The number of RS-fMRI features corresponding to approximately 80% of 
# the total number of participants were retained, which corresponds to 176 
# RS-fMRI connectivity features in a sample of 220 participants in Drysdale
# Dinga retained the top 150 RS-fMRI features with the highest Spearman's 
# correlation with any of the 17 IDS symptoms to preserve the same feature 
# to subjects ratio (80% of 187 subjects).

select_features_drysdale <- function(X, Y, n_selected_vars){
  correlations <- cor(Y, X, method = "spearman")
  correlations <- apply(correlations, 2, function(x){max(abs(x))})
  corr.threshold <- sort(correlations, decreasing = T)[n_selected_vars]
  selected.X <- correlations >= corr.threshold
  selected.X <- X[,selected.X]
}


## Xia et al. (2018) method

# As features that do not vary across subjects cannot be predictive of 
# individual differences, we limited our analysis of connectivity data 
# to the top ten percent most variable connections, as measured by median 
# absolute deviation, which is more robust against outliers than standard 
# deviation.

select_features_xia <- function(X, Y, n_selected_vars){
  mads <- apply(X, 2, mad)
  mads.threshold <- sort(mads, decreasing = T)[n_selected_vars]
  selected.X <- mads >= mads.threshold
  selected.X <- X[,selected.X]
}

# Select features + CCA function

select_and_cca_fit <- function(X, Y, n_selected_vars, selection_function){
  #select features
  selected.X <- selection_function(X, Y, n_selected_vars)
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
  return(list(penalty = c(acca$bestpenaltyx, acca$bestpenaltyz), cca_cors = acca2$CCA$cors, loadings = c(acca2$CCA$u, acca2$CCA$v)))
}

total_n <- dim(mri_df)[1]

cca_output_drysdale <- select_and_cca_fit(X = mri_df[2:ncol(mri_df)], 
                                          Y = behavioral_df[2:ncol(behavioral_df)], 
                                          n_selected_vars = round(.80*total_n), 
                                          selection_function = select_features_drysdale)
cca_output_xia <- select_and_cca_fit(X = mri_df[2:ncol(mri_df)], 
                                     Y = behavioral_df[2:ncol(behavioral_df)], 
                                     n_selected_vars = round(.10*(ncol(mri_df)-1)), 
                                     selection_function = select_features_xia) # -1 to exclude the ID col; Y isn't used here
