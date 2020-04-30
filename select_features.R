## Drysdale (2017) and Dinga (2019) method# From the 37,675 connectivity features (the lower diagonal of the
# functional correlation matrix), they selected the subset of features with
# the highest Spearman's correlation with any of their 17 IDS symptoms.
# The number of RS-fMRI features corresponding to approximately 80% of
# the total number of participants were retained, which corresponds to 176
# RS-fMRI connectivity features in a sample of 220 participants in Drysdale
# Dinga retained the top 150 RS-fMRI features with the highest Spearman's
# correlation with any of the 17 IDS symptoms to preserve the same feature
# to subjects ratio (80% of 187 subjects).

select_features <- function(X, Y, n_selected_vars){
  correlations <- cor(Y, X, method = "spearman")
  correlations <- apply(correlations, 2, function(x){max(abs(x))})
  corr.threshold <- sort(correlations, decreasing = T)[n_selected_vars]
  selected.X <- correlations >= corr.threshold
  selected.X <- X[,selected.X]
}

# Example:
# total_n <- 256
# n_selected_vars <- round(.80*total_n)
# selected_rs_features <- select_features(mri_df[2:ncol(mri_df)], behavioral_df[2:ncol(behavioral_df)], n_selected_vars)

## Xia et al. (2018) method# As features that do not vary across subjects cannot be predictive of
# individual differences, we limited our analysis of connectivity data
# to the top ten percent most variable connections, as measured by median
# absolute deviation, which is more robust against outliers than standard
# deviation.
select_features2 <- function(X, n_selected_vars2){
  mads <- apply(X, 2, mad)
  mads.threshold <- sort(mads, decreasing = T)[n_selected_vars2]
  selected.X <- mads >= mads.threshold
  selected.X <- X[,selected.X]
}

#Example:
# n_selected_vars2 <- round(.10*(ncol(mri_df)-1)) # -1 to exclude the ID col
# selected_rs_features2 <- select_features2(mri_df[2:ncol(mri_df)], n_selected_vars2)