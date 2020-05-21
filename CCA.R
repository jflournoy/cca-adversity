# CCA Analysis Questionnaire Coding
# Katherine Grisanzio & John Flournoy

NCPU = as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE')) #How many CPUS we have to run this chunk
TASK_ID = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID')) #This chunk's number
MAX_TASKS = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_MAX')) #How many total chunks this has been split up into
#MAX_TASKS=3
#TASK_ID=2

if(!file.exists('data/permute_index.rds')){
  stop('Please generate the permutations using `generate_permutations.R`.')
} else {
  permutations <- readRDS('data/permute_index.rds')
  if(any(is.na(c(NCPU, TASK_ID, MAX_TASKS)))){
    message('Not running on NCF, setting small values for test purposes')
    NCPU = 10
    fname = file.path('data', 'test.rds')
    permutations_i <- 1:10
  } else {
    message('Running on NCF.
Using ', NCPU, ' cores.
This is chunk ', TASK_ID,' of ', MAX_TASKS, '.')
    fname = file.path('data', paste0('cca_perms-chunk_', TASK_ID, '.rds'))
    message('Output filename: ', fname)
    permutations_i <- split(1:dim(permutations)[1], f = 1:MAX_TASKS)[[TASK_ID]]
    message('Running ', length(permutations_i), ' of ', dim(permutations)[1], ' permutations...')
  }
}

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

if(var(c(dim(permutations)[[2]], dim(mri_df)[[1]], dim(behavioral_df)[[1]])) > 0){
  stop('Dimensions of data and/or permutations are not the same.')
}

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

select_and_cca_fit <- function(X, Y, K, n_selected_vars, selection_function){
  #select features
  selected.X <- selection_function(X, Y, n_selected_vars)
  #cca fit with best penalty
  penalties = seq(0, 1, length.out = 20) #This covers the whole range pretty well, though maybe overkill
  system.time(acca <- CCA.permute(Y, selected.X, typex = 'standard', typez = 'standard', 
                                  penaltyxs = penalties, penaltyzs = penalties,
                                  nperms = 1000, trace = FALSE))
  acca2 <- CCA(Y, selected.X, K = K,
               typex = 'standard', typez = 'standard', 
               penaltyx = acca$bestpenaltyx,
               penaltyz = acca$bestpenaltyz, 
               trace = FALSE)
  return(list(penalty = c(acca$bestpenaltyx, acca$bestpenaltyz), 
              cca_cors = acca2$cors))
}

total_n <- dim(mri_df)[1]

message('Creating cluster with ', NCPU, ' cores...')
cl <- parallel::makePSOCKcluster(NCPU)
message('Exporting data to cluster nodes...')
parallel::clusterExport(cl = cl, 
                        varlist = c('mri_df', 'behavioral_df', 'total_n',
                                    'select_features_drysdale', 'permutations',
                                    'select_and_cca_fit'))
message('Running permutations...')
system.time({
  rez <- parallel::parLapply(
    cl = cl, X = permutations_i, 
    fun = function(i){
      library(PMA)
      perm <- permutations[i,]
      cca_output_drysdale <- select_and_cca_fit(X = mri_df[perm, 2:ncol(mri_df)],
                                                Y = behavioral_df[,2:ncol(behavioral_df)],
                                                K = 10,
                                                n_selected_vars = round(.80*total_n),
                                                selection_function = select_features_drysdale)
      return(cca_output_drysdale)
    })
})
parallel::stopCluster(cl)

saveRDS(rez, fname)

# cca_output_xia <- select_and_cca_fit(X = mri_df[2:ncol(mri_df)], 
#                                      Y = behavioral_df[2:ncol(behavioral_df)], 
#                                      n_selected_vars = round(.10*(ncol(mri_df)-1)), # -1 to exclude the ID col; Y isn't used here
#                                      selection_function = select_features_xia) 


