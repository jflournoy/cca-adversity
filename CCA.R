# CCA Analysis
# Katherine Grisanzio & John Flournoy

library(argparse)
library(PMA)
library(candisc)
library(readr)

parser <- ArgumentParser(description='Run CCA analysis')
parser$add_argument('--nopermute', action = 'store_true', help = 'Run the CCA on the unpermuted observed data and save the result. Do not run any permutations (this CCA will not be computed on unpermuted data when running permutations).')
parser$add_argument('--useraw', action = 'store_true', help = 'Use the un-residualized, raw data for X and Y. Otherwise, we will use data residualized on Age and Sex. (NOT YET IMPLEMENTED)')
parser$add_argument('--selectfun', type="character", required = TRUE,
                    help = 'Selection function to use for data reduction prior to CCA. The character string given here will be postpended to the string `select_features_`. To specify the function `select_features_drysdale` use "--selectfun drysdale".')
parser$add_argument('--maxchunks', type="integer",
                    help = 'Number of total chunks to split permutation index into.')
parser$add_argument('--chunkid', type="integer",
                    help = 'Index of the chunk of permutations to be run after the permutation index has been split into the number of chunks specified by "--maxchunks"')
parser$add_argument('--mc.cores', type="integer", default = 1,
                    help = 'Number of cores to parallelize over.')
parser$add_argument('--datadir', type="character",default = 'data', 
                    help = 'Directory where data and permutation index are stored, and where output is saved.')
parser$add_argument('--permfile', type="character",default = 'permute_index.rds',
                    help = 'RDS file containing permutation matrix')
parser$add_argument('--innerperms', type="integer",default = 1000,
                    help = 'Number of permutations for choosing penalty.')
parser$add_argument('--k', type = 'integer', default = NULL, 
                    help = 'Number of canonical variate pairs. Default will use min(p,q).')
parser$add_argument('--noreg', action = 'store_true', 
                    help = 'Use vanilla CCA from the candisc package, rather than regularized CCA (the default).')
parser$add_argument('--behfeatures', type="character", required = TRUE,
                    help = 'csv file with behavioral features')
parser$add_argument('--behsuffix', type="character", required = TRUE,
                    help = 'add this to the output filenames')
parser$add_argument('--behfeaturesID', type="character", default = 'ID',
                    help = 'ID column for this file')

#args <- parser$parse_args(c('--selectfun', 'xia', '--mc.cores', '4', '--chunkid', '1', '--maxchunks', '1000',  '--innerperms', '25', '--behfeatures', 'data/cca_psy_resid.csv', '--behfeaturesID', 'ID', '--nopermute', '--behsuffix', 'psy')) # 
args <- parser$parse_args()
NCPU = args$mc.cores
if(!is.na(as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))) & 
   NCPU < as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))){
  warning('mc.cores option is set to fewer than the available cores. Processing will be inefficient.')
}
CHUNK_ID = args$chunkid
MAX_TASKS = args$maxchunks
SELECTION = args$selectfun
NPERMS = args$innerperms
K = args$k
NOREG = args$noreg
BEHFEATFILE = args$behfeatures
BEHSUFFIX = args$behsuffix
IDCOL = args$behfeaturesID

#for file naming, later
REGTYPE <- ''
if(NOREG){
  REGTYPE <- '-noreg'
}

##---------Load packages and import data-------

behavioral_df_all <- data.frame(readr::read_csv(BEHFEATFILE))
mri_df_all <- readRDS('data/rsfc_data_resid.rds')
sub_list_DT <- read.csv("subject_lists/Rest_DT-CCA_n118.csv")
sub_list_MT <- read.csv("subject_lists/Rest_MT-CCA_n121.csv")

##---------Select resting state features-------

# Retain participants who have clean scan data from Steph's QC
sub_list <- rbind(sub_list_DT, sub_list_MT)
names(sub_list)[1] <- IDCOL
names(mri_df_all)[which(names(mri_df_all) == 'ID')] <- IDCOL

behavioral_df <- merge(behavioral_df_all, sub_list, by = IDCOL, all = FALSE) # final behavioral dataframe
mri_df <- merge(mri_df_all, behavioral_df[1], by = IDCOL, all = FALSE) # final imaging dataframe
  # Katherine: the reason I merged mri_df_all with behavioral_df instead of sub_list is because there are apparently two
  # subjects who completed the scan but not the behavioral data. So this way of merging makes the dimensions
  # of the two datasets to be the same (N=238)


## No feature selection

#Does what it says on the can.
select_features_nofeatsel <- function(X, Y, n_selected_vars = NULL){
  return(list(X = X, Y = Y))
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
  return(list(X = selected.X, Y = Y))
}

select_features_drysdale2 <- function(X, Y, n_selected_vars = NULL){
  if(is.null(n_selected_vars)){
    total_p <- dim(X)[2]
    n_selected_vars <- round(.20*total_p)
  }
  selected <- select_features_drysdale(X, Y, n_selected_vars = n_selected_vars)
  return(selected)
}

select_features_drysdale3 <- function(X, Y, n_selected_vars = 100){
  #Select the top N highly correlated features on both sides, to be submitted to
  #a non-regularized CCA.
  
  #select X (MRI vars)
  correlations <- cor(X, Y, method = "spearman")
  correlations_max.X <- apply(correlations, 1, function(x){max(abs(x))})
  correlations_max.Y <- apply(correlations, 2, function(x){max(abs(x))})
  selected.X.names <- names(sort(correlations_max.X, decreasing = T)[1:n_selected_vars])
  selected.Y.names <- names(sort(correlations_max.Y, decreasing = T)[1:n_selected_vars])
  selected.X <- X[, selected.X.names]
  selected.Y <- Y[, selected.Y.names]
  
  return(list(X = selected.X, Y = selected.Y))
}

## Xia et al. (2018) method

# As features that do not vary across subjects cannot be predictive of 
# individual differences, we limited our analysis of connectivity data 
# to the top ten percent most variable connections, as measured by median 
# absolute deviation, which is more robust against outliers than standard 
# deviation.

select_features_xia <- function(X, Y, n_selected_vars = NULL){
  if(is.null(n_selected_vars)){
    total_p <- dim(X)[2]
    n_selected_vars <- round(.10*(total_p))
  }
  mads <- apply(X, 2, mad)
  mads.threshold <- sort(mads, decreasing = T)[n_selected_vars]
  selected.X <- mads >= mads.threshold
  selected.X <- X[,selected.X]
  return(list(X = selected.X, Y = Y))
}

selectfun <- eval(parse(text = paste0('select_features_', SELECTION)))

#cca funcitons

noreg_cca <- function(selected.Y, selected.X,
                      K, nperms, 
                      return_cca_object){
  acca <- candisc::cancor(selected.X, selected.Y, ndim = K)
  rez <- list(penalty = NULL, cca_cors = acca$cancor)
  if(return_cca_object){
    rez <- c(list(cca_obj = acca, cca.permute_obj = NULL), rez)
  }
  return(rez)
}

reg_cca <- function(selected.Y, selected.X,
                    K, nperms, 
                    return_cca_object){
  #cca fit with best penalty
  penalties = seq(.1, .7, length.out = 10) #this is PMA::CCA.permute default
  system.time(acca <- PMA::CCA.permute(selected.Y, selected.X, typex = 'standard', typez = 'standard', 
                                  penaltyxs = penalties, penaltyzs = penalties,
                                  nperms = nperms, trace = FALSE))
  acca2 <- PMA::CCA(selected.Y, selected.X, K = K,
               typex = 'standard', typez = 'standard', 
               penaltyx = acca$bestpenaltyx,
               penaltyz = acca$bestpenaltyz, 
               trace = FALSE)
  rez <- list(penalty = c(acca$bestpenaltyx, acca$bestpenaltyz), 
              cca_cors = acca2$cors)
  if(return_cca_object){
    rez <- c(list(cca_obj = acca2, cca.permute_obj = acca), rez)
  }
  return(rez)
}

# Select features + CCA function

select_and_cca_fit <- function(X, Y, K, selection_function, return_cca_object = FALSE, nperms = 1000, noreg = FALSE){
  #select features
  selected_features <- selection_function(X, Y)
  selected.X <- selected_features[['X']]
  selected.Y <- selected_features[['Y']]
  
  if(is.null(K)){
    K <- min(dim(selected.X)[2], dim(selected.Y)[2])
  } 
  if(K > dim(selected.X)[1] & noreg){
    stop("K is greater than number of observations. You should use regularization.")
  }
  
  if(noreg){
    rez <- noreg_cca(selected.Y = selected.Y, selected.X = selected.X,
                     K = K, nperms = nperms, 
                     return_cca_object = return_cca_object)
    
  } else {
    rez <- reg_cca(selected.Y = selected.Y, selected.X = selected.X,
                   K = K, nperms = nperms, 
                   return_cca_object = return_cca_object)
  }

  return(rez)
}

if(!args$nopermute){
  if(!file.exists('data/permute_index.rds')){
    stop('Please generate the permutations using `generate_permutations.R`.')
  } else {
    permutations <- readRDS('data/permute_index.rds')
    if(any(is.na(c(NCPU, CHUNK_ID, MAX_TASKS)))){
      message('Not running on NCF, setting small values for test purposes')
      NCPU = 10
      fname = file.path('data', 'test.rds')
      permutations_i <- 1:10
    } else {
      message('Running on NCF.
Using ', NCPU, ' cores.
This is chunk ', CHUNK_ID,' of ', MAX_TASKS, '.')
      fname = file.path('data', paste0('cca_perms-', BEHSUFFIX, '-', SELECTION, REGTYPE, '-chunk_', CHUNK_ID, '.rds'))
      message('Output filename: ', fname)
      permutations_i <- split(1:dim(permutations)[1], f = 1:MAX_TASKS)[[CHUNK_ID]]
      message('Running ', length(permutations_i), ' of ', dim(permutations)[1], ' permutations...')
    }
  }
  if(file.exists(fname)){
    stop(paste0('Output ', fname, ' already exists. Stopping.'))
  }
  if(var(c(dim(permutations)[[2]], dim(mri_df)[[1]], dim(behavioral_df)[[1]])) > 0){
    stop('Dimensions of data and/or permutations are not the same.')
  }
  message('Creating cluster with ', NCPU, ' cores...')
  cl <- parallel::makePSOCKcluster(NCPU)
  message('Exporting data to cluster nodes...')
  parallel::clusterExport(cl = cl, 
                          varlist = c('mri_df', 'behavioral_df',
                                      'permutations', 'NPERMS', 
                                      'K', 'NOREG',
                                      lsf.str()))
  message('Running permutations...')
  system.time({
    rez <- parallel::parLapply(
      cl = cl, X = permutations_i, 
      fun = function(i){
        library(PMA)
        perm <- permutations[i,]
        cca_output <- select_and_cca_fit(X = mri_df[perm, 2:ncol(mri_df)],
                                         Y = behavioral_df[,2:ncol(behavioral_df)],
                                         K = K, nperms = NPERMS,
                                         selection_function = selectfun,
                                         noreg = NOREG)
        return(cca_output)
      })
  })
  parallel::stopCluster(cl)
  
  message('Saving results...')
  saveRDS(rez, fname)
  message('Done permuting.')
} else {
  outfile <- file.path('data', paste0('cca-', BEHSUFFIX, '-', SELECTION, REGTYPE, '.rds'))
  if(file.exists(outfile)){
    stop(outfile, ' exists. Will not recompute.')
  }
  message('Estimating CCA on unpermuted data...')
  system.time(
    cca_rez <- select_and_cca_fit(X = mri_df[, 2:ncol(mri_df)],
                                  Y = behavioral_df[, 2:ncol(behavioral_df)],
                                  K = K, nperms = NPERMS,
                                  selection_function = selectfun,
                                  return_cca_object = TRUE,
                                  noreg = NOREG)
  )
  message('Saving result to ', outfile)
  saveRDS(cca_rez, outfile)
  message('Done.')
}