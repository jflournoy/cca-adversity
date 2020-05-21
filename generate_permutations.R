if (!require("readr")) {install.packages("readr"); require("readr")}
if (!require("permute")) {install.packages("permute"); require("permute")}

behavioral_df_all <- data.frame(readr::read_csv('data/behavior_data.csv'))
mri_df_all <- readRDS('data/rsfc_data.rds')
sub_list_DT <- read.csv("subject_lists/Rest_DT-CCA_n118.csv")
sub_list_MT <- read.csv("subject_lists/Rest_MT-CCA_n121.csv")

##---------Select resting state features-------

# Retain participants who have clean scan data from Steph's QC
sub_list <- rbind(sub_list_DT, sub_list_MT)
names(sub_list)[1] <- "ID"

behavioral_df <- merge(behavioral_df_all, sub_list, by = "ID", all = FALSE) # final behavioral dataframe
mri_df <- merge(mri_df_all, behavioral_df[1], by = "ID", all = FALSE)

if(dim(behavioral_df)[[1]] != dim(mri_df)[[1]]){
  stop('X and Y dimensions do not correspond')
}

ctrl <- permute::how(within = Within(type = 'free'), plots = Plots(strata = mri_df$ID, type = 'free'))
perms <- permute::shuffleSet(mri_df$ID, nset = 1e4, ctrl)
saveRDS(perms, 'data/permute_index.rds')
