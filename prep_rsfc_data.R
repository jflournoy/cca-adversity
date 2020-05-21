##---------Process resting state data-------

if (!require("data.table")) {install.packages("data.table"); require("data.table")}

# Import datafiles and combine into one dataframe
file_names <- dir("/net/holynfs01/srv/export/mclaughlin/share_root/stressdevlab/cca_adversity/data/imaging/", 
                  pattern = '.*csv',
                  full.names = TRUE, recursive = TRUE)
df <- data.table::rbindlist(lapply(file_names, function(x) data.table::fread(x)[, ID := gsub('.*/(\\d+)_.*', '\\1', x)]))
class(df) #check to make sure it's a data table still.

# Combine row and col variables
df[, row_col := paste(row, col, sep="_")]
df[, c("row", "col") := NULL] # remove original row and col vars

# Transform long to wide
system.time(df_wide <- data.table::dcast(data = df, formula = ID ~ row_col, value.var = 'r'))

# View number of NA's for each variable, sorted
sort(colSums(is.na(df_wide)))

# View number of NA's for each person, sorted
sort(rowSums(is.na(df_wide))) # one person - subject 5071 - is missing 281 resting state values

# Replace remaining missing values with mean connectivity across all ID
#
# first, compute the mean for each connectivity feature by first fisher-z
# transforming the correlation (atanh), computing the mean, and then
# transforming back to correlation.
system.time(df_long <- data.table::melt(df_wide, id.vars = 'ID')) #This populates NAs for the participants missing values for certain features
df_long[, m := tanh(mean(atanh(value), na.rm = T)), by = c('variable')]
df_long[is.na(value), value := m]
df_long[is.na(value)]
system.time(df_wide <- data.table::dcast(data = df_long, formula = ID ~ variable, value.var = 'value'))
sort(rowSums(is.na(df_wide))) # one person - subject 5071 - is missing 281 resting state values

mri_df_all <- data.frame(df_wide)

format(object.size(mri_df_all), units = 'MB')

saveRDS(mri_df_all, 'data/rsfc_data.rds')
