library(readr)
library(data.table)
setDTthreads(threads = 1)
getDTthreads()

make_resid <- function(y, controls){
  controls <- as.matrix(as.data.frame(controls))
  m <- lm(y ~ controls)
  r <- resid(m)
  return(r)
}

#BEHFNAME <- 'data/behavior_data.csv'
BEHFNAME <- 'data/cca_psy.csv'

behavioral_df_all <- data.table(data.frame(readr::read_csv(BEHFNAME)))

mri_df_all <- data.table(readRDS('data/rsfc_data.rds'))
mri_df_all[, ID := as.numeric(ID)]

controls <- c('age_s1', 'sex')
demog <- data.table(readr::read_csv('data/full_mt_dt_demogs.csv'))
demog[, ID := subject]
demog[, subject := NULL]
demog_complete_ <- melt(demog[, mget(c('ID', controls))], id.vars = 'ID')
demog_complete_ <- demog_complete_[, complete := all(!is.na(value)), by = ID][complete == TRUE][, complete := NULL]
demog_complete <- dcast(demog_complete_, ID ~ variable, value.var = 'value')

db <- data.table::merge.data.table(behavioral_df_all, demog_complete, by = c('ID'))
dm <- data.table::merge.data.table(mri_df_all, demog_complete, by = c('ID'))

#faster to do it longwise for a big DT
dm_l <- melt(dm, id.vars = c('ID', controls))
system.time(dm_l[, resid := make_resid(y = value, controls = mget(controls)), by = variable])
dm_l[, c('value', 'age_s1', 'sex') := NULL]
dm_out <- dcast(dm_l, ID ~ variable, value.var = 'resid')

#fine to use `set` in a loop of the more brief behavioral DT
bcols <- names(behavioral_df_all)[-1]
for(col in bcols){
  set(db, j = col, value = make_resid(y = db[, get(col)], controls = db[, mget(controls)]))
}

db_out <- db[, mget(names(behavioral_df_all))]

OUTBEHFNAME <- gsub('\\.csv', '_resid.csv', BEHFNAME)

readr::write_csv(db_out, OUTBEHFNAME)
saveRDS(data.frame(dm_out), file.path('data', 'rsfc_data_resid.rds'))
