################################################################.
# DESCRIPTION & INFO -------------------------------------------
################################################################.

########## CODE DESCRIPTION
# loads, merges, and formats demographic data
# saves RData and csv output of demogs
# data wrangle full dataset starts line 50
# data wrangle cca subset starts line 136
# analysis of cca subset demogs starts line 217

########## STUDY INFO

# for both dt and mt, 
# session1 = behav measures
# session2 = behav tasks
# session3 = scan

########## VARIABLE INFO

# race:
# 1 = white
# 2 = black
# 3 = latino
# 4 = asian
# 5 = middle eastern, biracial, native, other

# "white" aka indicator of minority status:
# 1 = yes (yes, subj is white)
# 0 = no (no, subj is non-white)

# age:
# if residualizing based on full sample, use s1
# for analyses within scanned sample, use s3
# people dropped out/weren't eligible between s1 and s3
# so there's missing data, but all scanned subjs have s3 age

# income-to-needs:
# not manipulated in full demogs (n=494)
# for cca subset, in order to retain all subjs in any covarying 
# etc., inc_needs imputed for the handful of missing subjs
# dt n=1 missing inc_needs value is imputed based on dt sample 
# mean inc_needs mt imputed values are means of sample inc_needs 
# by group (control/trauma); for n=2 controls, mean inc_needs 
# of mt controls is imputed; for n=6 trauma, mean inc_needs of 
# mt trauma is imputed


################################################################.
# LOAD & WRANGLE FULL DATA - MT & DT COMBINED ------------------
################################################################.

rm(list=ls())

library(pacman)
p_load("tidyverse", "psych", "haven")

dt_age_orig <- read_sav("Data/Behavioral/DT_SESSION_AGES.sav")
dt_age <- dt_age_orig %>%
  select(subject = ID, age_s1 = S1ExactAge, 
         age_s2 = S2ExactAge, age_s3 = S3ExactAge)
# session1 = behav measures; s2 = behav task; s3 = scan
write.csv(dt_age, file = "Data/Behavioral/dt_age.csv")
rm(dt_age_orig)
rm(dt_age)

dt_age_orig <- read.csv("Data/Behavioral/dt_age.csv")
dt_data_orig <- read.csv("Data/Behavioral/DT_data.csv")
mt_data_orig <- read.csv("Data/Behavioral/MT_data.csv")

dt_age <- dt_age_orig %>% 
  dplyr::select(-X)

#tidyselect::vars_select(names(dt_data_orig), starts_with("INC"))

dt_data <- dt_data_orig %>%
  dplyr::select(subject = id, 
                sex = SEX,
                race = RACE,
                inc_needs = INC_NEEDS)

mt_data <- mt_data_orig %>%
  dplyr::select(subject = ID,
                sex = SEX,
                race = RACE,
                inc_needs = INC_NEEDS,
                trauma = ANY_ABUSE_DV,
                age_s1 = S1AGE, 
                age_s2 = S2AGE, 
                age_s3 = S3AGE)

dt_demogs <- dt_data %>%
  left_join(dt_age, by = "subject") %>%
  mutate(trauma = NA, study = "dt")

mt_demogs <- mt_data %>%
  mutate(study = "mt")

# compute variable "white" 
dt_demogs <- dt_demogs %>%
  mutate(white = if_else(race == 1, 1, 0)) %>%
  dplyr::select(subject, sex, race, white, inc_needs,
                age_s1, age_s2, age_s3, trauma, study) 

mt_demogs <- mt_demogs %>%
  mutate(white = if_else(race == 1, 1, 0)) %>%
  dplyr::select(subject, sex, race, white, inc_needs,
                age_s1, age_s2, age_s3, trauma, study)
             
# combine
full_mt_dt_demogs <- rbind(mt_demogs, dt_demogs)

# factor vars
full_mt_dt_demogs$subject <- factor(full_mt_dt_demogs$subject)

# male = 0; female = 1 (male is reference)
full_mt_dt_demogs$sex <- factor(full_mt_dt_demogs$sex) 

# white = 1; black = 2; latino = 3; 
# asian/pacific islander = 4; biracial/other = 5
# (white is reference)
full_mt_dt_demogs$race <- factor(full_mt_dt_demogs$race, levels = c(1, 2, 3, 4, 5)) 

# white = 1; nonwhite = 0 (white is reference)
full_mt_dt_demogs$white <- factor(full_mt_dt_demogs$white, levels = c(1, 0)) 

# control = 0; trauma = 1 (control is reference)
full_mt_dt_demogs$trauma <- factor(full_mt_dt_demogs$trauma) 

# save
save(full_mt_dt_demogs, file = "Data/Behavioral/demogs/full_mt_dt_demogs_n494.RData")
write.csv(full_mt_dt_demogs, file = "Data/Behavioral/demogs/full_mt_dt_demogs_n494.csv")


################################################################.
# LOAD & WRANGLE CCA SUBSET ------------------------------------
################################################################.

rm(list=ls())
load("Data/Behavioral/demogs/full_mt_dt_demogs_n494.RData")

dt_subjs <- read.csv("SubjectLists/Rest_DT-CCA_n118.csv")
mt_subjs <- read.csv("SubjectLists/Rest_MT-CCA_n121.csv")

all_dt_demogs <- full_mt_dt_demogs %>%
  filter(study == "dt")

all_mt_demogs <- full_mt_dt_demogs %>%
  filter(study == "mt")

# get info for only subjs relevant to cca-adversity
all_dt_demogs$subject <- as.character(all_dt_demogs$subject)
dt_subjs$subject <- as.character(dt_subjs$subject)
cca_dt_demogs <- dt_subjs %>%
  left_join(all_dt_demogs, by = "subject") 

all_mt_demogs$subject <- as.character(all_mt_demogs$subject)
mt_subjs$subject <- as.character(mt_subjs$subject)
cca_mt_demogs <- mt_subjs %>%
  left_join(all_mt_demogs, by = "subject")

# impute inc_needs

# get mean inc_needs by group
mean_dt <- mean(cca_dt_demogs$inc_needs, na.rm = T)

mt_means <- cca_mt_demogs %>% 
  group_by(trauma) %>% 
  summarise(mean_inc = mean(inc_needs, na.rm = T))
#mean_mt_ctl <- mt_means[1, 2] # keeps it as a tibble; need pull
mean_mt_ctl <- pull(mt_means[1, 2])
mean_mt_tr <- pull(mt_means[2, 2])

# replace dt NAs
cca_dt_demogs <- cca_dt_demogs %>% mutate(inc_needs = replace_na(inc_needs, mean_dt))
# ways that did not work or are less readable are below
# cca_dt_demogs[7, "inc_needs"] <- mean_dt
# cca_dt_demogs <- cca_dt_demogs %>% replace_na(list=(inc_needs = mean_dt)) # no, makes col a list

# replace mt NAs
# this is preferable code that is more specific (didn't work before when vars
# were tibbles and not vectors but now works)
cca_mt_demogs <- cca_mt_demogs %>%     
  mutate(inc_needs =
           if_else(trauma == "0", replace_na(inc_needs, mean_mt_ctl), 
                   if_else(trauma == "1", replace_na(inc_needs, mean_mt_tr), NA_real_)))
#less preferable ways below
# 1) less specific if else leaves rooom for errors
# cca_mt_demogs <- cca_mt_demogs %>%
#  mutate(inc_needs =
#           if_else(trauma == "0", replace_na(inc_needs, mean_mt_ctl), replace_na(inc_needs, mean_mt_tr)))
# 2) inelegant
# cca_mt_demogs_ctl <- cca_mt_demogs %>%
#  filter(trauma == "0")
# cca_mt_demogs_tr <- cca_mt_demogs%>%
#  filter(trauma == "1")
# cca_mt_demogs_ctl <- cca_mt_demogs_ctl %>% replace_na(list(inc_needs = mean_mt_ctl))
# cca_mt_demogs_tr <- cca_mt_demogs_tr %>% replace_na(list(inc_needs = mean_mt_tr))
# cca_mt_demogs <- rbind(cca_mt_demogs_ctl, cca_mt_demogs_tr)
# cca_mt_demogs <- cca_mt_demogs %>%
#  arrange(subject)

# now have dataframes:
# cca_mt_demogs
# cca_dt_demogs

cca_demogs <- rbind(cca_mt_demogs, cca_dt_demogs)

# factor vars
cca_demogs$subject <- factor(cca_demogs$subject)

save(cca_demogs, file = "Data/Behavioral/demogs/cca_demogs_n239.RData")
write.csv(cca_demogs, file = "Data/Behavioral/demogs/cca_demogs_n239.csv")


################################################################.
# ANALYSIS CCA SUBSET ------------------------------------------
################################################################.

rm(list=ls())
load("Data/Behavioral/demogs/cca_demogs_n239.RData")

summary(cca_demogs)
psych::describe(cca_demogs)

# 126 female out of 239 = 52.7% female
# mean age_s3 = 12.3; SD = 2.1




