# CCA Analysis Questionnaire Coding
# Katherine Grisanzio
# 5/15/19


##---------Load packages and import data-------

if (!require("haven")) {install.packages("haven"); require("haven")}
if (!require("psych")) {install.packages("psych"); require("psych")}
if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
if (!require("PMA")) {install.packages("PMA"); require("PMA")}
if (!require("data.table")) {install.packages("data.table"); require("data.table")}
if (!require("candisc")) {install.packages("candisc"); require("candisc")}


DT_data <- read_sav("/Users/katherinegrisanzio/Dropbox/Harvard/CCA/Questionnaire SPSS Data/DT_CHILD_PARENT_MASTER.sav")
MT_data <- read_sav("/Users/katherinegrisanzio/Dropbox/Harvard/CCA/Questionnaire SPSS Data/MT_BASELINE_CHILD_PARENT_MASTER.sav")
age_data <- read_sav("/Users/katherinegrisanzio/Dropbox/Harvard/CCA/Questionnaire SPSS Data/DT_SESSION_AGES.sav")

sub_list_DT <- read.csv("/Users/katherinegrisanzio/Dropbox/CCA-Adversity/SubjectLists/Rest_DT-CCA_n118.csv")
sub_list_MT <- read.csv("/Users/katherinegrisanzio/Dropbox/CCA-Adversity/SubjectLists/Rest_MT-CCA_n120.csv")


##---------DT Data-------


### UCLA PTSD Index 

# Items 1-12 for both child (trauma_cX) and parent (trauma_pX) versions
ucla_vars <- c("trauma_c1", "trauma_c2", "trauma_c3", "trauma_c4", "trauma_c5", "trauma_c6", "trauma_c7", "trauma_c8", "trauma_c9", "trauma_c10", "trauma_c11", "trauma_c12", "trauma_p1", "trauma_p2", "trauma_p3", "trauma_p4", "trauma_p5", "trauma_p6", "trauma_p7", "trauma_p8", "trauma_p9", "trauma_p10", "trauma_p11", "trauma_p12")
ucla_df <- DT_data[ucla_vars]

# Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
ucla_df[ucla_df == 999] <- NA

# Create child/parent composite - if either child or parent said yes, code as yes. If parent or child has NA, it's ignored and the other value is used (if not NA).
ucla_df <- transform(ucla_df, trauma_1 = pmax(trauma_c1, trauma_p1, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_2 = pmax(trauma_c2, trauma_p2, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_3 = pmax(trauma_c3, trauma_p3, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_4 = pmax(trauma_c4, trauma_p4, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_5 = pmax(trauma_c5, trauma_p5, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_6 = pmax(trauma_c6, trauma_p6, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_7 = pmax(trauma_c7, trauma_p7, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_8 = pmax(trauma_c8, trauma_p8, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_9 = pmax(trauma_c9, trauma_p9, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_10 = pmax(trauma_c10, trauma_p10, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_11 = pmax(trauma_c11, trauma_p11, na.rm=TRUE))
ucla_df <- transform(ucla_df, trauma_12 = pmax(trauma_c12, trauma_p12, na.rm=TRUE))

# Reduce dataset to composite variables
ucla_df <- ucla_df[,(ncol(ucla_df)-11):ncol(ucla_df)]

# Add tally of "yes's" from items 1-12
ucla_df$ucla_tally <- rowSums(ucla_df)


### VEX-R

# Extract base questions, using "VEXR-X" variabes that are the highest of parent and child report
vexr_vars <- colnames(DT_data)[grepl('VEXR_\\d', colnames(DT_data))] 
vexr_df <- DT_data[vexr_vars]


### JVQ_R2

# Extract all JVQ items
jvq_vars <- colnames(DT_data)[grepl('jvq_\\d', colnames(DT_data))] 
jvq_df <- DT_data[jvq_vars]

# Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
jvq_df[jvq_df == 999] <- NA


### CTS - Parent Child


## Come back to, can't find CTS - parent child form variables?


### CTS - Family

# Extract all CTS Family items besides items 1, 2, and 3 (positively phrased q's)
cts_fam_vars <- colnames(DT_data)[grepl('cts_\\d', colnames(DT_data))] 
cts_fam_vars <- cts_fam_vars[4:17] # Code this the same way as below
cts_fam_df <- DT_data[cts_fam_vars]

### CTQ

# Extract all CTQ vars except items 10, 16, and 22 (items just for validity of survey)
ctq_vars <- colnames(DT_data)[grepl('ctq_\\d', colnames(DT_data))] 
ctq_df <- DT_data[ctq_vars]
ctq_df <- ctq_df[, -match(c("ctq_10", "ctq_16", "ctq_22"), names(ctq_df))] 

# Reverse code positively phrased questions
keys_ctq <- c(1,-1, 1, 1, -1, 1, -1, 1, 1, 1, 1, -1, 1, 1, 1, 1, -1, 1, 1, 1, 1, 1, -1, 1, -1)
ctq_df <- reverse.code(keys_ctq, ctq_df, mini = 1, maxi = 5)
# Ensure items are in the same order in MT


### CECA-Q

## Items we're retaining:
# par_2a - 'Did you ever live in a childrens home or institution?'
# par_2b - 'IF YES: How long did you live there?'
# par_3ma - '(Mother) Are both of your parents still living?'
# par_3fa - '(Father) Are both of your parents still living?'
# par_4ma - '(Mother) Have you ever been separated from either of your parents for one year or more?'
# par_4fa - '(Father) Have you ever been separated from either of your parents for one year or more?'
# par_4mc - '(Mother) How long did it last?'
# par_4fc - '(Father) How long did it last?'
# mfig_1 through mfig_16
# ffig_1 through ffig_16
# pun - 'When you were a child or a teenager were you ever hit repeatedly with something (like a belt or a stick) or punched, kicked, or burnt by someone in the household?'
# pun_2 - 'Did the hitting happen more than once?'
# pun_2a - 'How many times has this ever happened? '
# pun_2b - 'How many times did it happen this year? '
# pun_4 - 'Were you ever injured (e.g., bruises, black-eyes, broken limbs)? '
# pun_q1 - 'Did anyone else ever do this to you?'
# use_1	- 'Has someone ever touched your private parts when you didnt want them to?'
# use_2	- 'Has someone ever made you touch their private parts when you didnt want to?'
# use_3	- 'Has someone made you do something sexual or watch something sexual that you didnt want to?'
# use_4	- 'Has someone forced you to have sex when you didnt want to?'
# use_5	- 'Have you ever had any other unwanted sexual experiences including seeing things you didnt want to see (either in person or on the internet)?'
# use_yf5	- 'Did this person do it to you more than once?'
# use_yf5a - 'How many times has this ever happened? '
# use_yf5b - 'How many times did it happen this year? '
# use_yf6 - 'Were you ever injured?'
# use_yw - 'Did anyone else ever do this to you? '
# use_yw5 - 'Did this person do it to you more than once?'
# use_yw5a - 'How many times has this ever happened? '
# use_yw5b - 'How many times did it happen this year? '
# use_yw6 - 'Were you ever injured?'
# fvip - 'When you were a child or teenager, did you ever see or hear your parents or caregivers hit each other repeatedly with something (like a belt or stick) or hit, punch, kick, or burn each other?'
# fvip_3 - 'Did the hitting happen more than once? '
# fvip_4 - 'Did you see it happen? '
# fvip_4a - 'IF YES: How many times have you ever seen this? '
# fvip_4b - 'How many times have you seen this in the past year?'
# fvip_6 - 'Were either of your parents/caregivers injured (e.g., bruises, black-eyes, broken limbs)?'
# fvip_7 - 'Were either of your parents/caregivers killed? '
# fvip_11 - 'Did you ever see or hear another adult do any of those things to one of your parents or caregivers?'

ceca_vars <- c("mfig_1", "mfig_2", "mfig_3", "mfig_4", "mfig_5", "mfig_6", "mfig_7", "mfig_8", "mfig_9", "mfig_10", "mfig_11", "mfig_12", "mfig_13", "mfig_14", "mfig_15", "mfig_16",
               "ffig_1", "ffig_2", "ffig_3", "ffig_4", "ffig_5", "ffig_6", "ffig_7", "ffig_8", "ffig_9", "ffig_10", "ffig_11", "ffig_12", "ffig_13", "ffig_14", "ffig_15", "ffig_16",
               "pun", "pun_2", "pun_2a", "pun_2b", "pun_4", "pun_q1", "use_1", "use_2", "use_3", "use_4", "use_5",
               "use_yf5", "use_yf5a", "use_yf5b", "use_yf6", "use_yw", "use_yw5", "use_yw5a", "use_yw5b", "use_yw6",
               "fvip", "fvip_3", "fvip_4", "fvip_4a", "fvip_4b", "fvip_6", "fvip_7", "fvip_11")
# Leave out "par_2a", "par_2b", "par_3ma", "par_3fa", "par_4ma", "par_4fa", "par_4mc", "par_4fc" for now while locating in MT
ceca_df <- DT_data[ceca_vars]

# Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
ceca_df[ceca_df == 999] <- NA

# Reverse code positively phrased mfig and ffig items
keys_ceca <- c(1,-1, -1, 1, -1, 1, 1, -1, 1, 1, -1, -1, -1, -1, 1, 1, 1,-1, -1, 1, -1, 1, 1, -1, 1, 1, -1, -1, -1, -1, 1, 1)
ceca_df[, grepl('mfig|ffig', colnames(ceca_df))] <- reverse.code(keys_ceca, ceca_df[, grepl('mfig|ffig', colnames(ceca_df))], mini = 1, maxi = 5)

# For use_1 through use_5, code "unsure" (99) as "yes" (1). Leave N/A or Prefer Not to Answer (999) as NA.
ceca_df[ceca_df[, grepl('use_\\d', colnames(ceca_df))] %in% 99] <- NA


# Can use cbind to merge the datasets
DT_data_final <- cbind(ucla_df, vexr_df, jvq_df, cts_fam_df, ctq_df, ceca_df)

# Add back in subject ID variable for DT
row.names(DT_data_final) <- DT_data$id



##---------MT Data-------


### UCLA PTSD Index 

# Items 1-12 for both child (trauma_cX) and parent (trauma_pX) versions
ucla_vars <- c("trauma_c1", "trauma_c2", "trauma_c3", "trauma_c4", "trauma_c5", "trauma_c6", "trauma_c7", "trauma_c8", "trauma_c9", "trauma_c10", "trauma_c11", "trauma_c12", "trauma_p1", "trauma_p2", "trauma_p3", "trauma_p4", "trauma_p5", "trauma_p6", "trauma_p7", "trauma_p8", "trauma_p9", "trauma_p10", "trauma_p11", "trauma_p12")
ucla_df_MT <- MT_data[ucla_vars]

# Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
ucla_df_MT[ucla_df_MT == 999] <- NA

# Create child/parent composite - if either child or parent said yes, code as yes. If parent or child has NA, it's ignored and the other value is used (if not NA).
ucla_df_MT <- transform(ucla_df_MT, trauma_1 = pmax(trauma_c1, trauma_p1, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_2 = pmax(trauma_c2, trauma_p2, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_3 = pmax(trauma_c3, trauma_p3, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_4 = pmax(trauma_c4, trauma_p4, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_5 = pmax(trauma_c5, trauma_p5, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_6 = pmax(trauma_c6, trauma_p6, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_7 = pmax(trauma_c7, trauma_p7, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_8 = pmax(trauma_c8, trauma_p8, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_9 = pmax(trauma_c9, trauma_p9, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_10 = pmax(trauma_c10, trauma_p10, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_11 = pmax(trauma_c11, trauma_p11, na.rm=TRUE))
ucla_df_MT <- transform(ucla_df_MT, trauma_12 = pmax(trauma_c12, trauma_p12, na.rm=TRUE))

# Reduce dataset to composite variables
ucla_df_MT <- ucla_df_MT[,(ncol(ucla_df_MT)-11):ncol(ucla_df_MT)]

# Add tally of "yes's" from items 1-12
ucla_df_MT$ucla_tally <- rowSums(ucla_df_MT)


### VEX-R

# Extract base questions, using "VEXR-X" variabes that are the highest of parent and child report
vexr_vars_MT <- colnames(MT_data)[grepl('VEXR_c\\d', colnames(MT_data))] 
vexr_df_MT <- MT_data[vexr_vars_MT]
### These are only child report, but for DT its highest of parent and child. Come back to see if I can locate the highest parent/child variables in the MT dataset.
### TEMPORARILY change VEXR variable names to match DT so that data can be merged
names(vexr_df_MT) <- c("VEXR_1", "VEXR_2", "VEXR_3", "VEXR_4", "VEXR_5", "VEXR_6", "VEXR_7", "VEXR_8", "VEXR_9", "VEXR_10", "VEXR_11", "VEXR_12", "VEXR_13", "VEXR_14", "VEXR_15", "VEXR_16", "VEXR_17", "VEXR_18", "VEXR_19", "VEXR_20", "VEXR_21", "VEXR_22")

### JVQ_R2

# Extract all JVQ items
jvq_vars <- colnames(DT_data)[grepl('jvq_\\d', colnames(DT_data))] 
jvq_df_MT <- MT_data[jvq_vars]

# Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
jvq_df_MT[jvq_df_MT == 999] <- NA


### CTS - Parent Child


## Add only if these variables exist in DT dataset


### CTS - Family

# Extract all CTS Family items besides items 1, 2, and 3 (positively phrased q's)
cts_fam_vars <- colnames(DT_data)[grepl('cts_\\d', colnames(DT_data))] 
cts_fam_vars <- cts_fam_vars[4:17] # Code this the same way as below
cts_fam_df_MT <- MT_data[cts_fam_vars]

### CTQ

# Extract all CTQ vars except items 10, 16, and 22 (items just for validity of survey)
ctq_vars <- colnames(DT_data)[grepl('ctq_\\d', colnames(DT_data))] 
ctq_df_MT <- MT_data[ctq_vars]
ctq_df_MT <- ctq_df_MT[, -match(c("ctq_10", "ctq_16", "ctq_22"), names(ctq_df_MT))] 

# Reverse code positively phrased questions
keys_ctq <- c(1,-1, 1, 1, -1, 1, -1, 1, 1, 1, 1, -1, 1, 1, 1, 1, -1, 1, 1, 1, 1, 1, -1, 1, -1)
ctq_df_MT <- reverse.code(keys_ctq, ctq_df_MT, mini = 1, maxi = 5)
# Ensure items are in the same order in MT


### CECA-Q

## Items we're retaining:
# par_2a - 'Did you ever live in a childrens home or institution?'
# par_2b - 'IF YES: How long did you live there?'
# par_3ma - '(Mother) Are both of your parents still living?'
# par_3fa - '(Father) Are both of your parents still living?'
# par_4ma - '(Mother) Have you ever been separated from either of your parents for one year or more?'
# par_4fa - '(Father) Have you ever been separated from either of your parents for one year or more?'
# par_4mc - '(Mother) How long did it last?'
# par_4fc - '(Father) How long did it last?'
# mfig_1 through mfig_16
# ffig_1 through ffig_16
# pun - 'When you were a child or a teenager were you ever hit repeatedly with something (like a belt or a stick) or punched, kicked, or burnt by someone in the household?'
# pun_2 - 'Did the hitting happen more than once?'
# pun_2a - 'How many times has this ever happened? '
# pun_2b - 'How many times did it happen this year? '
# pun_4 - 'Were you ever injured (e.g., bruises, black-eyes, broken limbs)? '
# pun_q1 - 'Did anyone else ever do this to you?'
# use_1	- 'Has someone ever touched your private parts when you didnt want them to?'
# use_2	- 'Has someone ever made you touch their private parts when you didnt want to?'
# use_3	- 'Has someone made you do something sexual or watch something sexual that you didnt want to?'
# use_4	- 'Has someone forced you to have sex when you didnt want to?'
# use_5	- 'Have you ever had any other unwanted sexual experiences including seeing things you didnt want to see (either in person or on the internet)?'
# use_yf5	- 'Did this person do it to you more than once?'
# use_yf5a - 'How many times has this ever happened? '
# use_yf5b - 'How many times did it happen this year? '
# use_yf6 - 'Were you ever injured?'
# use_yw - 'Did anyone else ever do this to you? '
# use_yw5 - 'Did this person do it to you more than once?'
# use_yw5a - 'How many times has this ever happened? '
# use_yw5b - 'How many times did it happen this year? '
# use_yw6 - 'Were you ever injured?'
# fvip - 'When you were a child or teenager, did you ever see or hear your parents or caregivers hit each other repeatedly with something (like a belt or stick) or hit, punch, kick, or burn each other?'
# fvip_3 - 'Did the hitting happen more than once? '
# fvip_4 - 'Did you see it happen? '
# fvip_4a - 'IF YES: How many times have you ever seen this? '
# fvip_4b - 'How many times have you seen this in the past year?'
# fvip_6 - 'Were either of your parents/caregivers injured (e.g., bruises, black-eyes, broken limbs)?'
# fvip_7 - 'Were either of your parents/caregivers killed? '
# fvip_11 - 'Did you ever see or hear another adult do any of those things to one of your parents or caregivers?'

ceca_vars_MT <- c("mfig_1", "mfig_2", "mfig_3", "mfig_4", "mfig_5", "mfig_6", "mfig_7", "mfig_8", "mfig_9", "mfig_10", "mfig_11", "mfig_12", "mfig_13", "mfig_14", "mfig_15", "mfig_16",
               "ffig_1", "ffig_2", "ffig_3", "ffig_4", "ffig_5", "ffig_6", "ffig_7", "ffig_8", "ffig_9", "ffig_10", "ffig_11", "ffig_12", "ffig_13", "ffig_14", "ffig_15", "ffig_16",
               "pun", "pun_2", "pun_2a", "pun_2b", "pun_4", "pun_q1", "use_1", "use_2", "use_3", "use_4", "use_5",
               "use_yf5", "use_yf5a", "use_yf5b", "use_yf6", "use_yw", "use_yw5", "use_yw5a", "use_yw5b", "use_yw6",
               "fvip", "fvip_3", "fvip_4", "fvip_4a", "fvip_4b", "fvip_6", "fvip_7", "fvip_11")
# Can't find "par_2a", "par_2b", "par_3ma", "par_3fa", "par_4ma", "par_4fa", "par_4mc", "par_4fc" variables in MT. Come back to locate.
ceca_df_MT <- MT_data[ceca_vars_MT]

# Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
ceca_df_MT[ceca_df_MT == 999] <- NA

# Reverse code positively phrased mfig and ffig items
keys_ceca <- c(1,-1, -1, 1, -1, 1, 1, -1, 1, 1, -1, -1, -1, -1, 1, 1, 1,-1, -1, 1, -1, 1, 1, -1, 1, 1, -1, -1, -1, -1, 1, 1)
ceca_df_MT[, grepl('mfig|ffig', colnames(ceca_df_MT))] <- reverse.code(keys_ceca, ceca_df_MT[, grepl('mfig|ffig', colnames(ceca_df_MT))], mini = 1, maxi = 5)
# Double check

# For use_1 through use_5, code "unsure" (99) as "yes" (1). Leave N/A or Prefer Not to Answer (999) as NA.
ceca_df_MT[ceca_df_MT[, grepl('use_\\d', colnames(ceca_df_MT))] %in% 99] <- NA


##---------Combining MT and DT adversity measures-------

# Can use cbind to merge the datasets
MT_data_final <- cbind(ucla_df_MT, vexr_df_MT, jvq_df_MT, cts_fam_df_MT, ctq_df_MT, ceca_df_MT)

# Add back in subject ID variable for MT
row.names(MT_data_final) <- MT_data$ID

# Rbind to merge MT_data_final and DT_data_final datasets
data_final <- rbind(DT_data_final, MT_data_final)

# Export data
# write.csv(data_final, file = "data_final.csv")


##---------Analysis and replacement of missingness-------

# Data untouched as 494 participants and 168 variables

# View number of NA's for each variable, sorted
sort(colSums(is.na(data_final)))

# Remove variables with more than 20% missing data
data_final <- data_final[colSums(is.na(data_final))/nrow(data_final) < .20]
# Now: 494 participants and 147 variables


# View number of NA's for each person, sorted
sort(rowSums(is.na(data_final)))

# Remove participants with more than 20% missing data
data_final <- data_final[rowSums(is.na(data_final))/ncol(data_final) < .20,]
# Now: 464 participants and 147 variables

# Mean replace remaining missing values

for(j in 2:ncol(data_final)) {
  m <- mean(data_final[,j], na.rm=TRUE)
  
  for (i in 1:nrow(data_final)) {
    if(is.na(data_final[i,j])) {
      data_final[i,j] <- m
    }
  }
}

# Get ID back as a column, rather than the row.name, for the reshaping in Descriptives
data_final <- data.frame(ID=rownames(data_final), data_final, row.names=NULL)

behavioral_df_all <- data.frame(data_final) 

##---------Descriptives-------

# Make long for plotting
varying <- colnames(data_final)[2:ncol(data_final)]
data_final_long <- reshape(data_final, varying = varying, idvar = "ID", timevar = "variable_name", v.names = "Score", times = varying, direction = "long")
data_final_long <- data.frame(data_final_long, row.names = NULL)

# Remove 999 and -993 values
data_final_long$Score[data_final_long$Score < -100 | data_final_long$Score > 100] <- NA

# Plot histograms
hist_plots <- ggplot(data_final_long, aes(Score)) +
  geom_histogram() +
  facet_wrap(~variable_name, scales = "free_x") +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))

# ggsave(filename="hist_plots.pdf", path="~/Downloads", plot=hist_plots, width=15, height=11, units=c("in"))


##---------Process resting state data-------

# Import datafiles and combine into one dataframe
file_names <- dir("/Users/katherinegrisanzio/Dropbox/Harvard/CCA/Resting state data/all_corrmat", full.names = TRUE) 
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

# Mean replace remaining missing values

df_wide <- data.frame(df_wide) # the mean replace code below only works if it's a data frame first

#for(j in 2:ncol(df_wide)) {
#  m <- mean(df_wide[,j], na.rm=TRUE)
#  
#  for (i in 1:nrow(df_wide)) {
#    if(is.na(df_wide[i,j])) {
#      df_wide[i,j] <- m
#    }
#  }
#}
# ^ this takes too long to run

NA_to_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df_wide[] <- lapply(df_wide, NA_to_mean)
# this throws a warning but it seems to work - come back to


mri_df_all <- data.frame(df_wide)


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

total_n <- 236

cca_output_drysdale <- select_and_cca_fit(X = mri_df[2:ncol(mri_df)], Y = behavioral_df[2:ncol(behavioral_df)], n_selected_vars = round(.80*total_n), selection_function = select_features_drysdale)
cca_output_xia <- select_and_cca_fit(X = mri_df[2:ncol(mri_df)], Y = behavioral_df[2:ncol(behavioral_df)], n_selected_vars = round(.10*(ncol(mri_df)-1)), selection_function = select_features_xia) # -1 to exclude the ID col; Y isn't used here


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

