if (!require("haven")) {install.packages("haven"); require("haven")}
if (!require("readr")) {install.packages("readr"); require("readr")}

PLOT_DESCRIPTIVES = FALSE
cleaned_beh_data_file <- 'data/behavior_data.csv'

if(file.exists(cleaned_beh_data_file)){
  behavioral_df_all <- data.frame(readr::read_csv(cleaned_beh_data_file))
} else {
  DT_data <- read_sav("/net/holynfs01/srv/export/mclaughlin/share_root/stressdevlab/cca_adversity/data/behavior/DT_CHILD_PARENT_MASTER.sav")
  MT_data <- read_sav("/net/holynfs01/srv/export/mclaughlin/share_root/stressdevlab/cca_adversity/data/behavior/MT_BASELINE_CHILD_PARENT_MASTER.sav")
  age_data <- read_sav("/net/holynfs01/srv/export/mclaughlin/share_root/stressdevlab/cca_adversity/data/behavior/DT_SESSION_AGES.sav")
  
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
  
  
  ### HOME
  
  # Extract all HOME variables
  home_vars <- colnames(DT_data)[grepl('home_\\d', colnames(DT_data))] 
  home_df <- DT_data[home_vars]
  
  # Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
  home_df[home_df == 999] <- NA
  
  
  # Can use cbind to merge the datasets
  DT_data_final <- cbind(ucla_df, vexr_df, jvq_df, cts_fam_df, ctq_df, ceca_df, home_df)
  
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
  
  
  ### HOME
  
  # Extract all HOME variables
  home_vars_MT <- colnames(MT_data)[grepl('home_\\d', colnames(MT_data))] 
  home_df_MT <- MT_data[home_vars_MT]
  
  # Convert "N/A or Prefer not to Answer" values (coded as 999) to NAs
  home_df_MT[home_df_MT == 999] <- NA
  
  ##---------Combining MT and DT adversity measures-------
  
  # Can use cbind to merge the datasets
  MT_data_final <- cbind(ucla_df_MT, vexr_df_MT, jvq_df_MT, cts_fam_df_MT, ctq_df_MT, ceca_df_MT, home_df_MT)
  
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
  
  if(!dir.exists('data')){
    dir.create('data')
  }
  readr::write_csv(behavioral_df_all, cleaned_beh_data_file)
}

##---------Descriptives-------
if(PLOT_DESCRIPTIVES){
  if (!require("ggplot2")) {install.packages("ggplot2"); require("ggplot2")}
  
  # Make long for plotting
  varying <- colnames(behavioral_df_all)[2:ncol(behavioral_df_all)]
  data_final_long <- reshape(data.frame(behavioral_df_all), varying = varying, idvar = "ID", timevar = "variable_name", v.names = "Score", times = varying, direction = "long")
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
  ggsave(filename="hist_plots.pdf", plot=hist_plots, width=15, height=11, units=c("in"))
}
