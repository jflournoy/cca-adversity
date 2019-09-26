#!/bin/bash
#
################################## 
# USAGE & PURPOSE
##################################
#
#  $ sbatch --array="subject file row indexing" reg_power_drysdale_spheres_final.sh [subjectlist.txt full file path]
#
# EXAMPLE:
#  $ sbatch --array="1-147" reg_power_drysdale_spheres_final.sh /mnt/stressdevlab/new_fear_pipeline/SubjectLists/Rest_TEMP_FINAL_n127.txt

# PUPOSE: This code registers power spheres from MNI152 2mm space to native space, one job per subject. Original spheres were 3dresampled from MNI152 2009c asym nonlinear to the 6th gen standard MNI152 2mm template beforehand.

# BATCH JOB PARAMETERS -----------------------------------------------------------
#SBATCH --job-name=reg-power-drysdale-spheres
#SBATCH --output=/mnt/stressdevlab/new_fear_pipeline/sbatch_output/job_%A_%a.out
#SBATCH --error=/mnt/stressdevlab/new_fear_pipeline/sbatch_output/job_%A_%a.err
#SBATCH --time=10:00:00
#SBATCH --partition=ncf_holy
#SBATCH --mail-type=END,FAIL
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G

root=/mnt/stressdevlab/new_fear_pipeline

arraynum=$SLURM_ARRAY_TASK_ID
subject=$(head $1 -n $arraynum | tail -n 1)


################################## 
# CODE
##################################

# uses GenericLabel; only one output (no further thresholding or binarizing)
bash /mnt/stressdevlab/scripts/Preprocessing/RegisterANTs-MNIToFunc-Label_SPHERES.sh /users/sdecross/StephanieNDeCross/cca-adversity/roi-timecourses/power_drysdale_spheres_mni152_2mm.nii ${root}/${subject}/session1/rest/Rest.nii.gz ${root}/${subject}/session1/rest/ROIMasks/power_drysdale_spheres_final.nii.gz
