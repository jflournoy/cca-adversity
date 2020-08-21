#!/bin/bash
#SBATCH --job-name=ccaperm-xia
#SBATCH --output=log/%x_%A_%a.out
#SBATCH --time=2-00:00:00
#SBATCH --partition=ncf
#SBATCH --mail-type=END,FAIL
#SBATCH --nodes=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=80G
module load gcc/7.1.0-fasrc01
module load R/3.5.1-fasrc01
cp ~/.R/Makevars.gcc ~/.R/Makevars
export R_LIBS_USER=/ncf/mclaughlin/users/jflournoy/R_3.5.1_GCC:$R_LIBS_USER

chunkid=$SLURM_ARRAY_TASK_ID
maxchunks=$SLURM_ARRAY_TASK_MAX
cores=$SLURM_CPUS_ON_NODE

srun -c "${cores}" Rscript --verbose --vanilla CCA.R \
  --selectfun nofeatsel \
  --maxchunks "${maxchunks}" \
  --chunkid "${chunkid}" \
  --mc.cores "${cores}" \
  --innerperms 25
