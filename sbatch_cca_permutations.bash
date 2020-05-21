#!/bin/bash
#SBATCH --job-name=ccaperm
#SBATCH --output=%x_%A_%a.out
#SBATCH --error=%x_%A_%a.err
#SBATCH --time=2-00:00:00
#SBATCH --partition=ncf
#SBATCH --mail-type=END,FAIL
#SBATCH --nodes=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=40G
module load gcc/7.1.0-fasrc01
module load R/3.5.1-fasrc01

cp ~/.R/Makevars.gcc ~/.R/Makevars

export R_LIBS_USER=/ncf/mclaughlin/users/jflournoy/R_3.5.1_GCC:$R_LIBS_USER

srun -c 48 Rscript --verbose --vanilla CCA.R