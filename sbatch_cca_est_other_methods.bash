#!/bin/bash
#SBATCH --job-name=ccaest
#SBATCH --output=log/%x_%A.out
#SBATCH --time=12:00:00
#SBATCH --partition=ncf
#SBATCH --mail-type=END,FAIL
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G
module load gcc/7.1.0-fasrc01
module load R/3.5.1-fasrc01
cp ~/.R/Makevars.gcc ~/.R/Makevars
export R_LIBS_USER=/ncf/mclaughlin/users/jflournoy/R_3.5.1_GCC:$R_LIBS_USER

srun -c 1 Rscript --verbose --vanilla CCA.R \
  --nopermute \
  --selectfun drysdale3 \
  --mc.cores 1 \
  --noreg
srun -c 1 Rscript --verbose --vanilla CCA.R \
  --nopermute \
  --selectfun nofeatsel \
  --mc.cores 1 \
  --innerperms 25
