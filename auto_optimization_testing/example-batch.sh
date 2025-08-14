#!/bin/bash -l
#
#SBATCH --time=00:05:00
#SBATCH --nodes=6
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=1000
#SBATCH --job-name="pirouette-mpi"
#SBATCH --mail-user=asbohosi@buffalo.edu
#SBATCH --mail-type=all
#SBATCH --partition=general-compute
#SBATCH --qos=general-compute
#SBATCH --cluster=ub-hpc
#SBATCH --constraint="CASCADE-LAKE-IB"

######## Test Settings ########

NUM_RUNS=3
NUM_ITERS=1000
PARTICIPANT_AMOUNTS="4 5 6"

###############################

printf "Recording system and test information\n"

info_dir=${PWD}/tests/$(date +%Y-%m-%d__%H-%M-%S)__$SLURM_JOB_NAME
mkdir -p $info_dir

# Save the slurmscript used for this job
cp $0 $info_dir

# Record Test Settings

printf \
"NUM_RUNS: $NUM_RUNS \n\
NUM_ITERS: $NUM_ITERS \n\
PARTICIPANT_AMOUNTS: $PARTICIPANT_AMOUNTS \n"\
> $info_dir/test-settings-info.txt

# Record SLURM settings
printf \
"SLURM_JOB_ID: $SLURM_JOB_ID \n\
SLURM_NODELIST: $SLURM_NODELIST \n\
SLURM_NNODES: $SLURM_NNODES \n\
SLURM_NTASKS: $SLURM_NTASKS \n\
SLURM_NTASKS_PER_NODE: $SLURM_NTASKS_PER_NODE \n\
SLURM_CPUS_PER_TASK: $SLURM_CPUS_PER_TASK \n\
SLURM_MEM_PER_NODE: $SLURM_MEM_PER_NODE \n"\
> $info_dir/slurm-info.txt

# Record the system info of each node
srun --nodes=$SLURM_NNODES bash record_system_info.sh $info_dir/node-info

printf "Loading modules\n"

module load ocaml
module load gcc/11.2.0
module load openmpi/4.1.1

printf "Starting opam; running `dune build`\n"

eval $(opam env)

cd ..
dune build
cd -

printf "Generating test programs:\n"

rm progs/*
./makecode.sh progs $NUM_ITERS $PARTICIPANT_AMOUNTS

printf "Compiling test programs:\n"

for p in progs/*.pir; do
    ../pirproj.sh -w -26 -b mpi -i unix -m collatz.ml $p
done

printf "Running tests:\n"

# https://vuw-research-computing.github.io/raapoi-docs/advanced/OpenMPI_users_guide/
# https://github.com/easybuilders/easybuild-easyconfigs/issues/20233
export OMPI_MCA_btl='^uct,ofi'
export OMPI_MCA_pml='ucx'
export OMPI_MCA_mtl='^ofi'

printf "test_name,prog_type,time\n" > $info_dir/test_data.csv
for ((i=1; i<=$NUM_RUNS; ++i)); do
    printf "On Run: $i\n" > $info_dir/progress.txt
    for p in progs/*.pir; do
        printf "On Program: $p\n" >> $info_dir/progress.txt
        mpiexec -n $(printf $p | grep -oE "[[:digit:]][[:digit:]]?") $(printf $p | cut -f1 -d'.').mpi.exe
        printf "\n"
    done >> $info_dir/test_data.csv
done
rm $info_dir/progress.txt

cp slurm-$SLURM_JOB_ID.out $info_dir/slurm.out
