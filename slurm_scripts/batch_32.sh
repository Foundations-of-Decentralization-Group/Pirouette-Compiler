#!/bin/bash -lp
#
#SBATCH --time=00:06:00
#SBATCH --nodes=31
#SBATCH --cpus-per-task=1
#SBATCH --mem=6000
#SBATCH --job-name="pirouette-mpi-test"
#SBATCH --output=pirouette-mpi-test.out
#SBATCH --mail-user=nitinvin@buffalo.edu
#SBATCH --mail-type=all
#SBATCH --partition=general-compute
#SBATCH --qos=general-compute
#SBATCH --cluster=ub-hpc
#SBATCH --constraint="[CASCADE-LAKE-IB]"
printf "Recording system information\n"

info_dir=${PWD}/job_info/$(date +%Y-%m-%d__%H-%M-%S)
mkdir -p $info_dir
mkdir -p $info_dir/unopt_data
mkdir -p $info_dir/opt_data

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

#eval $(opam env)

#dune build

printf "Compiling and executing Pirouette code\n"
#for i in {1..2}
#do
#         mpiexec /user/nitinvin/Downloads/Pirouette-Compiler/optimization_tests/31_Participants/31_Participants/Test/p31_fan_out_fan_in_collatz.exe > $info_dir/opt_data/output_opt.txt
         mpiexec /user/nitinvin/Downloads/Pirouette-Compiler/optimization_tests/31_Participants/31_Participants/Test/p31_seq_out_seq_in.exe > $info_dir/unopt_data/output_unopt.txt

#         done
#./pirproj.sh -b mpi -p Buyer,Seller -r examples/bookseller.pir
