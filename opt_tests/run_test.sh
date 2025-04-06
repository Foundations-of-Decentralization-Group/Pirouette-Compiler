#! /bin/bash

## Script Parameters:
# ./run-test.sh [-p|-x] <test name> <num runs> <iters per run> [<iters per run> ...]

while getopts px opt
do
    case $opt in
        p)
            perf_flag=-p
            ;;
        x)
            send_recv_mod_flag=-x
            ;;
    esac
done

shift $((OPTIND-1))

test_name=$1
file_dir=tests/$1/$1

## Config:
num_runs=$2
shift 2
list_num_iters="$@"

echo -n "" > $file_dir.ave
pushd .
cd tests/$test_name
for iters in $list_num_iters
do
    ../../build-test.sh $perf_flag $send_recv_mod_flag $test_name $iters
    echo -n "$iters " >> $test_name.ave
    for i in $(seq 1 $num_runs)
    do
        ./$test_name
        if [ -n "$perf_flag" ]
        then
            perf record -o perf.data --call-graph=dwarf -- ./$test_name
            perf report
        fi
    done | awk '{sum+=$2-$1}END{print sum/NR}' >> $test_name.ave
    # https://stackoverflow.com/questions/15681498/scripts-for-computing-the-average-of-a-list-of-numbers-in-a-data-file
done
popd
