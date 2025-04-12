#! /bin/bash

while getopts pxh opt
do
    case $opt in
        p)
            perf_flag=-p
            ;;
        x)
            send_recv_mod_flag=-x
            ;;
        h)
            echo "Script Parameters:"
            echo "  ./run-test.sh [-p|-x|-h] <result name> <test name> <num runs> <iters per run> [<iters per run> ...]"
            echo ""
            echo "Flags:"
            echo "  -p : run perf while running the test(s) (WARNING: produces large files even when run for short time)"
            echo "  -x : splice in extra computations in between sends and receives"
            echo "  -h : display this help message"
            exit
            ;;
    esac
done

shift $((OPTIND-1))

res_name=$1
test_name=$2
res_dir=results/${test_name}__$res_name
file_dir=tests/$2/$2

num_runs=$3
shift 3
list_num_iters="$@"

if [ -d $res_dir ]
then
    echo -n $res_dir "already exists! OVERWRITE? [y/n]: "
    read overwrite
    if [ "$overwrite" = "y" ]
    then
        rm res_dir/*
        rmdir res_dir
    else
        exit
    fi
fi
mkdir -p $res_dir

mkdir -p tests
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
    done > times.txt
    awk '{sum+=$2-$1}END{print sum/NR}' $test_name.res >> averages.txt
    # https://stackoverflow.com/questions/15681498/scripts-for-computing-the-average-of-a-list-of-numbers-in-a-data-file
done
popd
mv tests/$test_name/times.txt tests/$test_name/averages.txt $res_dir
if [ -n "$perf_flag" ]
then
    mv tests/$test_name/perf.data $res_dir
fi
