#! /bin/sh

# Script Parameters:
# ./build-test.sh [-p|-x] <test name> <num iters>

while getopts px opt
do
    case $opt in
        p)
            perf_flag=-g
            ;;
        x)
            send_recv_mod_flag=-x
            ;;
    esac
done

shift $((OPTIND-1))

file=$1

# Use C pre-processor to expand macros
cpp -DNUM_ITERS=$2 -xc -P -E $file.pir -o $file-.pir &&
    dune exec pirc -- $file-.pir &&
    rm $file-.pir &&
    rm *.ast &&
    # Replace '___' with '.'
    awk '{gsub("___", ".", $0); print $0}' $file-.ml > $file.ml &&
    if [ -n "$send_recv_mod_flag" ]
    then
        rm $file-.ml
        mv $file.ml $file-.ml
        awk '{
            for (i=1; i<=NF; i++) {
                if(match($i, /Domainslib.Chan.send/)) {
                    start=substr($i,0,RSTART-1);
                    i++;
                    ch=$i;
                    i++;
                    label=$i
                    print start, "Domainslib.Chan.send", ch, label, "Domainslib.Chan.send", ch, label;
                } else if(match($i, /Domainslib.Chan.recv/)) {
                    i++;
                    ch=$i;
                    print "(Domainslib.Chan.recv ", ch, " ; Helper.fib 30; Domainslib.Chan.recv ", ch, ")";
                    # print "Domainslib.Chan.recv ", ch;
                } else {
                    printf "%s ",$i;
                }
            }
            print ""
        }' $file-.ml > $file.ml
    fi &&
    rm $file-.ml &&
    ln -fs ../../helper/helper.ml helper.ml &&
    ocamlfind ocamlopt $perf_flag -package domainslib helper.ml -linkpkg $file.ml -o $file &&
    rm helper.cm* $file.cm* $file.o
    
